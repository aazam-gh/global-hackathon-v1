// server.js
import 'dotenv/config';
import express from 'express';
import cors from 'cors';
import fs from 'fs';
import { cb } from './lib/cerebras.js';

const app = express();
app.use(cors());
app.use(express.json({ limit: '2mb' }));
app.use(express.static('public'));

app.get('/graph', (req, res) => {
  const g = JSON.parse(fs.readFileSync('./data/graph.json', 'utf8'));
  res.json(g);
});

// LLM helper: “what should I learn next given I know X?”
app.post('/ask', async (req, res) => {
  const { question, knownIds = [] } = req.body || {};
  const graph = JSON.parse(fs.readFileSync('./data/graph.json', 'utf8'));
  const context = JSON.stringify({
    nodes: graph.nodes.filter(n => knownIds.includes(n.id) || n.prerequisites.some(p => knownIds.includes(p))),
    edges: graph.edges.filter(e => knownIds.includes(e.source) || knownIds.includes(e.target))
  }).slice(0, 12000); // keep context manageable

  const out = await cb.chat.completions.create({
    model: 'llama-4-scout-17b-16e-instruct',
    messages: [
      { role: 'system', content: "You are a tutor that uses a knowledge graph to plan next steps." },
      { role: 'user', content:
        `Question: ${question}\nKnown completed nodes: ${knownIds.join(', ') || '(none)'}\n` +
        `Graph context (subset): ${context}\n` +
        `Return a brief plan with 3-5 next nodes and why each matters.`
      }
    ],
    temperature: 0.3,
    max_completion_tokens: 500
  });

  res.json({ answer: out.choices[0].message.content });
});

// Local quiz route to mirror Netlify function for dev
app.post('/quiz', async (req, res) => {
  try {
    const { nodeId } = req.body || {};
    if (!nodeId || typeof nodeId !== 'string') return res.status(400).json({ error: 'Missing nodeId' });
    const graph = JSON.parse(fs.readFileSync('./data/graph.json', 'utf8'));
    const nodes = graph.nodes || [];
    const edges = graph.edges || [];
    const focus = nodes.find(n => n.id === nodeId);
    if (!focus) return res.status(404).json({ error: 'Node not found' });

    const byId = new Map(nodes.map(n => [n.id, n]));
    const prereqs = nodes.filter(n => byId.get(nodeId)?.prerequisites?.includes(n.id));
    const dependents = nodes.filter(n => n.prerequisites?.includes(nodeId));
    const included = new Set([nodeId, ...prereqs.map(n => n.id), ...dependents.map(n => n.id)]);
    const subNodes = Array.from(included).map(id => byId.get(id)).filter(Boolean);
    const subEdges = edges.filter(e => included.has(e.source) && included.has(e.target));
    const subgraph = { nodes: subNodes, edges: subEdges };

    const nodeDetails = {
      id: focus.id,
      title: focus.title,
      kind: focus.kind,
      summary: focus.summary,
      objectives: Array.isArray(focus.objectives) ? focus.objectives.slice(0, 6) : [],
      keywords: Array.isArray(focus.keywords) ? focus.keywords.slice(0, 10) : [],
      resources: Array.isArray(focus.resources) ? focus.resources.slice(0, 4) : []
    };

    const sys = 'You are a tutor that writes small, high-quality quizzes grounded strictly in the given node context and its immediate neighborhood.';
    const user = `Create a compact quiz for the focus node.\nFocus node: ${JSON.stringify(nodeDetails)}\nNeighborhood: ${JSON.stringify(subgraph).slice(0, 10000)}\n\nRules:\n- 3 to 5 items total.\n- Include 2-4 multiple choice items and 1-2 short answer items.\n- For multiple choice, provide exactly 4 options and one correct answer.\n- Keep questions concise and aligned with the node objectives.\n- Explanations must justify why the correct answer is right.\n- sources should reference focus.resources where available (label or localRef); otherwise keep empty.\n- Do not include markdown or code fences in fields.\n\nReturn JSON only.`;

    const out = await cb.chat.completions.create({
      model: 'llama-4-scout-17b-16e-instruct',
      response_format: {
        type: 'json_schema',
        json_schema: {
          name: 'quiz_payload',
          strict: true,
          schema: {
            type: 'object',
            additionalProperties: false,
            required: ['items'],
            properties: {
              items: {
                type: 'array', minItems: 3, maxItems: 5,
                items: { type: 'object', additionalProperties: false,
                  required: ['id','nodeId','type','question','answer','explanation','sources'],
                  properties: {
                    id: { type:'string' }, nodeId: { type:'string' }, type: { enum:['mcq','short'] },
                    question: { type:'string' }, options: { type:'array', items:{ type:'string' }, minItems:0, maxItems:4 },
                    answer: { type:'string' }, explanation: { type:'string' }, sources: { type:'array', items:{ type:'string' }, maxItems:4 }
                  }
                }
              }
            }
          }
        }
      },
      messages: [ { role: 'system', content: sys }, { role: 'user', content: user } ],
      temperature: 0.2,
      max_completion_tokens: 500
    });

    let payload;
    try { payload = JSON.parse(out?.choices?.[0]?.message?.content || '{}'); } catch { payload = { items: [] }; }
    const items = Array.isArray(payload.items) ? payload.items : [];
    const safeItems = items.map((it, idx) => {
      const typ = it?.type === 'mcq' ? 'mcq' : (it?.type === 'short' ? 'short' : (idx % 3 === 0 ? 'short' : 'mcq'));
      const opts = Array.isArray(it?.options) ? it.options.slice(0, 4).map(s => String(s || '').slice(0, 300)) : [];
      return {
        id: String(it?.id || `${nodeId}::q${idx + 1}`).slice(0, 160),
        nodeId,
        type: typ,
        question: String(it?.question || '').slice(0, 500),
        options: typ === 'mcq' ? (opts.length === 4 ? opts : opts.concat(Array(Math.max(0, 4 - opts.length)).fill(''))).slice(0,4) : [],
        answer: String(it?.answer || '').slice(0, 300),
        explanation: String(it?.explanation || '').slice(0, 600),
        sources: Array.isArray(it?.sources) ? it.sources.slice(0, 4).map(s => String(s || '').slice(0, 200)) : []
      };
    }).filter(q => q.question);

    const finalItems = safeItems.length ? safeItems : [
      {
        id: `${nodeId}::q1`, nodeId, type: 'mcq',
        question: `Which best describes ${focus.title}?`,
        options: [
          focus.summary || 'A concept in the course',
          'An unrelated topic',
          'A historical anecdote',
          'A UI component'
        ],
        answer: focus.summary || 'A concept in the course',
        explanation: 'This aligns with the node summary and objectives.',
        sources: (focus.resources || []).map(r => r?.label || r?.localRef).filter(Boolean).slice(0, 2)
      }
    ];
    res.json({ nodeId, items: finalItems.slice(0, 5) });
  } catch (e) {
    res.status(500).json({ error: 'Server error', details: String(e) });
  }
});

app.listen(3000, () => console.log('http://localhost:3000'));
