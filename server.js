// server.js
import 'dotenv/config';
import express from 'express';
import cors from 'cors';
import fs from 'fs';
import { cb, streamChat } from './lib/cerebras.js';

const app = express();
app.use(cors());
app.use(express.json({ limit: '2mb' }));
app.use(express.static('public'));
// Serve OCW content locally under /ocw for development
app.use('/ocw', express.static('ocw_course'));

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

// Streaming suggestions via SSE-like chunked response
app.post('/ask/stream', async (req, res) => {
  res.setHeader('Content-Type', 'text/event-stream');
  res.setHeader('Cache-Control', 'no-cache, no-transform');
  res.setHeader('Connection', 'keep-alive');
  if (res.flushHeaders) res.flushHeaders();

  const send = (event, data) => {
    res.write(`event: ${event}\n`);
    res.write(`data: ${typeof data === 'string' ? data : JSON.stringify(data)}\n\n`);
  };

  try {
    const { question, knownIds = [] } = req.body || {};
    const graph = JSON.parse(fs.readFileSync('./data/graph.json', 'utf8'));
    const context = JSON.stringify({
      nodes: graph.nodes.filter(n => knownIds.includes(n.id) || n.prerequisites.some(p => knownIds.includes(p))),
      edges: graph.edges.filter(e => knownIds.includes(e.source) || knownIds.includes(e.target))
    }).slice(0, 12000);

    const params = {
      model: 'llama-4-scout-17b-16e-instruct',
      messages: [
        { role: 'system', content: 'You are a tutor that uses a knowledge graph to plan next steps.' },
        { role: 'user', content: `Question: ${question}\nKnown completed nodes: ${knownIds.join(', ') || '(none)'}\nGraph context (subset): ${context}\nReturn a brief plan with 3-5 next nodes and why each matters.` }
      ],
      temperature: 0.3,
      max_completion_tokens: 500
    };

    let full = '';
    await streamChat(params, (delta) => {
      full += delta;
      send('token', delta);
    });
    send('done', { answer: full });
  } catch (e) {
    send('error', { message: String(e) });
  } finally {
    res.end();
  }
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

    const sys = 'You are a tutor that writes high-quality MULTIPLE-CHOICE quizzes grounded strictly in the given node context and its immediate neighborhood.';
    const user = `Create a compact quiz for the focus node.\nFocus node: ${JSON.stringify(nodeDetails)}\nNeighborhood: ${JSON.stringify(subgraph).slice(0, 10000)}\n\nRules:\n- 3 to 5 items total.\n- ALL QUESTIONS MUST BE MULTIPLE CHOICE (no short answers).\n- Provide EXACTLY 4 options and one correct answer.\n- Keep questions concise and aligned with the node objectives.\n- Explanations must justify why the correct answer is right.\n- sources should reference focus.resources where available (label or localRef); otherwise keep empty.\n- Do not include markdown or code fences in fields.\n\nReturn JSON only.`;

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
                  required: ['id','nodeId','type','question','options','answer','explanation','sources'],
                  properties: {
                    id: { type:'string' }, nodeId: { type:'string' }, type: { enum:['mcq'] },
                    question: { type:'string' }, options: { type:'array', items:{ type:'string' }, minItems:4, maxItems:4 },
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
      let opts = Array.isArray(it?.options) ? it.options.slice(0, 4).map(s => String(s || '').slice(0, 300)) : [];
      let answer = String(it?.answer || '').slice(0, 300);
      while (opts.length < 4) opts.push('');
      opts = opts.slice(0, 4);
      if (answer && !opts.includes(answer)) {
        opts[0] = answer;
      }
      return {
        id: String(it?.id || `${nodeId}::q${idx + 1}`).slice(0, 160),
        nodeId,
        type: 'mcq',
        question: String(it?.question || '').slice(0, 500),
        options: opts,
        answer,
        explanation: String(it?.explanation || '').slice(0, 600),
        sources: Array.isArray(it?.sources) ? it.sources.slice(0, 4).map(s => String(s || '').slice(0, 200)) : []
      };
    }).filter(q => q.question && q.options.length === 4);

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

// Streaming chat about a node via SSE-like chunked response
app.post('/askNode/stream', async (req, res) => {
  res.setHeader('Content-Type', 'text/event-stream');
  res.setHeader('Cache-Control', 'no-cache, no-transform');
  res.setHeader('Connection', 'keep-alive');
  if (res.flushHeaders) res.flushHeaders();

  const send = (event, data) => {
    res.write(`event: ${event}\n`);
    res.write(`data: ${typeof data === 'string' ? data : JSON.stringify(data)}\n\n`);
  };

  try {
    const { focusId, question, knownIds = [], history = [] } = req.body || {};
    const graph = JSON.parse(fs.readFileSync('./data/graph.json', 'utf8'));
    const nodes = graph.nodes || [];
    const edges = graph.edges || [];
    const byId = new Map(nodes.map(n => [n.id, n]));
    if (!byId.has(focusId)) throw new Error('Invalid focusId');

    const prereqs = nodes.filter(n => byId.get(focusId)?.prerequisites?.includes(n.id));
    const dependents = nodes.filter(n => n.prerequisites?.includes(focusId));
    const included = new Set([focusId, ...prereqs.map(n => n.id), ...dependents.map(n => n.id)]);
    const subNodes = Array.from(included).map(id => byId.get(id)).filter(Boolean);
    const subEdges = edges.filter(e => included.has(e.source) && included.has(e.target));
    const subgraph = { nodes: subNodes, edges: subEdges };

    const focusNode = byId.get(focusId) || {};
    const exampleIntent = /\b(example|sample|exercise|practice|question\s*example)\b/i.test(String(question || ''));
    const msgs = [];
    msgs.push({ role: 'system', content: 'You are a helpful tutor grounded in a knowledge graph and a specific node context. Never propose a study plan or list multiple next nodes. Keep answers extremely concise (≤ 100 words). If the user asks for an example, give exactly one short, relevant example. Do not include references or citations. Answer only about the focus node and its immediate context.' });
    if (Array.isArray(history) && history.length) {
      history.slice(-6).forEach(m => {
        if (m && (m.role === 'user' || m.role === 'assistant') && typeof m.content === 'string') {
          msgs.push({ role: m.role, content: m.content.slice(0, 4000) });
        }
      });
    }
    const nodeDetails = {
      id: focusNode.id,
      title: focusNode.title,
      summary: focusNode.summary,
      objectives: Array.isArray(focusNode.objectives) ? focusNode.objectives.slice(0, 6) : [],
      keywords: Array.isArray(focusNode.keywords) ? focusNode.keywords.slice(0, 10) : []
    };
    const exampleDirective = exampleIntent
      ? 'If the request implies an example, provide exactly ONE concrete example for this node, starting with "Example:" and no bullets. Keep it ≤ 60 words. Set suggestedNext to an empty array.'
      : 'If not explicitly asked for an example, answer briefly and optionally suggest 1–2 next nodes.';
    msgs.push({ role: 'user', content: `Focus node id: ${focusId}\nFocus node details: ${JSON.stringify(nodeDetails)}\nKnown completed nodes: ${(Array.isArray(knownIds)?knownIds:[]).join(', ') || '(none)'}\nContext (focus + immediate prereqs/dependents): ${JSON.stringify(subgraph).slice(0, 12000)}\nUser question: ${String(question || '(briefly explain this topic)')}\n${exampleDirective}\nReturn JSON with fields: answer (plain text), suggestedNext (array of {id, reason}). Do not propose a plan.` });

    const params = {
      model: 'llama-4-scout-17b-16e-instruct',
      messages: msgs,
      temperature: 0.2,
      max_completion_tokens: 300
    };

    let text = '';
    await streamChat(params, (delta) => {
      text += delta;
      send('token', delta);
    });

    let payload;
    try {
      payload = JSON.parse(text);
    } catch {
      payload = { answer: text, suggestedNext: [] };
    }
    const validIds = new Set((graph.nodes || []).map(n => n.id));
    let normalizedNext = Array.isArray(payload.suggestedNext) ? payload.suggestedNext
      .filter(x => x && typeof x.id === 'string' && typeof x.reason === 'string' && validIds.has(x.id))
      .slice(0, 6)
      : [];
    if (exampleIntent) normalizedNext = [];
    const words = String(payload.answer || '').trim().split(/\s+/);
    let answer = words.length > 100 ? words.slice(0, 100).join(' ') : String(payload.answer || '');
    send('done', { answer, suggestedNext: normalizedNext });
  } catch (e) {
    send('error', { message: String(e) });
  } finally {
    res.end();
  }
});

app.listen(3000, () => console.log('http://localhost:3000'));
