import { cb } from '../../lib/cerebras.js';
import graph from '../../data/graph.json';

function buildNeighborhood(focusId) {
  const nodes = graph.nodes || [];
  const edges = graph.edges || [];
  const byId = new Map(nodes.map(n => [n.id, n]));
  if (!byId.has(focusId)) return { nodes: [], edges: [] };
  const prereqs = nodes.filter(n => byId.get(focusId)?.prerequisites?.includes(n.id));
  const dependents = nodes.filter(n => n.prerequisites?.includes(focusId));
  const included = new Set([focusId, ...prereqs.map(n => n.id), ...dependents.map(n => n.id)]);
  const subNodes = Array.from(included).map(id => byId.get(id)).filter(Boolean);
  const subEdges = edges.filter(e => included.has(e.source) && included.has(e.target));
  return { nodes: subNodes, edges: subEdges };
}

function sanitizeId(s) {
  return String(s || '').replace(/[^a-zA-Z0-9_\-:.]/g, '').slice(0, 120);
}

export const handler = async (event) => {
  try {
    const { nodeId, variant = 'default' } = JSON.parse(event.body || '{}');
    const id = sanitizeId(nodeId);
    if (!id) {
      return { statusCode: 400, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Missing nodeId' }) };
    }

    const nodes = graph.nodes || [];
    const focus = nodes.find(n => n.id === id);
    if (!focus) {
      return { statusCode: 404, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Node not found' }) };
    }

    const subgraph = buildNeighborhood(id);
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
    const user = `Create a compact quiz for the focus node.
Focus node: ${JSON.stringify(nodeDetails)}
Neighborhood: ${JSON.stringify(subgraph).slice(0, 10000)}

Rules:
- 3 to 5 items total.
- Include 2-4 multiple choice items and 1-2 short answer items.
- For multiple choice, provide exactly 4 options and one correct answer.
- Keep questions concise and unambiguous, aligned with the node's objectives.
- Explanations must justify why the correct answer is right.
- sources should reference focus.resources where available (label or localRef); otherwise keep empty.
- Do not include markdown or code fences in fields.

Return JSON only.`;

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
                type: 'array',
                minItems: 3,
                maxItems: 5,
                items: {
                  type: 'object', additionalProperties: false,
                  required: ['id','nodeId','type','question','answer','explanation','sources'],
                  properties: {
                    id: { type:'string' },
                    nodeId: { type:'string' },
                    type: { enum:['mcq','short'] },
                    question: { type:'string' },
                    options: { type:'array', items:{ type:'string' }, minItems: 0, maxItems: 4 },
                    answer: { type:'string' },
                    explanation: { type:'string' },
                    sources: { type:'array', items:{ type:'string' }, maxItems: 4 }
                  }
                }
              }
            }
          }
        }
      },
      messages: [
        { role: 'system', content: sys },
        { role: 'user', content: user }
      ],
      temperature: 0.2,
      max_completion_tokens: 500
    });

    let payload;
    try {
      payload = JSON.parse(out?.choices?.[0]?.message?.content || '{}');
    } catch {
      payload = { items: [] };
    }

    // Normalize and sanitize
    const items = Array.isArray(payload.items) ? payload.items : [];
    const safeItems = items.map((it, idx) => {
      const typ = it?.type === 'mcq' ? 'mcq' : (it?.type === 'short' ? 'short' : (idx % 3 === 0 ? 'short' : 'mcq'));
      const opts = Array.isArray(it?.options) ? it.options.slice(0, 4).map(s => String(s || '').slice(0, 300)) : [];
      return {
        id: String(it?.id || `${id}::q${idx + 1}`).slice(0, 160),
        nodeId: id,
        type: typ,
        question: String(it?.question || '').slice(0, 500),
        options: typ === 'mcq' ? (opts.length === 4 ? opts : opts.concat(Array(Math.max(0, 4 - opts.length)).fill(''))).slice(0,4) : [],
        answer: String(it?.answer || '').slice(0, 300),
        explanation: String(it?.explanation || '').slice(0, 600),
        sources: Array.isArray(it?.sources) ? it.sources.slice(0, 4).map(s => String(s || '').slice(0, 200)) : []
      };
    }).filter(q => q.question);

    // If model failed, create a tiny heuristic quiz
    const finalItems = safeItems.length ? safeItems : [
      {
        id: `${id}::q1`, nodeId: id, type: 'mcq',
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

    const body = { nodeId: id, variant, items: finalItems.slice(0, 5) };
    return { statusCode: 200, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) };
  } catch (err) {
    return { statusCode: 500, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Server error', details: String(err) }) };
  }
};


