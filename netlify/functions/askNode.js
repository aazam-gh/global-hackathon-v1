import { cb } from '../../lib/cerebras.js';
import graph from '../../data/graph.json';

function sanitizeQuestion(q) {
  return String(q || '').replace(/[\u0000-\u001F\u007F<>]/g, '').slice(0, 2000);
}

function buildNeighborhood(focusId) {
  const nodes = graph.nodes || [];
  const edges = graph.edges || [];
  const byId = new Map(nodes.map(n => [n.id, n]));
  if (!byId.has(focusId)) return { nodes: [], edges: [] };

  // Fixed small context: focus node + immediate prereqs + immediate dependents
  const prereqs = nodes.filter(n => byId.get(focusId)?.prerequisites?.includes(n.id));
  const dependents = nodes.filter(n => n.prerequisites?.includes(focusId));
  const included = new Set([focusId, ...prereqs.map(n => n.id), ...dependents.map(n => n.id)]);
  const subNodes = Array.from(included).map(id => byId.get(id)).filter(Boolean);
  const subEdges = edges.filter(e => included.has(e.source) && included.has(e.target));
  return { nodes: subNodes, edges: subEdges };
}

export const handler = async (event) => {
  try {
    const { focusId, question, knownIds = [], history = [] } = JSON.parse(event.body || '{}');

    if (!focusId || typeof focusId !== 'string') {
      return { statusCode: 400, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Missing focusId' }) };
    }
    const cleanQ = sanitizeQuestion(question);

    const subgraph = buildNeighborhood(focusId);

    const known = Array.isArray(knownIds) ? knownIds.map(String).slice(0, 100) : [];
    const msgs = [
      { role: 'system', content: 'You are a helpful tutor grounded in a knowledge graph and a specific node context. Keep answers very short (≤ 80 words). No references or citations.' },
      { role: 'user', content: `Focus node: ${focusId}\nKnown completed nodes: ${known.join(', ') || '(none)'}\nContext (focus + immediate prereqs/dependents): ${JSON.stringify(subgraph).slice(0, 12000)}\nQuestion: ${cleanQ || '(briefly explain this topic)'}\nReturn JSON with fields: answer (plain text), suggestedNext (array of {id, reason}). Answer ≤ 80 words.` }
    ];

    if (Array.isArray(history) && history.length) {
      history.slice(-6).forEach(m => {
        if (m && (m.role === 'user' || m.role === 'assistant') && typeof m.content === 'string') {
          msgs.push({ role: m.role, content: m.content.slice(0, 4000) });
        }
      });
    }

    const out = await cb.chat.completions.create({
      model: 'llama-4-scout-17b-16e-instruct',
      response_format: {
        type: 'json_schema',
        json_schema: {
          name: 'node_answer',
          strict: true,
          schema: {
            type: 'object',
            additionalProperties: false,
            required: ['answer','suggestedNext'],
            properties: {
              answer: { type: 'string', maxLength: 800 },
              suggestedNext: {
                type: 'array',
                items: {
                  type: 'object', additionalProperties: false,
                  required: ['id','reason'],
                  properties: { id: { type:'string' }, reason: { type:'string', maxLength: 300 } }
                },
                maxItems: 6
              }
            }
          }
        }
      },
      messages: msgs,
      temperature: 0.2,
      max_completion_tokens: 250
    });

    let payload;
    try {
      payload = JSON.parse(out?.choices?.[0]?.message?.content || '{}');
    } catch {
      payload = { answer: out?.choices?.[0]?.message?.content || '', suggestedNext: [] };
    }

    const validIds = new Set((graph.nodes || []).map(n => n.id));
    const normalizedNext = Array.isArray(payload.suggestedNext) ? payload.suggestedNext
      .filter(x => x && typeof x.id === 'string' && typeof x.reason === 'string' && validIds.has(x.id))
      .slice(0, 6)
      : [];

    const body = {
      answer: String(payload.answer || '').slice(0, 800),
      suggestedNext: normalizedNext
    };

    return { statusCode: 200, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) };
  } catch (err) {
    return { statusCode: 500, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Server error', details: String(err) }) };
  }
};


