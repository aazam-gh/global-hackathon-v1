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
    const focusNode = (graph.nodes || []).find(n => n.id === focusId) || {};
    const exampleIntent = /\b(example|sample|exercise|practice|question\s*example)\b/i.test(cleanQ || '');

    const known = Array.isArray(knownIds) ? knownIds.map(String).slice(0, 100) : [];
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
    msgs.push({ role: 'user', content: `Focus node id: ${focusId}\nFocus node details: ${JSON.stringify(nodeDetails)}\nKnown completed nodes: ${known.join(', ') || '(none)'}\nContext (focus + immediate prereqs/dependents): ${JSON.stringify(subgraph).slice(0, 12000)}\nUser question: ${cleanQ || '(briefly explain this topic)'}\n${exampleDirective}\nReturn JSON with fields: answer (plain text), suggestedNext (array of {id, reason}). Do not propose a plan.` });

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
      max_completion_tokens: 300
    });

    let payload;
    try {
      payload = JSON.parse(out?.choices?.[0]?.message?.content || '{}');
    } catch {
      payload = { answer: out?.choices?.[0]?.message?.content || '', suggestedNext: [] };
    }

    const validIds = new Set((graph.nodes || []).map(n => n.id));
    let normalizedNext = Array.isArray(payload.suggestedNext) ? payload.suggestedNext
      .filter(x => x && typeof x.id === 'string' && typeof x.reason === 'string' && validIds.has(x.id))
      .slice(0, 6)
      : [];
    if (exampleIntent) normalizedNext = [];

    let answer = String(payload.answer || '').slice(0, 800);
    // Sanitize plan-like prefixes or headings
    answer = answer.replace(/\*\*?next steps:?\*\*?/gi, '').replace(/\b(next steps|plan)[:：]/gi, '');
    // Enforce ≤ 100 words hard cap as a final safeguard
    const words = answer.trim().split(/\s+/);
    if (words.length > 100) answer = words.slice(0, 100).join(' ');

    const body = {
      answer,
      suggestedNext: normalizedNext
    };

    return { statusCode: 200, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) };
  } catch (err) {
    return { statusCode: 500, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Server error', details: String(err) }) };
  }
};


