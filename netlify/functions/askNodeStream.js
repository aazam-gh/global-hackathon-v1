import { streamChat } from '../../lib/cerebras.js';
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

export const handler = async (event) => {
  try {
    const { focusId, question, knownIds = [], history = [] } = JSON.parse(event.body || '{}');
    const nodes = graph.nodes || [];
    const byId = new Map(nodes.map(n => [n.id, n]));
    if (!byId.has(focusId)) {
      return { statusCode: 400, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Invalid focusId' }) };
    }

    const subgraph = buildNeighborhood(focusId);
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

    const encoder = new TextEncoder();
    const stream = new ReadableStream({
      start(controller) {
        (async () => {
          let text = '';
          try {
            await streamChat(params, (delta) => {
              text += delta;
              const chunk = `event: token\n` + `data: ${delta}\n\n`;
              controller.enqueue(encoder.encode(chunk));
            });
            let payload;
            try { payload = JSON.parse(text); } catch { payload = { answer: text, suggestedNext: [] }; }
            const done = `event: done\n` + `data: ${JSON.stringify(payload)}\n\n`;
            controller.enqueue(encoder.encode(done));
          } catch (e) {
            const err = `event: error\n` + `data: ${JSON.stringify({ message: String(e) })}\n\n`;
            controller.enqueue(encoder.encode(err));
          } finally {
            controller.close();
          }
        })();
      }
    });

    return new Response(stream, {
      status: 200,
      headers: {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache, no-transform',
        'Connection': 'keep-alive'
      }
    });
  } catch (err) {
    return { statusCode: 500, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Server error', details: String(err) }) };
  }
};


