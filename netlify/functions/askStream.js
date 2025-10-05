import { streamChat } from '../../lib/cerebras.js';
import graph from '../../data/graph.json';

export const handler = async (event) => {
  try {
    const { question, knownIds = [] } = JSON.parse(event.body || '{}');

    const subset = {
      nodes: (graph.nodes || []).filter(n => knownIds.includes(n.id) || (n.prerequisites || []).some(p => knownIds.includes(p))),
      edges: (graph.edges || []).filter(e => knownIds.includes(e.source) || knownIds.includes(e.target))
    };
    const context = JSON.stringify(subset).slice(0, 12000);

    const params = {
      model: 'llama-4-scout-17b-16e-instruct',
      messages: [
        { role: 'system', content: 'You are a tutor that uses a knowledge graph to plan next steps.' },
        { role: 'user', content:
          `Question: ${question}\nKnown completed nodes: ${knownIds.join(', ') || '(none)'}\n` +
          `Graph context (subset): ${context}\n` +
          `Return a brief plan with 3-5 next nodes and why each matters.`
        }
      ],
      temperature: 0.3,
      max_completion_tokens: 500
    };

    const encoder = new TextEncoder();
    const stream = new ReadableStream({
      start(controller) {
        (async () => {
          let full = '';
          try {
            await streamChat(params, (delta) => {
              full += delta;
              const chunk = `event: token\n` + `data: ${delta}\n\n`;
              controller.enqueue(encoder.encode(chunk));
            });
            const done = `event: done\n` + `data: ${JSON.stringify({ answer: full })}\n\n`;
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


