import { cb } from '../../lib/cerebras.js';
import graph from '../../data/graph.json';

export const handler = async (event) => {
  try {
    const { question, knownIds = [] } = JSON.parse(event.body || '{}');

    const subset = {
      nodes: (graph.nodes || []).filter(n => knownIds.includes(n.id) || (n.prerequisites || []).some(p => knownIds.includes(p))),
      edges: (graph.edges || []).filter(e => knownIds.includes(e.source) || knownIds.includes(e.target))
    };

    const context = JSON.stringify(subset).slice(0, 12000);

    const out = await cb.chat.completions.create({
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
    });

    return {
      statusCode: 200,
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ answer: out.choices[0].message.content })
    };
  } catch (err) {
    return {
      statusCode: 500,
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ error: 'Server error', details: String(err) })
    };
  }
};
