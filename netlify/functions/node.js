import graph from '../../data/graph.json';

export const handler = async (event) => {
  try {
    const { id } = JSON.parse(event.body || '{}');
    if (!id || typeof id !== 'string') {
      return { statusCode: 400, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Missing id' }) };
    }

    const nodes = graph.nodes || [];
    const edges = graph.edges || [];
    const node = nodes.find(n => n.id === id);
    if (!node) {
      return { statusCode: 404, headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ error: 'Not found' }) };
    }

    const incoming = edges.filter(e => e.target === id);
    const outgoing = edges.filter(e => e.source === id);
    const neighborIds = new Set([
      ...incoming.map(e => e.source),
      ...outgoing.map(e => e.target)
    ]);
    neighborIds.delete(id);

    const prerequisites = nodes.filter(n => node.prerequisites?.includes(n.id));
    const dependents = nodes.filter(n => n.prerequisites?.includes(id));
    const neighbors = nodes.filter(n => neighborIds.has(n.id));
    const relatedEdges = edges.filter(e => e.source === id || e.target === id);

    return {
      statusCode: 200,
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ node, prerequisites, dependents, neighbors, edges: relatedEdges })
    };
  } catch (err) {
    return {
      statusCode: 500,
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ error: 'Server error', details: String(err) })
    };
  }
};


