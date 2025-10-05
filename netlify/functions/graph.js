import graph from '../../data/graph.json';

export const handler = async () => {
  return {
    statusCode: 200,
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(graph)
  };
};
