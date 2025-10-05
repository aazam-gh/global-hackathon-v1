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

app.listen(3000, () => console.log('http://localhost:3000'));
