// ingest/ingest_ocw.js
import 'dotenv/config';
import fs from 'fs';
import path from 'path';
import { JSDOM } from 'jsdom';
import { htmlToText } from 'html-to-text';
import glob from 'glob';
import { extractGraphFromChunk } from '../llm/extractGraph.js';

const OCW_DIR = process.argv[2] || './ocw_course'; // unzip target
const OUT = './data/graph.json';

// naive HTML → text
function strip(html) {
  return htmlToText(html, { wordwrap: false, selectors: [{ selector:'nav', format:'skip' }] });
}
// simple merge (dedupe by id)
function mergeGraph(acc, part) {
  const seen = new Set(acc.nodes.map(n => n.id));
  for (const n of part.nodes) if (!seen.has(n.id)) acc.nodes.push(n);
  const edgeKey = e => `${e.source}->${e.target}:${e.type}`;
  const seenE = new Set(acc.edges.map(edgeKey));
  for (const e of part.edges) if (!seenE.has(edgeKey(e))) acc.edges.push(e);
  return acc;
}

(async () => {
  const files = glob.sync(path.join(OCW_DIR, '**/*.html'));
  const chunks = [];
  for (const file of files) {
    const html = fs.readFileSync(file, 'utf8');
    const dom = new JSDOM(html);
    const main = dom.window.document.querySelector('main') || dom.window.document.body;
    const text = strip(main?.innerHTML || '');
    // rough chunking by headings
    const parts = text.split(/\n(?=[A-Z][^\n]{0,80}\n[-=]{3,})/g).filter(s => s.length > 300);
    parts.forEach(p => chunks.push({ file, text: p.slice(0, 4000) })); // keep prompt size tame
  }

  let graph = { nodes: [], edges: [] };
  for (const c of chunks) {
    try {
      const partial = await extractGraphFromChunk(c.text);
      // add localRef where possible
      partial.nodes.forEach(n => n.resources.push({ label: 'OCW Source', localRef: c.file }));
      graph = mergeGraph(graph, partial);
    } catch (e) {
      console.error('Chunk failed:', c.file, e.message);
    }
  }

  fs.writeFileSync(OUT, JSON.stringify(graph, null, 2));
  console.log(`Wrote ${OUT} with ${graph.nodes.length} nodes, ${graph.edges.length} edges.`);
})();
