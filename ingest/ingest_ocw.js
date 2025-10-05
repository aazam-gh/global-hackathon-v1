// ingest/ingest_ocw.js
import 'dotenv/config';
import fs from 'fs';
import path from 'path';
import { JSDOM } from 'jsdom';
import { htmlToText } from 'html-to-text';
import { globSync } from 'glob';
import { extractGraphFromChunk } from '../llm/extractGraph.js';

const OCW_DIR = process.argv[2] || './ocw_course'; // unzip target
const OUT = './data/graph.json';
const argv = process.argv.slice(3);

function getArg(flag, fallback) {
  const idx = argv.indexOf(flag);
  if (idx !== -1 && argv[idx + 1]) return argv[idx + 1];
  return fallback;
}

const scope = getArg('--scope', process.env.INGEST_SCOPE || 'pages'); // pages | all
const limit = parseInt(getArg('--limit', process.env.INGEST_LIMIT || '40'), 10);
const delayMs = parseInt(getArg('--delay-ms', process.env.INGEST_DELAY_MS || '1200'), 10);
const maxRetries = parseInt(getArg('--retries', process.env.INGEST_MAX_RETRIES || '3'), 10);

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

function sleep(ms) { return new Promise(r => setTimeout(r, ms)); }

async function callWithBackoff(text, file) {
  let attempt = 0;
  let backoff = delayMs;
  // basic retry for 429
  // eslint-disable-next-line no-constant-condition
  while (true) {
    try {
      return await extractGraphFromChunk(text);
    } catch (e) {
      const message = String(e?.message || e);
      const isRate = message.includes('429') || message.toLowerCase().includes('rate');
      if (!isRate || attempt >= maxRetries) throw e;
      attempt += 1;
      const jitter = Math.floor(Math.random() * 200);
      const waitFor = backoff + jitter;
      console.warn(`Rate limited on ${file}. Retry ${attempt}/${maxRetries} after ${waitFor}ms`);
      await sleep(waitFor);
      backoff = Math.min(backoff * 2, 8000);
    }
  }
}

(async () => {
  const pattern = scope === 'all'
    ? path.join(OCW_DIR, '**/*.html')
    : path.join(OCW_DIR, 'pages/**/*.html');
  const files = globSync(pattern);
  const chunks = [];
  for (const file of files) {
    const html = fs.readFileSync(file, 'utf8');
    const dom = new JSDOM(html);
    const main = dom.window.document.querySelector('main') || dom.window.document.body;
    const text = strip(main?.innerHTML || '');
    // rough chunking by headings
    const parts = text.split(/\n(?=[A-Z][^\n]{0,80}\n[-=]{3,})/g).filter(s => s.length > 300);
    parts.forEach(p => {
      const trimmed = p.replace(/\s+/g, ' ').slice(0, 3000);
      chunks.push({ file, text: trimmed });
    }); // keep prompt size tame
  }

  const selected = Number.isFinite(limit) && limit > 0 ? chunks.slice(0, limit) : chunks;
  console.log(`Processing ${selected.length}/${chunks.length} chunks from ${files.length} files (scope=${scope}).`);

  let graph = { nodes: [], edges: [] };
  let processed = 0;
  for (const c of selected) {
    try {
      const partial = await callWithBackoff(c.text, c.file);
      // add localRef where possible
      partial.nodes.forEach(n => {
        if (!Array.isArray(n.resources)) n.resources = [];
        n.resources.push({ label: 'OCW Source', localRef: c.file });
      });
      graph = mergeGraph(graph, partial);
    } catch (e) {
      console.error('Chunk failed:', c.file, e.message);
    }
    processed += 1;
    if (delayMs > 0) await sleep(delayMs);
    if (processed % 5 === 0) console.log(`Progress: ${processed}/${selected.length}`);
  }

  fs.writeFileSync(OUT, JSON.stringify(graph, null, 2));
  console.log(`Wrote ${OUT} with ${graph.nodes.length} nodes, ${graph.edges.length} edges.`);
})();
