# Graph-Based Learning Tutor (MIT OCW Edition)

Turn a linear course into an interactive knowledge graph you can explore, chat with, and quiz yourself on — powered by Cerebras inference.

This project ingests content from MIT OpenCourseWare, builds a compact concept graph, and serves a delightful UI to learn non‑linearly: jump between related concepts, ask “what should I learn next?”, get a focused explanation of a node, and generate MCQ quizzes grounded in the node’s neighborhood.

Live demo: https://acta-hack-aazam.netlify.app/

## Features

- Interactive knowledge graph (vis‑network) filtered to OCW‑sourced nodes
- “What’s next?” tutor using the graph as context (`/ask`, streaming at `/ask/stream`)
- Node chat with streaming responses and optional 1 example (`/askNode/stream`)
- Auto‑generated, strictly MCQ quizzes for any node (`/quiz` and Netlify `/.netlify/functions/quiz`)
- Local progress tracking: mark nodes Known; quiz correctness saved in browser
- Built‑in OCW snapshot served locally at `/ocw` for fast, private browsing

## Quick start

Prerequisites: Node.js 20+ (tested on Node 22), a Cerebras API key.

```bash
git clone <your-fork-url>
cd global-hackathon-v1
npm install

# Required: Cerebras API key for LLM features
export CEREBRAS_API_KEY=sk-...  # macOS/Linux
# setx CEREBRAS_API_KEY "sk-..."  # Windows PowerShell

# Start local server (serves UI + local APIs)
npm run dev
# open http://localhost:3000
```

UI tips:
- Search a node by title or id, press Enter to cycle matches.
- Click a node to open the right chat panel; press “Quiz” to generate a quiz.
- Press “N” to stream “What’s next?” suggestions.
- Use “Mark known” to track progress; “Focus” to filter the current neighborhood; “Reset” to see the whole graph again.

## Environment

- `CEREBRAS_API_KEY` (required): used by `lib/cerebras.js` to call the Cerebras SDK.
- Port: fixed to `3000` in `server.js` (customize if desired).

## Data model

The app reads the graph from `data/graph.json`. Nodes and edges follow the schema in `data/schema.ts` (high‑level):
- Node: `id`, `title`, `kind` (concept|skill|example|theorem), `summary`, `prerequisites[]`, `objectives[]`, `keywords[]`, `resources[]`, `difficulty` (1–5), `domain`
- Edge: `source`, `target`, `type` (`prereq|refines|applies-to|related`)

Only nodes with OCW‑matching resources are shown in the UI (see resource filters in `public/index.html`).

## Local API (Express)

Base URL: `http://localhost:3000`

- GET `/graph`
  - Returns the full graph JSON.
- POST `/ask`
  - Body: `{ question: string, knownIds?: string[] }`
  - Returns: `{ answer: string }`
- POST `/ask/stream` (SSE‑like chunked)
  - Body: `{ question: string, knownIds?: string[] }`
  - Emits `event: token` with text chunks, and `event: done` with `{ answer }`.
- POST `/quiz`
  - Body: `{ nodeId: string }`
  - Returns: `{ nodeId, items: MCQ[] }` where each item has `{ id, nodeId, type: 'mcq', question, options[4], answer, explanation, sources[] }`.
- POST `/askNode/stream` (SSE‑like chunked)
  - Body: `{ focusId: string, question: string, knownIds?: string[], history?: {role:'user'|'assistant',content:string}[] }`
  - Emits `event: token` with text chunks, and `event: done` with `{ answer, suggestedNext: {id,reason}[] }`.

Examples:

```bash
# Ask for next steps (non-streaming)
curl -sX POST http://localhost:3000/ask \
  -H 'Content-Type: application/json' \
  -d '{"question":"What should I learn next?","knownIds":["probability-basics"]}'

# Streamed suggestion
curl -N -sX POST http://localhost:3000/ask/stream \
  -H 'Content-Type: application/json' \
  -d '{"question":"What should I learn next?","knownIds":[]}'

# Streamed node chat
curl -N -sX POST http://localhost:3000/askNode/stream \
  -H 'Content-Type: application/json' \
  -d '{"focusId":"bayes-theorem","question":"brief example"}'

# Generate a quiz
curl -sX POST http://localhost:3000/quiz \
  -H 'Content-Type: application/json' \
  -d '{"nodeId":"normal-distribution"}'
```

## Production API (Netlify functions)

This repo ships with Netlify redirects (`netlify.toml`) so the same routes work in production:
- `/graph` → `/.netlify/functions/graph`
- `/ask` → `/.netlify/functions/ask`
- `/ask/stream` → `/.netlify/functions/askStream`
- `/askNode/stream` → `/.netlify/functions/askNodeStream`
- `/.netlify/functions/quiz` (and others used by the UI)

Netlify functions use the same Cerebras client and the same graph JSON.

## Ingestion (optional)

The graph can be regenerated from course text in chunks using the LLM:
- `llm/extractGraph.js` defines a JSON‑schema constrained extraction call to Cerebras.
- `ingest/ingest_ocw.js` can be adapted to fetch/prepare text.
- The OCW snapshot is under `ocw_course/`; a postbuild step copies it into `public/ocw` for hosting.

Tip: keep nodes concise (summaries ≤ 40 words), prefer `prereq` edges, and cap node/edge counts to keep the UI snappy.

## Deployment

### Netlify (recommended)
1. Connect your GitHub repo on Netlify.
2. In Site settings → Environment variables, add `CEREBRAS_API_KEY`.
3. Build settings are provided by `netlify.toml` (`publish = public`, `functions = netlify/functions`).
4. Deploy. The UI will be served from `public/`, OCW snapshot at `/ocw`, and functions will power the API.

### Self‑host
```bash
npm ci
export CEREBRAS_API_KEY=sk-...
npm start   # runs server.js on http://localhost:3000
```
Behind a reverse proxy, route traffic to port 3000. Static assets are served from `public/`.

## Troubleshooting

- Missing `CEREBRAS_API_KEY`: API calls will fail; set it in your environment.
- SSE seems blocked: some proxies buffer responses; try locally or ensure streaming pass‑through.
- Quiz has odd options: the server sanitizes items to 4 options and ensures the answer is present.
- Graph is empty: confirm `data/graph.json` exists and contains OCW‑sourced nodes.

## License

MIT — see `LICENSE`.

Credits: MIT OpenCourseWare for the underlying course content; Cerebras for fast inference.
