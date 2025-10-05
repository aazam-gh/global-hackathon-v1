// llm/extractGraph.js
import { cb } from '../lib/cerebras.js';
import { estimateTokens, scheduleLLM } from '../ingest/limiter.js';
import { withRetries } from '../lib/retry.js';
import JSON5 from 'json5';

export async function extractGraphFromChunk(textChunk) {
  const sys = "You convert course text into a compact knowledge graph. Return only JSON that matches the schema; prefer 'prereq' edges.";
  const user = `Extract a concept graph from this course text. 
Use kebab-case ids. Summaries ≤ 40 words.

For each node, set:
- difficulty: 1 (intro) to 5 (advanced)
- domain: probability | statistics | r | meta-learning | other
- 2–6 short learning objectives (verbs!)
- 3–10 keywords (lowercase)

TEXT:
${textChunk}`;

  const completionAllowance = 800;
  const tokenCost = estimateTokens(sys + user) + completionAllowance;

  function stripCodeFences(s) {
    if (!s) return s;
    return s.replace(/```[a-zA-Z]*\n([\s\S]*?)```/g, '$1');
  }

  function extractBalancedJson(s) {
    const text = stripCodeFences(String(s).trim());
    let start = -1;
    let depth = 0;
    for (let i = 0; i < text.length; i++) {
      const ch = text[i];
      if (ch === '{') { if (start === -1) start = i; depth++; }
      else if (ch === '}') { depth--; if (depth === 0 && start !== -1) { return text.slice(start, i + 1); } }
    }
    // fallback: take from first { to last }
    const first = text.indexOf('{');
    const last = text.lastIndexOf('}');
    if (first !== -1 && last !== -1 && last > first) return text.slice(first, last + 1);
    return text;
  }

  return scheduleLLM(async () => {
    const resp = await withRetries(() =>
      cb.chat.completions.create({
        model: 'llama-4-scout-17b-16e-instruct',
        response_format: {
          type: 'json_schema',
          json_schema: {
            name: 'concept_graph',
            strict: true,
            schema: {
              type: 'object',
              additionalProperties: false,
              required: ['nodes', 'edges'],
              properties: {
                nodes: {
                  type: 'array',
                  items: {
                    type: 'object',
                    additionalProperties: false,
                    required: ['id','title','kind','summary','prerequisites','outcomes','resources','difficulty','domain','objectives','keywords'],
                    properties: {
                      id: { type:'string' }, title:{ type:'string' },
                      kind:{ enum:['concept','skill','example','theorem'] },
                      summary:{ type:'string' },
                      prerequisites:{ type:'array', items:{ type:'string' } },
                      outcomes:{ type:'array', items:{ type:'string' } },
                      resources:{ type:'array', items:{
                        type:'object', additionalProperties:false,
                        properties:{ label:{type:'string'}, url:{type:'string'}, localRef:{type:'string'} }
                        } },
                        difficulty: { type: 'integer', minimum: 1, maximum: 5 },
                        domain: { enum: ['probability','statistics','r','meta-learning','other'] },
                        objectives: { type: 'array', items: { type: 'string' }, maxItems: 6 },
                        keywords: { type: 'array', items: { type: 'string' }, maxItems: 10 }
                    }
                  }
                },
                edges: {
                  type:'array',
                  items:{
                    type:'object', additionalProperties:false,
                    required:['source','target','type'],
                    properties:{ source:{type:'string'}, target:{type:'string'}, type:{enum:['prereq','refines','applies-to','related']}, weight:{type:'number'} }
                  }
                }
              }
            }
          }
        },
        messages: [{ role:'system', content: sys }, { role:'user', content: user }],
        temperature: 0.1,
        max_completion_tokens: 800
      })
    );

    // The SDK may not expose raw headers directly, so get them from resp?.response
    const headers = resp?.response?.headers || {};
    const raw = resp?.choices?.[0]?.message?.content || '';
    let parsed;
    try {
      parsed = JSON.parse(raw);
    } catch (e) {
      try {
        const extracted = extractBalancedJson(raw)
          .replace(/[\u201C\u201D]/g, '"')
          .replace(/[\u2018\u2019]/g, "'");
        parsed = JSON5.parse(extracted);
      } catch (e2) {
        throw e;
      }
    }
    // Defensive caps and defaults
    parsed.nodes = Array.isArray(parsed.nodes) ? parsed.nodes.slice(0, 50) : [];
    parsed.edges = Array.isArray(parsed.edges) ? parsed.edges.slice(0, 120) : [];
    parsed.nodes.forEach(n => {
      if (!Array.isArray(n.resources)) n.resources = [];
      n.resources = n.resources.slice(0, 5);
      if (!Array.isArray(n.objectives)) n.objectives = [];
      n.objectives = n.objectives.map(o => String(o)).slice(0, 6);
      if (!Array.isArray(n.keywords)) n.keywords = [];
      n.keywords = n.keywords.map(k => String(k).toLowerCase()).slice(0, 10);
      if (!Number.isInteger(n.difficulty)) n.difficulty = 1;
      n.difficulty = Math.min(5, Math.max(1, n.difficulty));
      const allowedDomains = ['probability','statistics','r','meta-learning','other'];
      if (!allowedDomains.includes(n.domain)) n.domain = 'other';
    });
    return { data: parsed, headers };
  }, tokenCost);
}
