// llm/extractGraph.js
import { cb } from '../lib/cerebras.js';

export async function extractGraphFromChunk(textChunk) {
  const response = await cb.chat.completions.create({
    model: 'llama-4-scout-17b-16e-instruct',
    response_format: {
      type: 'json_schema',
      json_schema: {
        name: 'concept_graph',
        strict: true,
        schema: {
          type: 'object',
          additionalProperties: false,
          properties: {
            nodes: {
              type: 'array',
              items: {
                type: 'object',
                additionalProperties: false,
                properties: {
                  id: { type: 'string' },
                  title: { type: 'string' },
                  kind: { enum: ['concept','skill','example','theorem'] },
                  summary: { type: 'string' },
                  prerequisites: { type: 'array', items: { type: 'string' } },
                  outcomes: { type: 'array', items: { type: 'string' } },
                  resources: {
                    type: 'array',
                    items: {
                      type: 'object',
                      additionalProperties: false,
                      properties: {
                        label: { type:'string' },
                        url: { type: 'string' },
                        localRef: { type: 'string' }
                      }
                    }
                  }
                },
                required: ['id','title','kind','summary','prerequisites','outcomes','resources']
              }
            },
            edges: {
              type: 'array',
              items: {
                type:'object',
                additionalProperties: false,
                properties: {
                  source: { type:'string' },
                  target: { type:'string' },
                  type: { enum: ['prereq','refines','applies-to','related'] },
                  weight: { type:'number' }
                },
                required: ['source','target','type']
              }
            }
          },
          required: ['nodes','edges']
        }
      }
    },
    messages: [
      {
        role: 'system',
        content:
          "You convert textbook-like content into a compact knowledge graph. " +
          "Return only valid JSON that conforms to the provided schema. " +
          "Prefer prerequisite edges for dependency relationships."
      },
      {
        role: 'user',
        content:
          `Extract a concept graph from the following course text. 
           Use stable, kebab-case ids. Summaries ≤ 60 words. 
           If some concepts are implied (e.g., law of total probability, Bayes), include them.
           
           TEXT:
           ${textChunk}`
      }
    ],
    temperature: 0.2,
    max_completion_tokens: 1500
  });

  const content = response.choices[0]?.message?.content;
  return JSON.parse(content);
}
