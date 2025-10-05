// lib/cerebras.js
import Cerebras from '@cerebras/cerebras_cloud_sdk';

export const cb = new Cerebras({
  apiKey: process.env.CEREBRAS_API_KEY,
  // warmTCPConnection true by default; keep the client singleton for perf
});
