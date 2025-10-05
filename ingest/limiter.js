import Bottleneck from "bottleneck";
import { encode } from "gpt-tokenizer";

// Default to a conservative Free-tier TPM if headers aren't seen yet.
let TOKENS_PER_MIN = Number(process.env.CB_TOKENS_PER_MIN || 60000); // approx Free-tier
const SAFETY = 0.9;
function tokensBudget() { return Math.floor(TOKENS_PER_MIN * SAFETY); }

const limiter = new Bottleneck({
  minTime: 0,
  reservoir: tokensBudget(),
  reservoirRefreshAmount: tokensBudget(),
  reservoirRefreshInterval: 60_000,
});

// Estimate tokens for a message payload (system+user+schema)
export function estimateTokens(str) {
  return encode(str).length + 300; // ~300 overhead for schema/messages
}

// Call fn under token budget; if server tells us new limits, auto-tune.
export async function scheduleLLM(callFn, tokenCost) {
  return limiter.schedule({ weight: tokenCost }, async () => {
    const { data, headers } = await callFn(); // callFn must return { data, headers }
    const minuteRem = Number(headers?.["x-ratelimit-remaining-tokens-minute"]);
    const minuteReset = Number(headers?.["x-ratelimit-reset-tokens-minute"]);
    if (Number.isFinite(minuteRem) && Number.isFinite(minuteReset)) {
      // Infer rolling TPM from remaining + elapsed proportionally
      const inferredTPM = Math.max(20000, minuteRem + tokensBudget()); // coarse, but stabilizes
      if (Math.abs(inferredTPM - TOKENS_PER_MIN) / TOKENS_PER_MIN > 0.25) {
        TOKENS_PER_MIN = inferredTPM;
        limiter.updateSettings({
          reservoir: tokensBudget(),
          reservoirRefreshAmount: tokensBudget(),
          reservoirRefreshInterval: 60_000
        });
      }
    }
    return data;
  });
}
