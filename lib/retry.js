export async function withRetries(fn, { tries = 6, baseMs = 1000 } = {}) {
    let n = 0;
    while (true) {
      try { return await fn(); }
      catch (err) {
        const status = err?.status || err?.response?.status;
        n++;
        if (status !== 429 || n >= tries) throw err;
        const h = err?.response?.headers || {};
        const reset =
          Number(h["x-ratelimit-reset-tokens-minute"]) ||
          Number(h["retry-after"]) || 0;
        const delay = reset > 0 ? reset * 1000 : baseMs * Math.pow(2, n - 1);
        await new Promise(r => setTimeout(r, delay));
      }
    }
  }
  