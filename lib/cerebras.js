// lib/cerebras.js
import Cerebras from '@cerebras/cerebras_cloud_sdk';

export const cb = new Cerebras({
  apiKey: process.env.CEREBRAS_API_KEY,
  // warmTCPConnection true by default; keep the client singleton for perf
});

// Best-effort streaming helper.
// Tries native SDK streaming; falls back to non-streaming and manual chunking.
export async function streamChat(params, onDelta) {
  // Guard
  const safeOnDelta = typeof onDelta === 'function' ? onDelta : () => {};

  // Try native streaming if available
  try {
    // Some SDKs expose chat.completions.stream(params)
    if (cb?.chat?.completions?.stream) {
      const stream = await cb.chat.completions.stream({ ...params, stream: true });
      // If the returned object is async-iterable, iterate tokens
      if (typeof stream?.[Symbol.asyncIterator] === 'function') {
        for await (const ev of stream) {
          const delta = ev?.choices?.[0]?.delta?.content || ev?.delta?.content || ev?.content || '';
          if (delta) safeOnDelta(String(delta));
        }
        return;
      }
      // Fallback: event emitter style
      if (typeof stream?.on === 'function') {
        await new Promise((resolve, reject) => {
          stream.on('message', (ev) => {
            try {
              const delta = ev?.choices?.[0]?.delta?.content || ev?.delta?.content || ev?.content || '';
              if (delta) safeOnDelta(String(delta));
            } catch {}
          });
          stream.on('end', resolve);
          stream.on('error', reject);
        });
        return;
      }
    }
  } catch (_) {
    // ignore and fall through to non-streaming fallback
  }

  // Non-streaming fallback: get full text and emit in chunks
  const resp = await cb.chat.completions.create({ ...params, stream: false });
  const text = resp?.choices?.[0]?.message?.content || '';
  if (!text) return;
  // Emit in small chunks to simulate streaming
  const parts = text.split(/(\s+)/).filter(Boolean);
  for (const p of parts) {
    safeOnDelta(p);
    await new Promise(r => setTimeout(r, 10));
  }
}
