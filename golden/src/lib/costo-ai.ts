// Stima in USD del costo di una chiamata a Claude, dai token riportati in
// `response.usage` (Anthropic Messages API). Puramente derivato da numeri
// già restituiti dalla chiamata stessa — nessuna chiamata di rete in più.

export type UsageChiamataAI = {
  input_tokens: number;
  output_tokens: number;
  cache_creation_input_tokens?: number | null;
  cache_read_input_tokens?: number | null;
};

// Prezzi $/milione di token, solo per i modelli effettivamente usati da
// questa app (vedi MODEL in claude.ts). Aggiorna qui se cambia
// ANTHROPIC_MODEL o se Anthropic rivede i prezzi.
const PREZZI_PER_MILIONE: Record<string, { input: number; output: number }> = {
  "claude-sonnet-5-5": { input: 2.0, output: 10.0 },
  "claude-opus-5-5": { input: 4.0, output: 20.0 },
};
const PREZZO_FALLBACK = PREZZI_PER_MILIONE["claude-sonnet-5-5"];

// Rapporti standard Anthropic per i token di cache rispetto al prezzo
// input base dello stesso modello: scrivere in cache costa di più (si paga
// anche la scrittura), leggerne costa molto meno del prezzo pieno.
const MOLTIPLICATORE_CACHE_SCRITTURA = 1.25;
const MOLTIPLICATORE_CACHE_LETTURA = 0.1;

/** Costo stimato in USD di una singola chiamata, dato il model id usato. */
export function calcolaCostoUsd(usage: UsageChiamataAI, model: string): number {
  const prezzi = PREZZI_PER_MILIONE[model] || PREZZO_FALLBACK;

  const costoInput = (usage.input_tokens / 1_000_000) * prezzi.input;
  const costoOutput = (usage.output_tokens / 1_000_000) * prezzi.output;
  const costoCacheScrittura =
    ((usage.cache_creation_input_tokens || 0) / 1_000_000) * prezzi.input * MOLTIPLICATORE_CACHE_SCRITTURA;
  const costoCacheLettura =
    ((usage.cache_read_input_tokens || 0) / 1_000_000) * prezzi.input * MOLTIPLICATORE_CACHE_LETTURA;

  return costoInput + costoOutput + costoCacheScrittura + costoCacheLettura;
}
