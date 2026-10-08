import { describe, it, expect } from "vitest";
import { calcolaCostoUsd } from "./costo-ai";

describe("calcolaCostoUsd", () => {
  it("calcola il costo per input e output ai prezzi di claude-sonnet-5-5", () => {
    const costo = calcolaCostoUsd({ input_tokens: 1_000_000, output_tokens: 1_000_000 }, "claude-sonnet-5-5");
    expect(costo).toBeCloseTo(2.0 + 10.0, 6);
  });

  it("usa i prezzi di claude-opus-5-5 quando richiesto", () => {
    const costo = calcolaCostoUsd({ input_tokens: 1_000_000, output_tokens: 1_000_000 }, "claude-opus-5-5");
    expect(costo).toBeCloseTo(4.0 + 20.0, 6);
  });

  it("ricade sui prezzi di claude-sonnet-5-5 per un modello non in tabella", () => {
    const costo = calcolaCostoUsd({ input_tokens: 1_000_000, output_tokens: 0 }, "modello-sconosciuto");
    expect(costo).toBeCloseTo(2.0, 6);
  });

  it("aggiunge il costo di scrittura in cache (1.25x il prezzo input)", () => {
    const costo = calcolaCostoUsd(
      { input_tokens: 0, output_tokens: 0, cache_creation_input_tokens: 1_000_000 },
      "claude-sonnet-5-5",
    );
    expect(costo).toBeCloseTo(2.0 * 1.25, 6);
  });

  it("aggiunge il costo di lettura dalla cache (0.1x il prezzo input)", () => {
    const costo = calcolaCostoUsd(
      { input_tokens: 0, output_tokens: 0, cache_read_input_tokens: 1_000_000 },
      "claude-sonnet-5-5",
    );
    expect(costo).toBeCloseTo(2.0 * 0.1, 6);
  });

  it("tratta i campi cache mancanti o null come zero", () => {
    const costo = calcolaCostoUsd({ input_tokens: 1000, output_tokens: 500 }, "claude-sonnet-5-5");
    const costoConNull = calcolaCostoUsd(
      { input_tokens: 1000, output_tokens: 500, cache_creation_input_tokens: null, cache_read_input_tokens: null },
      "claude-sonnet-5-5",
    );
    expect(costoConNull).toBeCloseTo(costo, 10);
  });
});
