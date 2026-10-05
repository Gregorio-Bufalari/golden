import { describe, it, expect } from "vitest";
import { isIngredienteARischio, ingredientiARischio } from "./glutine-check";

describe("isIngredienteARischio", () => {
  it("segnala gli ingredienti con glutine evidente", () => {
    expect(isIngredienteARischio("Pasta")).toBe(true);
    expect(isIngredienteARischio("Farina di frumento")).toBe(true);
    expect(isIngredienteARischio("Pane")).toBe(true);
    expect(isIngredienteARischio("Salsa di soia")).toBe(true);
    expect(isIngredienteARischio("Dado vegetale")).toBe(true);
  });

  it("non segnala ingredienti chiaramente senza glutine", () => {
    expect(isIngredienteARischio("Petto di pollo")).toBe(false);
    expect(isIngredienteARischio("Pomodoro")).toBe(false);
    expect(isIngredienteARischio("Riso")).toBe(false);
    expect(isIngredienteARischio("Uova")).toBe(false);
  });

  it("non segnala un ingrediente a rischio se ha un qualificatore sicuro", () => {
    expect(isIngredienteARischio("Pasta di riso")).toBe(false);
    expect(isIngredienteARischio("Farina di mais")).toBe(false);
    expect(isIngredienteARischio("Pasta senza glutine")).toBe(false);
    expect(isIngredienteARischio("Cuscus di quinoa")).toBe(false);
  });

  it("non è sensibile a maiuscole/minuscole", () => {
    expect(isIngredienteARischio("PASTA")).toBe(true);
    expect(isIngredienteARischio("fRuMeNtO")).toBe(true);
  });
});

describe("ingredientiARischio", () => {
  it("filtra solo gli ingredienti a rischio da una lista", () => {
    const risultato = ingredientiARischio(["Petto di pollo", "Pasta", "Pomodoro", "Farro"]);
    expect(risultato).toEqual(["Pasta", "Farro"]);
  });

  it("restituisce un array vuoto se nessun ingrediente è a rischio", () => {
    expect(ingredientiARischio(["Riso", "Pollo", "Pasta di riso"])).toEqual([]);
  });
});
