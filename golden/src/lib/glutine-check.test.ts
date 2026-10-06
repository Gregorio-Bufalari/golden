import { describe, it, expect } from "vitest";
import {
  categorizzaIngrediente,
  ingredientiNonAdatti,
  ingredientiDaVerificare,
  ingredientiDaSegnalare,
} from "./glutine-check";

describe("categorizzaIngrediente", () => {
  it("segnala 'non_adatto' per ingredienti con glutine senza ambiguità", () => {
    expect(categorizzaIngrediente("Pasta")).toBe("non_adatto");
    expect(categorizzaIngrediente("Farina di frumento")).toBe("non_adatto");
    expect(categorizzaIngrediente("Pane")).toBe("non_adatto");
    expect(categorizzaIngrediente("Farro")).toBe("non_adatto");
  });

  it("segnala 'da_verificare' per ingredienti il cui glutine dipende dalla marca", () => {
    expect(categorizzaIngrediente("Salsa di soia")).toBe("da_verificare");
    expect(categorizzaIngrediente("Dado vegetale")).toBe("da_verificare");
    expect(categorizzaIngrediente("Besciamella")).toBe("da_verificare");
    expect(categorizzaIngrediente("Avena")).toBe("da_verificare");
  });

  it("segnala 'informazioni_sufficienti' per ingredienti chiaramente senza glutine", () => {
    expect(categorizzaIngrediente("Petto di pollo")).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Pomodoro")).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Riso")).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Uova")).toBe("informazioni_sufficienti");
  });

  it("segnala 'informazioni_sufficienti' per una base alternativa intrinsecamente senza glutine", () => {
    expect(categorizzaIngrediente("Pasta di riso")).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Farina di mais")).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Cuscus di quinoa")).toBe("informazioni_sufficienti");
  });

  it("segnala 'verificato' solo per una certificazione esplicita", () => {
    expect(categorizzaIngrediente("Pasta senza glutine")).toBe("verificato");
    expect(categorizzaIngrediente("Pane gluten free")).toBe("verificato");
    expect(categorizzaIngrediente("Farina certificata senza glutine")).toBe("verificato");
  });

  it("i qualificatori hanno priorità sulle parole chiave di rischio", () => {
    // "pasta" matcherebbe non_adatto, ma il qualificatore esplicito vince.
    expect(categorizzaIngrediente("Pasta di riso certificata")).toBe("verificato");
  });

  it("non è sensibile a maiuscole/minuscole", () => {
    expect(categorizzaIngrediente("PASTA")).toBe("non_adatto");
    expect(categorizzaIngrediente("fRuMeNtO")).toBe("non_adatto");
  });
});

describe("ingredientiNonAdatti", () => {
  it("filtra solo gli ingredienti non adatto, non quelli da verificare", () => {
    const risultato = ingredientiNonAdatti(["Pollo", "Pasta", "Dado vegetale", "Pomodoro", "Farro"]);
    expect(risultato).toEqual(["Pasta", "Farro"]);
  });

  it("restituisce un array vuoto se nessun ingrediente è non adatto", () => {
    expect(ingredientiNonAdatti(["Riso", "Pollo", "Dado vegetale", "Pasta di riso"])).toEqual([]);
  });
});

describe("ingredientiDaVerificare", () => {
  it("filtra solo gli ingredienti da verificare, non quelli non adatto", () => {
    const risultato = ingredientiDaVerificare(["Pollo", "Pasta", "Dado vegetale", "Salsa di soia"]);
    expect(risultato).toEqual(["Dado vegetale", "Salsa di soia"]);
  });
});

describe("ingredientiDaSegnalare", () => {
  it("unisce non adatto e da verificare", () => {
    const risultato = ingredientiDaSegnalare(["Pollo", "Pasta", "Dado vegetale", "Pomodoro"]);
    expect(risultato).toEqual(["Pasta", "Dado vegetale"]);
  });

  it("restituisce un array vuoto se nessun ingrediente merita attenzione", () => {
    expect(ingredientiDaSegnalare(["Riso", "Pollo", "Pasta di riso"])).toEqual([]);
  });
});
