import { describe, it, expect } from "vitest";
import {
  categorizzaIngrediente,
  ingredientiNonAdatti,
  ingredientiDaVerificare,
  ingredientiDaSegnalare,
  allergeniDalProfilo,
} from "./allergeni-check";

describe("allergeniDalProfilo", () => {
  it("mappa le restrizioni note ai rispettivi allergeni", () => {
    expect(allergeniDalProfilo(["Glutine (celiachia)", "Uova"])).toEqual(["glutine", "uova"]);
  });

  it("ignora le restrizioni senza un controllo deterministico (Lattosio, Vegetariano, Vegano)", () => {
    expect(allergeniDalProfilo(["Lattosio", "Vegetariano", "Vegano"])).toEqual([]);
  });

  it("restituisce un array vuoto per null/undefined/vuoto", () => {
    expect(allergeniDalProfilo(null)).toEqual([]);
    expect(allergeniDalProfilo(undefined)).toEqual([]);
    expect(allergeniDalProfilo([])).toEqual([]);
  });

  it("riconosce tutti e quattro gli allergeni gestiti", () => {
    expect(allergeniDalProfilo(["Glutine (celiachia)", "Frutta a guscio", "Uova", "Pesce/Crostacei"])).toEqual([
      "glutine",
      "frutta_a_guscio",
      "uova",
      "pesce_crostacei",
    ]);
  });
});

describe("categorizzaIngrediente — glutine", () => {
  it("segnala 'non_adatto' per ingredienti con glutine senza ambiguità", () => {
    expect(categorizzaIngrediente("Pasta", ["glutine"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Farina di frumento", ["glutine"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Pane", ["glutine"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Farro", ["glutine"])).toBe("non_adatto");
  });

  it("segnala 'da_verificare' per ingredienti il cui glutine dipende dalla marca", () => {
    expect(categorizzaIngrediente("Salsa di soia", ["glutine"])).toBe("da_verificare");
    expect(categorizzaIngrediente("Dado vegetale", ["glutine"])).toBe("da_verificare");
    expect(categorizzaIngrediente("Besciamella", ["glutine"])).toBe("da_verificare");
    expect(categorizzaIngrediente("Avena", ["glutine"])).toBe("da_verificare");
  });

  it("segnala 'informazioni_sufficienti' per ingredienti chiaramente senza glutine", () => {
    expect(categorizzaIngrediente("Petto di pollo", ["glutine"])).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Pomodoro", ["glutine"])).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Riso", ["glutine"])).toBe("informazioni_sufficienti");
  });

  it("segnala 'informazioni_sufficienti' per una base alternativa intrinsecamente senza glutine", () => {
    expect(categorizzaIngrediente("Pasta di riso", ["glutine"])).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Farina di mais", ["glutine"])).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Cuscus di quinoa", ["glutine"])).toBe("informazioni_sufficienti");
  });

  it("segnala 'verificato' solo per una certificazione esplicita", () => {
    expect(categorizzaIngrediente("Pasta senza glutine", ["glutine"])).toBe("verificato");
    expect(categorizzaIngrediente("Pane gluten free", ["glutine"])).toBe("verificato");
  });

  it("i qualificatori hanno priorità sulle parole chiave di rischio", () => {
    expect(categorizzaIngrediente("Pasta di riso certificata", ["glutine"])).toBe("verificato");
  });

  it("non è sensibile a maiuscole/minuscole", () => {
    expect(categorizzaIngrediente("PASTA", ["glutine"])).toBe("non_adatto");
    expect(categorizzaIngrediente("fRuMeNtO", ["glutine"])).toBe("non_adatto");
  });
});

describe("categorizzaIngrediente — frutta a guscio", () => {
  it("segnala 'non_adatto' per frutta a guscio senza ambiguità", () => {
    expect(categorizzaIngrediente("Mandorle", ["frutta_a_guscio"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Granella di nocciole", ["frutta_a_guscio"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Burro di arachidi", ["frutta_a_guscio"])).toBe("non_adatto");
  });

  it("segnala 'da_verificare' per prodotti che spesso ne contengono", () => {
    expect(categorizzaIngrediente("Pesto alla genovese", ["frutta_a_guscio"])).toBe("da_verificare");
    expect(categorizzaIngrediente("Cioccolato fondente", ["frutta_a_guscio"])).toBe("da_verificare");
  });

  it("resta 'informazioni_sufficienti' per ingredienti non collegati", () => {
    expect(categorizzaIngrediente("Petto di pollo", ["frutta_a_guscio"])).toBe("informazioni_sufficienti");
    expect(categorizzaIngrediente("Riso", ["frutta_a_guscio"])).toBe("informazioni_sufficienti");
  });

  it("segnala 'verificato' per una certificazione esplicita", () => {
    expect(categorizzaIngrediente("Biscotti senza frutta a guscio", ["frutta_a_guscio"])).toBe("verificato");
  });
});

describe("categorizzaIngrediente — uova", () => {
  it("segnala 'non_adatto' per uova e derivati senza ambiguità", () => {
    expect(categorizzaIngrediente("Uova", ["uova"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Maionese", ["uova"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Crema pasticcera", ["uova"])).toBe("non_adatto");
  });

  it("segnala 'da_verificare' per prodotti che dipendono dalla ricetta", () => {
    expect(categorizzaIngrediente("Pasta fresca", ["uova"])).toBe("da_verificare");
    expect(categorizzaIngrediente("Pane", ["uova"])).toBe("da_verificare");
  });

  it("segnala 'verificato' per vegano o certificazione esplicita", () => {
    expect(categorizzaIngrediente("Maionese vegana", ["uova"])).toBe("verificato");
    expect(categorizzaIngrediente("Pasta senza uova", ["uova"])).toBe("verificato");
  });

  it("resta 'informazioni_sufficienti' per ingredienti non collegati", () => {
    expect(categorizzaIngrediente("Petto di pollo", ["uova"])).toBe("informazioni_sufficienti");
  });
});

describe("categorizzaIngrediente — pesce e crostacei", () => {
  it("segnala 'non_adatto' per pesce e crostacei senza ambiguità", () => {
    expect(categorizzaIngrediente("Salmone", ["pesce_crostacei"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Gamberi", ["pesce_crostacei"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Cozze", ["pesce_crostacei"])).toBe("non_adatto");
    expect(categorizzaIngrediente("Petto di pollo", ["pesce_crostacei"])).toBe("informazioni_sufficienti");
  });

  it("segnala 'da_verificare' per condimenti che spesso ne contengono", () => {
    expect(categorizzaIngrediente("Salsa worcester", ["pesce_crostacei"])).toBe("da_verificare");
  });

  it("segnala 'verificato' per vegetariano/vegano o certificazione esplicita", () => {
    expect(categorizzaIngrediente("Condimento vegetariano", ["pesce_crostacei"])).toBe("verificato");
  });
});

describe("categorizzaIngrediente — più allergeni insieme (profilo con più restrizioni)", () => {
  it("usa la categoria più severa tra tutti gli allergeni attivi", () => {
    // "Pasta" è non_adatto per glutine, informazioni_sufficienti per uova:
    // il risultato complessivo deve restare non_adatto (unione per sicurezza).
    expect(categorizzaIngrediente("Pasta", ["glutine", "uova"])).toBe("non_adatto");
    // "Mandorle" è non_adatto per frutta a guscio, ma informazioni_sufficienti per pesce.
    expect(categorizzaIngrediente("Mandorle", ["pesce_crostacei", "frutta_a_guscio"])).toBe("non_adatto");
  });

  it("un ingrediente sicuro per tutti gli allergeni attivi resta informazioni_sufficienti", () => {
    expect(categorizzaIngrediente("Petto di pollo", ["glutine", "uova", "pesce_crostacei", "frutta_a_guscio"])).toBe(
      "informazioni_sufficienti",
    );
  });

  it("restituisce informazioni_sufficienti se non ci sono allergeni attivi (nessuna restrizione gestita)", () => {
    expect(categorizzaIngrediente("Pasta", [])).toBe("informazioni_sufficienti");
  });
});

describe("ingredientiNonAdatti", () => {
  it("filtra solo gli ingredienti non adatto per l'allergene indicato, non quelli da verificare", () => {
    const risultato = ingredientiNonAdatti(["Pollo", "Pasta", "Dado vegetale", "Pomodoro", "Farro"], ["glutine"]);
    expect(risultato).toEqual(["Pasta", "Farro"]);
  });

  it("unisce i risultati su più allergeni attivi insieme", () => {
    const risultato = ingredientiNonAdatti(["Pasta", "Mandorle", "Pollo", "Salmone"], ["glutine", "frutta_a_guscio"]);
    expect(risultato).toEqual(["Pasta", "Mandorle"]);
  });

  it("restituisce un array vuoto se nessun ingrediente è non adatto", () => {
    expect(ingredientiNonAdatti(["Riso", "Pollo", "Dado vegetale", "Pasta di riso"], ["glutine"])).toEqual([]);
  });
});

describe("ingredientiDaVerificare", () => {
  it("filtra solo gli ingredienti da verificare, non quelli non adatto", () => {
    const risultato = ingredientiDaVerificare(["Pollo", "Pasta", "Dado vegetale", "Salsa di soia"], ["glutine"]);
    expect(risultato).toEqual(["Dado vegetale", "Salsa di soia"]);
  });
});

describe("ingredientiDaSegnalare", () => {
  it("unisce non adatto e da verificare per un singolo allergene", () => {
    const risultato = ingredientiDaSegnalare(["Pollo", "Pasta", "Dado vegetale", "Pomodoro"], ["glutine"]);
    expect(risultato).toEqual(["Pasta", "Dado vegetale"]);
  });

  it("copre un pasto con rischi su allergeni diversi contemporaneamente (profilo celiaco + allergico a uova)", () => {
    const risultato = ingredientiDaSegnalare(["Pasta all'uovo", "Petto di pollo", "Riso"], ["glutine", "uova"]);
    expect(risultato).toEqual(["Pasta all'uovo"]);
  });

  it("restituisce un array vuoto se nessun ingrediente merita attenzione", () => {
    expect(ingredientiDaSegnalare(["Riso", "Pollo", "Pasta di riso"], ["glutine"])).toEqual([]);
  });
});
