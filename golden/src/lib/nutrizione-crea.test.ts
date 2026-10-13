import { describe, it, expect } from "vitest";
import { cercaValoriCrea, calcolaNutrizionePasto, CITAZIONE_CREA } from "./nutrizione-crea";

describe("cercaValoriCrea", () => {
  it("trova la riga giusta per un nome ingrediente generico (minuscolo, senza accenti)", () => {
    expect(cercaValoriCrea("pollo")?.nome).toBe("Pollo, petto, crudo");
    expect(cercaValoriCrea("Petto di pollo")?.nome).toBe("Pollo, petto, crudo");
  });

  it("non combacia per sottostringa quando il prodotto reale è diverso (fagiolini vs fagioli)", () => {
    expect(cercaValoriCrea("Fagiolini")).toBeNull();
    expect(cercaValoriCrea("Fagioli cannellini")?.nome).toBe("Fagioli, secchi");
  });

  it("non combacia per sottostringa quando il prodotto reale è diverso (melanzana vs mela)", () => {
    expect(cercaValoriCrea("Melanzana")).toBeNull();
    expect(cercaValoriCrea("Mela")?.nome).toBe("Mele, fresche, golden");
  });

  it("esclude le varianti vegetali di latte/yogurt dal match sul prodotto animale", () => {
    expect(cercaValoriCrea("Latte di mandorla")).toBeNull();
    expect(cercaValoriCrea("Latte di soia")).toBeNull();
    expect(cercaValoriCrea("Latte intero")?.nome).toBe("Latte di vacca, pastorizzato, intero");
    expect(cercaValoriCrea("Yogurt di soia")).toBeNull();
    expect(cercaValoriCrea("Yogurt greco")).toBeNull();
  });

  it("esclude la patata dolce dal match sulla patata comune", () => {
    expect(cercaValoriCrea("Patate dolci")).toBeNull();
    expect(cercaValoriCrea("Patate")?.nome).toBe("Patate, crude");
  });

  it("esclude il burro di arachidi dal match sul burro", () => {
    expect(cercaValoriCrea("Burro di arachidi")).toBeNull();
    expect(cercaValoriCrea("Burro")?.nome).toBe("Burro");
  });

  it("l'aglio resta volutamente non coperto", () => {
    expect(cercaValoriCrea("Aglio")).toBeNull();
  });

  it("restituisce null per un ingrediente non in tabella", () => {
    expect(cercaValoriCrea("Curcuma")).toBeNull();
  });
});

describe("calcolaNutrizionePasto", () => {
  const stimaAI = { calorie: 999, proteine_g: 1, carboidrati_g: 1, grassi_g: 1, fibre_g: 1 };

  it("usa il dato CREA/INRAN quando tutti gli ingredienti sono coperti ed espressi in g", () => {
    const { nutrizione, fonte } = calcolaNutrizionePasto(
      [
        { nome: "Petto di pollo", quantita: 200, unita: "g" },
        { nome: "Riso", quantita: 100, unita: "g" },
      ],
      stimaAI,
    );

    expect(fonte).toBe("crea_inran");
    // Pollo 200g: 200 kcal, 46.6g proteine. Riso 100g: 334 kcal, 6.7g proteine.
    expect(nutrizione.calorie).toBe(534);
    expect(nutrizione.proteine_g).toBe(53.3);
  });

  it("converte kg in grammi prima di scalare", () => {
    const { nutrizione, fonte } = calcolaNutrizionePasto([{ nome: "Riso", quantita: 1, unita: "kg" }], stimaAI);
    expect(fonte).toBe("crea_inran");
    expect(nutrizione.calorie).toBe(3340);
  });

  it("resta stima AI se anche un solo ingrediente non è in tabella — non mischia le fonti", () => {
    const { nutrizione, fonte } = calcolaNutrizionePasto(
      [
        { nome: "Petto di pollo", quantita: 200, unita: "g" },
        { nome: "Curcuma", quantita: 2, unita: "g" },
      ],
      stimaAI,
    );
    expect(fonte).toBe("stima_ai");
    expect(nutrizione).toEqual(stimaAI);
  });

  it("resta stima AI se un ingrediente coperto è espresso in un'unità non convertibile in grammi", () => {
    const { fonte } = calcolaNutrizionePasto([{ nome: "Olio", quantita: 1, unita: "confezione" }], stimaAI);
    expect(fonte).toBe("stima_ai");
  });

  it("resta stima AI per un piatto senza ingredienti", () => {
    const { fonte, nutrizione } = calcolaNutrizionePasto([], stimaAI);
    expect(fonte).toBe("stima_ai");
    expect(nutrizione).toEqual(stimaAI);
  });
});

describe("CITAZIONE_CREA", () => {
  it("contiene l'attribuzione richiesta", () => {
    expect(CITAZIONE_CREA).toContain("CREA/INRAN");
  });
});
