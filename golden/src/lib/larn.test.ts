import { describe, it, expect } from "vitest";
import { calcolaRiferimentoLARN, confrontaConLARN, DISCLAIMER_LARN, type DatiBiometrici } from "./larn";

describe("calcolaRiferimentoLARN", () => {
  it("calcola un riferimento settimanale plausibile per un uomo adulto attivo", () => {
    const profilo: DatiBiometrici = {
      sesso: "M",
      eta: 35,
      peso_kg: 80,
      altezza_cm: 180,
      livello_attivita: "moderato",
    };

    const rif = calcolaRiferimentoLARN(profilo);

    // Mifflin-St Jeor: BMR = 10*80 + 6.25*180 - 5*35 + 5 = 1755; *1.55 (PAL moderato) = 2720.25/giorno
    const kcalGiornoAtteso = (10 * 80 + 6.25 * 180 - 5 * 35 + 5) * 1.55;
    expect(rif.calorie).toBe(Math.round(kcalGiornoAtteso * 7));

    // Valori settimanali nell'ordine di grandezza atteso per un adulto attivo.
    expect(rif.proteine_g).toBeGreaterThan(400);
    expect(rif.proteine_g).toBeLessThan(600);
    expect(rif.fibre_g).toBe(30 * 7); // uomo: 30g/giorno
  });

  it("usa un fabbisogno proteico più alto per gli over 65", () => {
    const base: Omit<DatiBiometrici, "eta"> = {
      sesso: "F",
      peso_kg: 60,
      altezza_cm: 165,
      livello_attivita: "sedentario",
    };

    const giovane = calcolaRiferimentoLARN({ ...base, eta: 40 });
    const anziana = calcolaRiferimentoLARN({ ...base, eta: 70 });

    // 0.9 g/kg sotto i 65 anni, 1.0 g/kg da 65 in su, a parità di peso.
    expect(anziana.proteine_g).toBeGreaterThan(giovane.proteine_g);
  });

  it("usa la fibra di riferimento femminile per le donne", () => {
    const rif = calcolaRiferimentoLARN({
      sesso: "F",
      eta: 30,
      peso_kg: 60,
      altezza_cm: 165,
      livello_attivita: "sedentario",
    });
    expect(rif.fibre_g).toBe(25 * 7);
  });

  it("un livello di attività più alto alza il fabbisogno calorico, a parità di tutto il resto", () => {
    const base = { sesso: "M" as const, eta: 30, peso_kg: 75, altezza_cm: 175 };
    const sedentario = calcolaRiferimentoLARN({ ...base, livello_attivita: "sedentario" });
    const attivo = calcolaRiferimentoLARN({ ...base, livello_attivita: "attivo" });
    expect(attivo.calorie).toBeGreaterThan(sedentario.calorie);
  });
});

describe("confrontaConLARN", () => {
  const riferimento = {
    calorie: 1000,
    proteine_g: 1000,
    carboidrati_g: 1000,
    grassi_g: 1000,
    fibre_g: 1000,
  };

  it("classifica come 'bassa' un totale sotto l'85% del riferimento", () => {
    const totali = { ...riferimento, proteine_g: 800 }; // 80%
    const confronto = confrontaConLARN(totali, riferimento);
    const proteine = confronto.find((n) => n.chiave === "proteine_g");
    expect(proteine?.fascia).toBe("bassa");
  });

  it("classifica come 'alta' un totale sopra il 115% del riferimento", () => {
    const totali = { ...riferimento, grassi_g: 1200 }; // 120%
    const confronto = confrontaConLARN(totali, riferimento);
    const grassi = confronto.find((n) => n.chiave === "grassi_g");
    expect(grassi?.fascia).toBe("alta");
  });

  it("classifica come 'media' un totale vicino al riferimento (tra 85% e 115%)", () => {
    const totali = { ...riferimento, carboidrati_g: 950 }; // 95%
    const confronto = confrontaConLARN(totali, riferimento);
    const carboidrati = confronto.find((n) => n.chiave === "carboidrati_g");
    expect(carboidrati?.fascia).toBe("media");
  });

  it("esattamente al 100% è sempre 'media'", () => {
    const confronto = confrontaConLARN(riferimento, riferimento);
    expect(confronto.every((n) => n.fascia === "media")).toBe(true);
  });

  it("riporta sempre totale e riferimento originali, non solo la fascia", () => {
    const totali = { ...riferimento, fibre_g: 42 };
    const confronto = confrontaConLARN(totali, riferimento);
    const fibre = confronto.find((n) => n.chiave === "fibre_g");
    expect(fibre?.totale).toBe(42);
    expect(fibre?.riferimento).toBe(1000);
  });

  it("include sempre tutti e 5 i nutrienti", () => {
    const confronto = confrontaConLARN(riferimento, riferimento);
    expect(confronto.map((n) => n.chiave).sort()).toEqual(
      ["calorie", "carboidrati_g", "fibre_g", "grassi_g", "proteine_g"].sort(),
    );
  });
});

describe("DISCLAIMER_LARN", () => {
  it("è presente e menziona che non sostituisce un professionista", () => {
    expect(DISCLAIMER_LARN).toMatch(/non sostituisce/i);
    expect(DISCLAIMER_LARN.length).toBeGreaterThan(0);
  });
});
