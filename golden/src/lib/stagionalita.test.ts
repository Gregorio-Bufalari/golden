import { describe, it, expect } from "vitest";
import { mesiDiStagione, isDiStagione, prodottiDiStagione, istruzioneStagionalita } from "./stagionalita";

describe("mesiDiStagione", () => {
  it("restituisce null per un ingrediente non in tabella (es. non frutta/verdura, o tropicale sempre disponibile)", () => {
    expect(mesiDiStagione("Petto di pollo")).toBeNull();
    expect(mesiDiStagione("Banana")).toBeNull();
  });

  it("restituisce i mesi per un ingrediente riconosciuto", () => {
    expect(mesiDiStagione("Zucchine")).toEqual([5, 6, 7, 8, 9]);
  });

  it("non è sensibile a maiuscole/minuscole", () => {
    expect(mesiDiStagione("ZUCCHINE")).toEqual([5, 6, 7, 8, 9]);
  });
});

describe("isDiStagione", () => {
  it("true quando il mese indicato è tra i mesi di stagione", () => {
    expect(isDiStagione("Pomodoro", 7)).toBe(true);
  });

  it("false quando il mese indicato non è tra i mesi di stagione", () => {
    expect(isDiStagione("Pomodoro", 1)).toBe(false);
  });

  it("false per un ingrediente non riconosciuto, in qualsiasi mese", () => {
    expect(isDiStagione("Petto di pollo", 7)).toBe(false);
  });

  it("riconosce un prodotto a cavallo di fine/inizio anno (es. Kiwi, di stagione anche a dicembre e gennaio)", () => {
    expect(isDiStagione("Kiwi", 12)).toBe(true);
    expect(isDiStagione("Kiwi", 1)).toBe(true);
    expect(isDiStagione("Kiwi", 7)).toBe(false);
  });
});

describe("prodottiDiStagione", () => {
  it("include prodotti tipicamente estivi a luglio", () => {
    const prodotti = prodottiDiStagione(7);
    expect(prodotti).toContain("Pomodori");
    expect(prodotti).toContain("Zucchine");
    expect(prodotti).toContain("Meloni");
  });

  it("include prodotti tipicamente invernali a gennaio, non quelli estivi", () => {
    const prodotti = prodottiDiStagione(1);
    expect(prodotti).toContain("Agrumi (arance, mandarini, clementine)");
    expect(prodotti).not.toContain("Pomodori");
    expect(prodotti).not.toContain("Meloni");
  });

  it("ogni mese dell'anno ha almeno un prodotto di stagione", () => {
    for (let mese = 1; mese <= 12; mese++) {
      expect(prodottiDiStagione(mese).length, `mese ${mese}`).toBeGreaterThan(0);
    }
  });
});

describe("istruzioneStagionalita", () => {
  it("nomina il mese per intero e l'elenco dei prodotti di stagione", () => {
    const testo = istruzioneStagionalita(7);
    expect(testo).toContain("luglio");
    expect(testo).toContain("Pomodori");
  });

  it("ricorda sempre che restrizioni e sicurezza vengono prima della stagionalità", () => {
    const testo = istruzioneStagionalita(7);
    expect(testo).toMatch(/restrizioni.*vincolo più alto/i);
  });

  it("usa il mese corrente quando non viene passato nessun argomento", () => {
    const meseCorrente = new Date().getMonth() + 1;
    expect(istruzioneStagionalita()).toBe(istruzioneStagionalita(meseCorrente));
  });
});
