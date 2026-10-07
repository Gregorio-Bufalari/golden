import { describe, it, expect } from "vitest";
import { sceglieFavoritoScoperta, includiFavoritoNelPiano, type PreferitoPerRotazione } from "./preferiti-scoperta";
import type { Giorno, Pasto } from "./claude";

function creaPreferito(overrides: Partial<PreferitoPerRotazione> = {}): PreferitoPerRotazione {
  return {
    id: "pref-1",
    nome: "Pollo al limone",
    tipo: "cena",
    ingredienti: [],
    tempo_preparazione_min: 25,
    nutrizione: { calorie: 450, proteine_g: 35, carboidrati_g: 20, grassi_g: 15, fibre_g: 3 },
    preparazione: ["Cuoci il pollo", "Aggiungi il limone"],
    ultima_proposta: null,
    ...overrides,
  };
}

function creaPasto(overrides: Partial<Pasto> = {}): Pasto {
  return {
    tipo: "cena",
    nome: "Pasta al pomodoro",
    ingredienti: [],
    tempo_preparazione_min: 20,
    nutrizione: { calorie: 500, proteine_g: 15, carboidrati_g: 80, grassi_g: 10, fibre_g: 4 },
    preparazione: ["Cuoci la pasta"],
    ...overrides,
  };
}

function creaGiorni(): Giorno[] {
  const giorniSettimana = ["Lunedì", "Martedì", "Mercoledì", "Giovedì", "Venerdì", "Sabato", "Domenica"] as const;
  return giorniSettimana.map((giorno) => ({
    giorno,
    pasti: [creaPasto({ tipo: "pranzo", nome: `Pranzo ${giorno}` }), creaPasto({ tipo: "cena", nome: `Cena ${giorno}` })],
  }));
}

describe("sceglieFavoritoScoperta", () => {
  it("restituisce null se non ci sono preferiti", () => {
    expect(sceglieFavoritoScoperta([], () => 0)).toBeNull();
  });

  it("restituisce null quando il sorteggio non capita questa settimana", () => {
    const preferiti = [creaPreferito()];
    expect(sceglieFavoritoScoperta(preferiti, () => 0.9)).toBeNull();
  });

  it("restituisce un preferito quando il sorteggio capita", () => {
    const preferiti = [creaPreferito()];
    expect(sceglieFavoritoScoperta(preferiti, () => 0)).toEqual(preferiti[0]);
  });

  it("a rotazione, sceglie il preferito mai riproposto prima di uno già riproposto", () => {
    const giaRiproposto = creaPreferito({ id: "a", nome: "A", ultima_proposta: "2026-01-01T00:00:00Z" });
    const maiRiproposto = creaPreferito({ id: "b", nome: "B", ultima_proposta: null });

    const scelto = sceglieFavoritoScoperta([giaRiproposto, maiRiproposto], () => 0);
    expect(scelto?.id).toBe("b");
  });

  it("a rotazione, tra due già riproposti sceglie quello riproposto meno di recente", () => {
    const recente = creaPreferito({ id: "a", nome: "A", ultima_proposta: "2026-02-01T00:00:00Z" });
    const meno_recente = creaPreferito({ id: "b", nome: "B", ultima_proposta: "2026-01-01T00:00:00Z" });

    const scelto = sceglieFavoritoScoperta([recente, meno_recente], () => 0);
    expect(scelto?.id).toBe("b");
  });
});

describe("includiFavoritoNelPiano", () => {
  it("sostituisce il primo pasto dello stesso tipo del preferito con l'istantanea salvata", () => {
    const giorni = creaGiorni();
    const favorito = creaPreferito({ tipo: "cena", nome: "Pollo al limone" });

    const risultato = includiFavoritoNelPiano(giorni, favorito);

    expect(risultato[0].pasti.find((p) => p.tipo === "cena")?.nome).toBe("Pollo al limone");
  });

  it("sostituisce un solo pasto nell'intera settimana, non uno per ogni giorno", () => {
    const giorni = creaGiorni();
    const favorito = creaPreferito({ tipo: "cena", nome: "Pollo al limone" });

    const risultato = includiFavoritoNelPiano(giorni, favorito);

    const occorrenze = risultato.flatMap((g) => g.pasti).filter((p) => p.nome === "Pollo al limone");
    expect(occorrenze).toHaveLength(1);
  });

  it("lascia invariati tutti gli altri pasti della settimana", () => {
    const giorni = creaGiorni();
    const favorito = creaPreferito({ tipo: "cena", nome: "Pollo al limone" });

    const risultato = includiFavoritoNelPiano(giorni, favorito);

    expect(risultato[0].pasti.find((p) => p.tipo === "pranzo")?.nome).toBe("Pranzo Lunedì");
    for (let i = 1; i < risultato.length; i++) {
      expect(risultato[i]).toEqual(giorni[i]);
    }
  });
});
