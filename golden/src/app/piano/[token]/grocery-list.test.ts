import { describe, it, expect } from "vitest";
import { riepilogoSostituzione } from "./grocery-list";

type GroceryReparto = Parameters<typeof riepilogoSostituzione>[0][number];

function reparto(nome: string, nomiItem: string[]): GroceryReparto {
  return {
    reparto: nome,
    subtotale: 0,
    items: nomiItem.map((n) => ({
      nome: n,
      quantita: 1,
      quantitaNecessaria: 1,
      confezione: null,
      unita: "pz",
      prezzo_stimato: 1,
    })),
  };
}

describe("riepilogoSostituzione", () => {
  it("segnala il nuovo prodotto quando la sostituzione aggiunge un nome nuovo", () => {
    const prima = [reparto("Pasta e cereali", ["Pasta di grano"])];
    const dopo = [reparto("Pasta e cereali", ["Pasta di riso"])];

    expect(riepilogoSostituzione(prima, dopo, "Pasta di grano")).toBe(
      "Pasta di grano sostituito con: Pasta di riso.",
    );
  });

  it("segnala la rimozione quando il prodotto sparisce senza un nome nuovo distinguibile", () => {
    // Es. l'AI ha spostato l'uso su un ingrediente già presente altrove
    // nella lista, quindi nessun nome nuovo appare.
    const prima = [reparto("Frutta e verdura", ["Pomodoro", "Zucchine"])];
    const dopo = [reparto("Frutta e verdura", ["Zucchine"])];

    expect(riepilogoSostituzione(prima, dopo, "Pomodoro")).toBe(
      "Pomodoro non è più nella lista — il piano è stato aggiornato.",
    );
  });

  it("usa un messaggio generico quando il prodotto resta (es. solo la quantità è cambiata)", () => {
    const prima = [reparto("Frigo", ["Mozzarella"])];
    const dopo = [reparto("Frigo", ["Mozzarella"])];

    expect(riepilogoSostituzione(prima, dopo, "Mozzarella")).toBe(
      "Il piano è stato aggiornato per Mozzarella.",
    );
  });
});
