import { describe, it, expect } from "vitest";
import {
  leggiDispensa,
  applicaConsumiDispensa,
  dispensaSenzaVersione,
  sostituisciConsumiDispensa,
  elencoDispensa,
  istruzioneDispensa,
} from "./dispensa";
import type { SupabaseClient } from "@supabase/supabase-js";

// Client Supabase finto, minimo indispensabile per imitare le catene usate
// da dispensa.ts (.from().select().eq() in lettura; .from().upsert() e
// .from().delete().eq().eq().eq() in scrittura), senza toccare una vera
// istanza Supabase.
function fakeSupabaseLettura(righe: Record<string, unknown>[]) {
  const builder = {
    select: () => builder,
    eq: () => builder,
    then: (resolve: (v: { data: Record<string, unknown>[] }) => void) => resolve({ data: righe }),
  };
  return { from: () => builder } as unknown as SupabaseClient;
}

function fakeSupabaseScrittura() {
  const upserts: Record<string, unknown>[] = [];
  const deletes: Record<string, unknown>[] = [];

  function queryBuilder() {
    return {
      upsert: (row: Record<string, unknown>) => {
        upserts.push(row);
        return Promise.resolve({ error: null });
      },
      delete: () => {
        const filtri: Record<string, unknown> = {};
        const chain = {
          eq: (colonna: string, valore: unknown) => {
            filtri[colonna] = valore;
            return chain;
          },
          then: (resolve: (v: { error: null }) => void) => {
            deletes.push({ ...filtri });
            resolve({ error: null });
          },
        };
        return chain;
      },
    };
  }

  const client = { from: () => queryBuilder() } as unknown as SupabaseClient;
  return { client, upserts, deletes };
}

describe("leggiDispensa", () => {
  it("costruisce la mappa nome+unità -> quantità dalle righe di 'rimanenze'", async () => {
    const supabase = fakeSupabaseLettura([
      { ingrediente: "Petto di pollo", unita: "g", quantita: "200.00" },
      { ingrediente: "Riso", unita: "g", quantita: "150" },
    ]);

    const dispensa = await leggiDispensa(supabase, "profilo-1");

    expect(dispensa.get("petto di pollo__g")).toBe(200);
    expect(dispensa.get("riso__g")).toBe(150);
  });

  it("restituisce una mappa vuota se non ci sono righe", async () => {
    const supabase = fakeSupabaseLettura([]);
    const dispensa = await leggiDispensa(supabase, "profilo-1");
    expect(dispensa.size).toBe(0);
  });
});

describe("applicaConsumiDispensa", () => {
  it("sottrae i consumi e aggiunge gli avanzi al saldo esistente", async () => {
    const { client, upserts } = fakeSupabaseScrittura();
    const dispensaIniziale = new Map([["petto di pollo__g", 300]]);

    await applicaConsumiDispensa(
      client,
      "profilo-1",
      dispensaIniziale,
      [{ nome: "Petto di pollo", unita: "g", quantita: 300 }],
      [{ nome: "Petto di pollo", unita: "g", quantita: 150, quantitaNecessaria: 350 }],
      "2026-10-05",
    );

    // 300 (saldo) - 300 (consumato) + 150 (nuovo avanzo) = 150
    expect(upserts).toEqual([
      { profile_id: "profilo-1", ingrediente: "Petto di pollo", unita: "g", quantita: 150, settimana: "2026-10-05" },
    ]);
  });

  it("elimina la riga quando il saldo risultante è zero o negativo", async () => {
    const { client, upserts, deletes } = fakeSupabaseScrittura();
    const dispensaIniziale = new Map([["riso__g", 200]]);

    await applicaConsumiDispensa(
      client,
      "profilo-1",
      dispensaIniziale,
      [{ nome: "Riso", unita: "g", quantita: 200 }],
      [],
      "2026-10-05",
    );

    expect(upserts).toEqual([]);
    expect(deletes).toEqual([{ profile_id: "profilo-1", ingrediente: "Riso", unita: "g" }]);
  });

  it("un ingrediente mai visto prima parte da saldo zero", async () => {
    const { client, upserts } = fakeSupabaseScrittura();

    await applicaConsumiDispensa(
      client,
      "profilo-1",
      new Map(),
      [],
      [{ nome: "Spinaci surgelati", unita: "g", quantita: 400, quantitaNecessaria: 350 }],
      "2026-10-05",
    );

    expect(upserts).toEqual([
      { profile_id: "profilo-1", ingrediente: "Spinaci surgelati", unita: "g", quantita: 400, settimana: "2026-10-05" },
    ]);
  });
});

describe("dispensaSenzaVersione", () => {
  it("ricostruisce la dispensa 'vera' annullando l'effetto di una versione già applicata", () => {
    // Scenario reale verificato con l'utente: 550g di riso erano un vero
    // avanzo di settimane precedenti; la generazione originale di questa
    // settimana ne ha consumati 400g senza comprarne altro (saldo -> 150g).
    const dispensaAttuale = new Map([["riso__g", 150]]);
    const vecchiConsumi = [{ nome: "Riso", unita: "g" as const, quantita: 400 }];
    const vecchioRimasto: { nome: string; unita: "g"; quantita: number; quantitaNecessaria: number }[] = [];

    const base = dispensaSenzaVersione(dispensaAttuale, vecchiConsumi, vecchioRimasto);

    expect(base.get("riso__g")).toBe(550);
  });

  it("non modifica la mappa originale (ritorna una copia)", () => {
    const dispensaAttuale = new Map([["riso__g", 100]]);
    dispensaSenzaVersione(dispensaAttuale, [{ nome: "Riso", unita: "g", quantita: 50 }], []);
    expect(dispensaAttuale.get("riso__g")).toBe(100);
  });
});

describe("sostituisciConsumiDispensa", () => {
  it("annulla la versione precedente e applica quella nuova senza contare due volte", async () => {
    const { client, upserts } = fakeSupabaseScrittura();

    // Stesso scenario cross-settimana verificato manualmente durante lo
    // sviluppo: 550g di riso erano un vero avanzo precedente; la versione
    // originale di questa settimana ne ha consumati 400 (saldo 150). Una
    // modifica porta il fabbisogno a 600g: 550 disponibili, 50 da comprare,
    // confezione 1000 -> 950g di nuovo avanzo.
    const dispensaAttuale = new Map([["riso__g", 150]]);

    await sostituisciConsumiDispensa(
      client,
      "profilo-1",
      dispensaAttuale,
      [{ nome: "Riso", unita: "g", quantita: 400 }], // vecchi consumi
      [], // vecchio rimasto
      [{ nome: "Riso", unita: "g", quantita: 550 }], // nuovi consumi
      [{ nome: "Riso", unita: "g", quantita: 950, quantitaNecessaria: 50 }], // nuovo rimasto
      "2026-10-05",
    );

    expect(upserts).toEqual([
      { profile_id: "profilo-1", ingrediente: "Riso", unita: "g", quantita: 950, settimana: "2026-10-05" },
    ]);
  });

  it("se la versione non cambia nulla, il saldo finale resta identico a quello iniziale", async () => {
    const { client, upserts, deletes } = fakeSupabaseScrittura();
    const dispensaAttuale = new Map([["pollo__g", 200]]);
    const consumi = [{ nome: "Pollo", unita: "g" as const, quantita: 300 }];
    const rimasto = [{ nome: "Pollo", unita: "g" as const, quantita: 200, quantitaNecessaria: 300 }];

    await sostituisciConsumiDispensa(client, "profilo-1", dispensaAttuale, consumi, rimasto, consumi, rimasto, "2026-10-05");

    expect(upserts).toEqual([
      { profile_id: "profilo-1", ingrediente: "Pollo", unita: "g", quantita: 200, settimana: "2026-10-05" },
    ]);
    expect(deletes).toEqual([]);
  });
});

describe("elencoDispensa", () => {
  it("ricostruisce nome/unità/quantità dalle chiavi 'nome__unita', in ordine alfabetico", () => {
    const dispensa = new Map([
      ["riso__g", 670],
      ["spinaci__g", 450],
      ["uova__pz", 1],
    ]);

    expect(elencoDispensa(dispensa)).toEqual([
      { nome: "riso", unita: "g", quantita: 670 },
      { nome: "spinaci", unita: "g", quantita: 450 },
      { nome: "uova", unita: "pz", quantita: 1 },
    ]);
  });

  it("esclude le voci a saldo zero o negativo", () => {
    const dispensa = new Map([
      ["riso__g", 0],
      ["spinaci__g", -5],
      ["uova__pz", 2],
    ]);

    expect(elencoDispensa(dispensa)).toEqual([{ nome: "uova", unita: "pz", quantita: 2 }]);
  });

  it("restituisce un array vuoto per una dispensa vuota", () => {
    expect(elencoDispensa(new Map())).toEqual([]);
  });
});

describe("istruzioneDispensa", () => {
  it("elenca gli ingredienti avanzati e chiede di usarli attivamente", () => {
    const dispensa = new Map([
      ["riso__g", 670],
      ["spinaci__g", 450],
    ]);

    const testo = istruzioneDispensa(dispensa);

    expect(testo).toContain("riso (670g)");
    expect(testo).toContain("spinaci (450g)");
    expect(testo).toMatch(/usali attivamente/i);
  });

  it("ricorda che resta un criterio subordinato a restrizioni, sicurezza e budget", () => {
    const testo = istruzioneDispensa(new Map([["riso__g", 670]]));
    expect(testo).toMatch(/restrizioni alimentari, sicurezza e budget/i);
  });

  it("restituisce una stringa vuota per una dispensa vuota (nessun avanzo da segnalare)", () => {
    expect(istruzioneDispensa(new Map())).toBe("");
  });
});
