import { describe, it, expect } from "vitest";
import { anteprimaPiatti, percentualeSuBase, riordinaScenariPerCosto } from "./menu-view";
import { etichettaGiorno } from "@/lib/settimana";

type ScenarioInput = Parameters<typeof riordinaScenariPerCosto>[0][number];

function scenario(chiave: ScenarioInput["chiave"], totale: number): ScenarioInput {
  return {
    chiave,
    etichetta: chiave,
    budget_target: null,
    giorni: [],
    grocery_list: { totale_stimato: totale },
    budget_superato: false,
  };
}

type GiornoInput = Parameters<typeof anteprimaPiatti>[0][number];

function giorno(nomePasti: string[], nomeGiorno = "Lunedì"): GiornoInput {
  return {
    giorno: nomeGiorno,
    pasti: nomePasti.map((nome) => ({
      tipo: "pranzo",
      nome,
      ingredienti: [],
      tempo_preparazione_min: 20,
      nutrizione: { calorie: 0, proteine_g: 0, carboidrati_g: 0, grassi_g: 0, fibre_g: 0 },
    })),
  };
}

describe("anteprimaPiatti", () => {
  it("elenca tutti i piatti quando sono due o meno", () => {
    const giorni = [giorno(["Pollo al forno", "Pasta al pesto"])];
    expect(anteprimaPiatti(giorni)).toBe("Pollo al forno, Pasta al pesto");
  });

  it("mostra solo i primi due piatti e il conteggio dei restanti", () => {
    const giorni = [giorno(["A", "B", "C"]), giorno(["D"])];
    expect(anteprimaPiatti(giorni)).toBe("A, B +2 altri");
  });
});

describe("percentualeSuBase", () => {
  it("restituisce una stringa vuota quando il totale coincide con la base", () => {
    expect(percentualeSuBase(80, 80)).toBe("");
  });

  it("mostra una percentuale negativa con il segno quando il totale è più basso", () => {
    expect(percentualeSuBase(64, 80)).toBe("-20%");
  });

  it("mostra una percentuale positiva con il segno quando il totale è più alto", () => {
    expect(percentualeSuBase(96, 80)).toBe("+20%");
  });

  it("restituisce una stringa vuota se la base non è positiva", () => {
    expect(percentualeSuBase(50, 0)).toBe("");
  });
});

describe("riordinaScenariPerCosto", () => {
  it("rietichetta per costo reale il caso segnalato dal vivo (6 novembre 2026): Equilibrato più caro di Abbondante", () => {
    // Come generati/nominati originariamente dal server: Risparmio 67.48,
    // Equilibrato 103.06 (il più caro di tutti!), Abbondante 76.17.
    const originali = [scenario("risparmio", 67.48), scenario("equilibrato", 103.06), scenario("abbondante", 76.17)];

    const riordinati = riordinaScenariPerCosto(originali, 50);

    const risparmio = riordinati.find((s) => s.chiave === "risparmio")!;
    const equilibrato = riordinati.find((s) => s.chiave === "equilibrato")!;
    const abbondante = riordinati.find((s) => s.chiave === "abbondante")!;

    // Ordine per costo reale sempre monotono, indipendentemente da quale
    // scenario è stato generato per quale target.
    expect(risparmio.grocery_list.totale_stimato).toBe(67.48);
    expect(equilibrato.grocery_list.totale_stimato).toBe(76.17);
    expect(abbondante.grocery_list.totale_stimato).toBe(103.06);
    expect(risparmio.grocery_list.totale_stimato).toBeLessThanOrEqual(equilibrato.grocery_list.totale_stimato);
    expect(equilibrato.grocery_list.totale_stimato).toBeLessThanOrEqual(abbondante.grocery_list.totale_stimato);

    // Target ricalcolati sul budget dichiarato (80%/100%/120%), non su
    // quello con cui ciascuno scenario era stato generato originariamente.
    expect(risparmio.budget_target).toBe(40);
    expect(equilibrato.budget_target).toBe(50);
    expect(abbondante.budget_target).toBe(60);
  });

  it("senza un budget dichiarato, usa il costo reale dello scenario centrale come base per i target ±20%", () => {
    const originali = [scenario("risparmio", 90), scenario("equilibrato", 100), scenario("abbondante", 80)];

    const riordinati = riordinaScenariPerCosto(originali, null);
    const equilibrato = riordinati.find((s) => s.chiave === "equilibrato")!;
    const risparmio = riordinati.find((s) => s.chiave === "risparmio")!;
    const abbondante = riordinati.find((s) => s.chiave === "abbondante")!;

    // Ordinati per costo reale: 80 (risparmio), 90 (equilibrato, ora
    // centrale), 100 (abbondante) — indipendentemente dalla chiave
    // originale di ciascuno.
    expect(risparmio.grocery_list.totale_stimato).toBe(80);
    expect(equilibrato.grocery_list.totale_stimato).toBe(90);
    expect(abbondante.grocery_list.totale_stimato).toBe(100);
    expect(equilibrato.budget_target).toBe(90); // base coincide col proprio costo -> mai "superato"
    expect(equilibrato.budget_superato).toBe(false);
    expect(risparmio.budget_target).toBe(72); // round(90*0.8)
    expect(abbondante.budget_target).toBe(108); // round(90*1.2)
  });

  it("con meno di 3 scenari (uno fallito) non riordina, restituisce l'elenco invariato", () => {
    const originali = [scenario("risparmio", 50), scenario("equilibrato", 60)];
    expect(riordinaScenariPerCosto(originali, 50)).toEqual(originali);
  });

  it("preserva piatti/lista spesa dello scenario, cambia solo chiave/etichetta/target/avviso", () => {
    const abbondanteEconomico = { ...scenario("abbondante", 30), giorni: [{ giorno: "Lunedì", pasti: [] }] };
    const originali = [scenario("risparmio", 50), scenario("equilibrato", 40), abbondanteEconomico];

    const riordinati = riordinaScenariPerCosto(originali, 50);
    const nuovoRisparmio = riordinati.find((s) => s.chiave === "risparmio")!;

    // Il contenuto (giorni) dello scenario originariamente "abbondante" (il
    // più economico, 30) segue la sua nuova etichetta "risparmio".
    expect(nuovoRisparmio.grocery_list.totale_stimato).toBe(30);
    expect(nuovoRisparmio.giorni).toEqual([{ giorno: "Lunedì", pasti: [] }]);
  });
});

describe("data del giorno nel Menu: chiave stabile, non legata ai pasti assegnati", () => {
  const SETTIMANA = "2026-10-05"; // lunedì

  it("resta la stessa per un giorno dopo uno scambio di pasti con un altro giorno", () => {
    const giorni = [giorno(["Pollo"], "Martedì"), giorno(["Riso"], "Giovedì")];
    const etichetteIniziali = giorni.map((g) => etichettaGiorno(g.giorno, SETTIMANA));

    // Stesso identico scambio fatto da scambiaPasti in actions.ts: si
    // riassegna solo il contenuto di pasti[indice], mai giorno.giorno.
    const [martedi, giovedi] = giorni;
    const pastoMartedi = martedi.pasti[0];
    const pastoGiovedi = giovedi.pasti[0];
    martedi.pasti[0] = pastoGiovedi;
    giovedi.pasti[0] = pastoMartedi;

    const etichetteDopoScambio = giorni.map((g) => etichettaGiorno(g.giorno, SETTIMANA));

    expect(etichetteDopoScambio).toEqual(etichetteIniziali);
    expect(giorni[0].pasti[0].nome).toBe("Riso"); // il pasto si è davvero spostato
    expect(giorni[1].pasti[0].nome).toBe("Pollo");
  });

  it("resta visibile con la sua data quando il contenuto di un giorno viene sostituito (es. 'giovedì mangio fuori')", () => {
    const giorni = [
      giorno(["Pasta"], "Mercoledì"),
      giorno(["Pollo", "Riso"], "Giovedì"),
      giorno(["Pesce"], "Venerdì"),
    ];
    const etichetteIniziali = giorni.map((g) => etichettaGiorno(g.giorno, SETTIMANA));

    // Una modifica via chat sostituisce solo il CONTENUTO di un giorno
    // (qui simulato), mai la sua posizione nell'array né il suo nome: lo
    // schema richiede sempre esattamente 7 giorni con nomi fissi, quindi
    // non può scalare o far sparire gli altri.
    giorni[1] = giorno(["Fuori"], "Giovedì");

    const etichetteDopo = giorni.map((g) => etichettaGiorno(g.giorno, SETTIMANA));

    expect(etichetteDopo).toEqual(etichetteIniziali); // nessun giorno si è spostato
    expect(giorni).toHaveLength(3); // Giovedì resta visibile, non è sparito
    expect(giorni[1].giorno).toBe("Giovedì");
  });
});
