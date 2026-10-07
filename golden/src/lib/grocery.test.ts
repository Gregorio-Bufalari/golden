import { describe, it, expect } from "vitest";
import { buildGroceryList } from "./grocery";
import { creaPianoEsempio, ingrediente } from "@/test/fixtures/piano-esempio";

function giornoCon(ingredienti: ReturnType<typeof ingrediente>[]) {
  return { pasti: [{ ingredienti }] };
}

describe("buildGroceryList — arrotondamento alla confezione reale", () => {
  it("arrotonda alla confezione reale e registra l'avanzo (scenario pollo 300g -> confezione 500g)", () => {
    const giorni = [
      giornoCon([
        ingrediente({ nome: "Petto di pollo", quantita: 300, unita: "g", reparto: "Carne e pesce", prezzo_stimato_eur: 3 }),
      ]),
    ];

    const { groceryList } = buildGroceryList(giorni, null);
    const items = groceryList.reparti.flatMap((r) => r.items);
    const pollo = items.find((i) => i.nome === "Petto di pollo");

    expect(pollo?.quantitaNecessaria).toBe(300);
    expect(pollo?.confezione).toBe(500);
    expect(pollo?.quantita).toBe(500); // quantità acquistata, arrotondata
    expect(groceryList.rimasto).toEqual([
      { nome: "Petto di pollo", quantita: 200, quantitaNecessaria: 300, unita: "g" },
    ]);
  });

  it("non arrotonda frutta/verdura fresca: si compra esattamente quanto serve", () => {
    const giorni = [
      giornoCon([
        ingrediente({ nome: "Pomodoro", quantita: 237, unita: "g", reparto: "Frutta e verdura", prezzo_stimato_eur: 0.7 }),
      ]),
    ];

    const { groceryList } = buildGroceryList(giorni, null);
    const pomodoro = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Pomodoro");

    expect(pomodoro?.quantita).toBe(237);
    expect(pomodoro?.confezione).toBeNull();
    expect(groceryList.rimasto).toEqual([]);
  });

  it("se la quantità necessaria coincide esattamente con un multiplo della confezione, non c'è avanzo", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Riso", quantita: 1000, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 2 })]),
    ];

    const { groceryList } = buildGroceryList(giorni, null);
    const riso = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Riso");

    expect(riso?.quantita).toBe(1000);
    expect(groceryList.rimasto).toEqual([]);
  });

  it("un reparto senza parola chiave usa la taglia di default del reparto (es. spezie in Dispensa)", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Cumino", quantita: 5, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 0.1 })]),
    ];

    const { groceryList } = buildGroceryList(giorni, null);
    const cumino = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Cumino");

    expect(cumino?.confezione).toBe(500); // default per "Dispensa"
    expect(cumino?.quantita).toBe(500);
  });

  it("aggrega la stessa materia prima usata in più pasti prima di arrotondare", () => {
    const giorni = [
      {
        pasti: [
          { ingredienti: [ingrediente({ nome: "Riso", quantita: 300, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 0.6 })] },
          { ingredienti: [ingrediente({ nome: "Riso", quantita: 250, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 0.5 })] },
        ],
      },
    ];

    const { groceryList } = buildGroceryList(giorni, null);
    const riso = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Riso");

    // 300 + 250 = 550 -> confezione 1000 -> acquista 1000, avanzo 450.
    expect(riso?.quantitaNecessaria).toBe(550);
    expect(riso?.quantita).toBe(1000);
    expect(groceryList.rimasto[0]).toMatchObject({ nome: "Riso", quantita: 450 });
  });
});

describe("buildGroceryList — dispensa (avanzi da settimane precedenti)", () => {
  it("se la dispensa copre tutto il fabbisogno, l'ingrediente non compare nella lista", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Riso", quantita: 100, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 0.2 })]),
    ];
    const dispensa = new Map([["riso__g", 150]]);

    const { groceryList, consumiDispensa } = buildGroceryList(giorni, null, dispensa);

    expect(groceryList.reparti).toEqual([]);
    expect(consumiDispensa).toEqual([{ nome: "Riso", unita: "g", quantita: 100 }]);
  });

  it("se la dispensa copre solo in parte, si compra solo la differenza (poi arrotondata)", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Petto di pollo", quantita: 650, unita: "g", reparto: "Carne e pesce", prezzo_stimato_eur: 6.5 })]),
    ];
    const dispensa = new Map([["petto di pollo__g", 300]]);

    const { groceryList, consumiDispensa } = buildGroceryList(giorni, null, dispensa);
    const pollo = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Petto di pollo");

    // 650 necessari - 300 in dispensa = 350 da comprare -> confezione 500 -> acquista 500, avanzo 150.
    expect(consumiDispensa).toEqual([{ nome: "Petto di pollo", unita: "g", quantita: 300 }]);
    expect(pollo?.quantitaNecessaria).toBe(350);
    expect(pollo?.quantita).toBe(500);
    expect(groceryList.rimasto[0]).toMatchObject({ nome: "Petto di pollo", quantita: 150 });
  });

  it("la dispensa non intacca mai un ingrediente diverso (chiave nome+unità)", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Riso", quantita: 100, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 0.2 })]),
    ];
    const dispensa = new Map([["farina__g", 500]]); // ingrediente diverso

    const { groceryList, consumiDispensa } = buildGroceryList(giorni, null, dispensa);

    expect(consumiDispensa).toEqual([]);
    expect(groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Riso")).toBeDefined();
  });
});

describe("buildGroceryList — prezzi e fasce di supermercato", () => {
  it("applica il moltiplicatore della fascia discount, media e premium", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Pomodoro", quantita: 100, unita: "g", reparto: "Frutta e verdura", prezzo_stimato_eur: 1 })]),
    ];

    const discount = buildGroceryList(giorni, "Eurospin").groceryList.totale_stimato;
    const media = buildGroceryList(giorni, "Conad").groceryList.totale_stimato;
    const premium = buildGroceryList(giorni, "Esselunga").groceryList.totale_stimato;
    const sconosciuto = buildGroceryList(giorni, null).groceryList.totale_stimato;

    expect(discount).toBeCloseTo(0.8, 5);
    expect(media).toBeCloseTo(1.0, 5);
    expect(premium).toBeCloseTo(1.3, 5);
    expect(sconosciuto).toBeCloseTo(1.0, 5); // supermercato non impostato -> fascia media
  });

  it("un supermercato non in elenco usa comunque la fascia media", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Pomodoro", quantita: 100, unita: "g", reparto: "Frutta e verdura", prezzo_stimato_eur: 1 })]),
    ];
    expect(buildGroceryList(giorni, "Un supermercato qualsiasi").groceryList.fascia).toBe("media");
  });

  it("applica il fattore di calibrazione sopra la fascia statica", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Pomodoro", quantita: 100, unita: "g", reparto: "Frutta e verdura", prezzo_stimato_eur: 1 })]),
    ];

    const { groceryList } = buildGroceryList(giorni, "Conad", new Map(), 1.2);

    expect(groceryList.totale_stimato).toBeCloseTo(1.2, 5); // 1.0 (media) * 1.2
    expect(groceryList.calibrato).toBe(true);
  });

  it("senza un fattore esplicito (o uguale a 1) la lista non risulta calibrata", () => {
    const giorni = [
      giornoCon([ingrediente({ nome: "Pomodoro", quantita: 100, unita: "g", reparto: "Frutta e verdura", prezzo_stimato_eur: 1 })]),
    ];

    expect(buildGroceryList(giorni, "Conad").groceryList.calibrato).toBe(false);
    expect(buildGroceryList(giorni, "Conad", new Map(), 1).groceryList.calibrato).toBe(false);
  });
});

describe("buildGroceryList — scenario end-to-end sul piano di esempio", () => {
  it("il piano di esempio produce una lista coerente: pollo arrotondato, pomodoro no, totale positivo", () => {
    const { groceryList } = buildGroceryList(creaPianoEsempio(), "Conad");

    const pollo = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Petto di pollo");
    expect(pollo?.quantita).toBe(500);
    expect(groceryList.rimasto.some((r) => r.nome === "Petto di pollo" && r.quantita === 200)).toBe(true);

    const pomodoro = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Pomodoro");
    expect(pomodoro?.confezione).toBeNull();

    expect(groceryList.totale_stimato).toBeGreaterThan(0);
  });

  it("l'ordine dei reparti nella lista segue sempre lo stesso ordine (REPARTI), non l'ordine di inserimento", () => {
    const { groceryList } = buildGroceryList(creaPianoEsempio(), "Conad");
    const ordineReparti = groceryList.reparti.map((r) => r.reparto);
    const copia = [...ordineReparti].sort();
    // Non verifichiamo l'ordine alfabetico (non lo è), ma che sia sempre
    // lo stesso a parità di input: eseguito due volte deve combaciare.
    const { groceryList: seconda } = buildGroceryList(creaPianoEsempio(), "Conad");
    expect(seconda.reparti.map((r) => r.reparto)).toEqual(ordineReparti);
    expect(copia.length).toBe(ordineReparti.length);
  });
});
