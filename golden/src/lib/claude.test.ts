import { describe, it, expect, vi, beforeEach } from "vitest";
import { creaPianoEsempio, pasto, ingrediente } from "@/test/fixtures/piano-esempio";
import { generateMealPlan, modificaPiano, confrontaScontrino, type ProfiloPerPiano, type Giorno } from "./claude";
import { buildGroceryList } from "./grocery";

// Il client Anthropic reale non viene mai istanziato in questi test: si
// sostituisce l'intero SDK con una classe finta il cui .messages.parse()
// restituisce una risposta "simulata" decisa da ogni test, così la
// generazione/modifica del piano si può verificare senza una chiave API
// né una chiamata di rete reale. vi.hoisted serve perché vi.mock viene
// issato sopra ogni import/const del file. Serve una vera "function" (non
// una arrow function) perché claude.ts fa `new Anthropic()`.
//
// generateMealPlan/modificaPiano/adattaBudget usano client.messages.stream
// (non .parse, vedi commento in claude.ts), ma .finalMessage() ha la stessa
// forma del risultato di .parse(): il finto .stream() delega subito a
// mockParse, così ogni test continua a configurare la risposta con
// mockParse.mockResolvedValueOnce(...) indipendentemente da quale dei due
// venga chiamato dalla funzione sotto test.
const { mockParse } = vi.hoisted(() => ({ mockParse: vi.fn() }));

vi.mock("@anthropic-ai/sdk", () => ({
  default: vi.fn().mockImplementation(function FakeAnthropic() {
    return {
      messages: {
        parse: mockParse,
        stream: (...args: unknown[]) => ({ finalMessage: () => mockParse(...args) }),
      },
    };
  }),
}));

const profiloBase: ProfiloPerPiano = {
  restrizioni: ["Glutine (celiachia)"],
  obiettivo: null,
  preferenze: null,
  tempo_max_cucina: null,
  household_size: 2,
  budget_settimanale: null,
};

beforeEach(() => {
  mockParse.mockReset();
});

describe("generateMealPlan — risposta AI simulata", () => {
  it("restituisce il piano quando l'AI risponde con un output valido", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    const risultato = await generateMealPlan(profiloBase);

    expect(risultato.giorni).toHaveLength(7);
    expect(mockParse).toHaveBeenCalledTimes(1);
  });

  it("lancia un errore chiaro se il parsing dell'output fallisce (parsed_output nullo)", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: null });

    await expect(generateMealPlan(profiloBase)).rejects.toThrow("Claude non ha restituito un piano valido.");
  });

  it("in modalità Scoperta aggiunge l'istruzione di varietà al prompt", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase, "scoperta");

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toContain("modalità 'Scoperta'");
  });

  it("in modalità Routine (default) non aggiunge l'istruzione di varietà", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).not.toContain("modalità 'Scoperta'");
  });

  it("in modalità Scoperta, con piatti recenti passati, chiede di evitarli", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase, "scoperta", new Map(), ["Pollo al forno", "Riso al salmone"]);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toContain("Pollo al forno");
    expect(richiesta.system).toContain("Riso al salmone");
    expect(richiesta.system).toMatch(/settimane recenti/i);
  });

  it("in modalità Routine, anche con piatti recenti passati, NON li menziona (Routine ripete apposta)", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase, "routine", new Map(), ["Pollo al forno"]);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).not.toContain("Pollo al forno");
  });

  it("in modalità Scoperta, senza piatti recenti (prima generazione), non menziona lo storico", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase, "scoperta");

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).not.toMatch(/settimane recenti/i);
  });

  it("include sempre le restrizioni del profilo nel messaggio inviato all'AI", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase);

    const richiesta = mockParse.mock.calls[0][0];
    const testoMessaggio = richiesta.messages[0].content as string;
    expect(testoMessaggio).toContain("Glutine (celiachia)");
  });

  it("senza obiettivi nutrizionali per pasto impostati, li segnala come non specificati", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase);

    const richiesta = mockParse.mock.calls[0][0];
    const testoMessaggio = richiesta.messages[0].content as string;
    expect(testoMessaggio).toMatch(/Obiettivi nutrizionali per pasto.*non specificati/);
  });

  it("include nel messaggio solo gli obiettivi nutrizionali per pasto effettivamente impostati", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan({
      ...profiloBase,
      obiettivi_nutrizionali: { calorie_min: null, calorie_max: 700, proteine_min_g: 30, carboidrati_max_g: null, grassi_max_g: null },
    });

    const richiesta = mockParse.mock.calls[0][0];
    const testoMessaggio = richiesta.messages[0].content as string;
    // Il target per pasto impostato (700 kcal, 30g proteine) è trattato come
    // un totale giornaliero implicito e ridistribuito 25%/35%/40% su
    // colazione/pranzo/cena (vedi FRAZIONE_OBIETTIVO_PER_PASTO in claude.ts).
    expect(testoMessaggio).toContain("al massimo 350 kcal");
    expect(testoMessaggio).toContain("almeno 15g di proteine");
    expect(testoMessaggio).toContain("al massimo 490 kcal");
    expect(testoMessaggio).toContain("almeno 21g di proteine");
    expect(testoMessaggio).toContain("al massimo 560 kcal");
    expect(testoMessaggio).toContain("almeno 24g di proteine");
    expect(testoMessaggio).not.toContain("carboidrati");
  });

  it("nel prompt di generazione, gli obiettivi nutrizionali per pasto hanno la stessa priorità dell'obiettivo generale", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/obiettivi nutrizionali per pasto.*stessa priorità/i);
  });

  it("include nel prompt il criterio di stagionalità per frutta e verdura, con restrizioni/budget sempre sopra", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/di stagione/i);
    expect(richiesta.system).toMatch(/restrizioni.*vincolo più alto/i);
  });

  it("senza dispensa passata non menziona ingredienti avanzati nel prompt", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).not.toMatch(/avanzati in dispensa/i);
  });

  it("con una dispensa passata, chiede di usare attivamente gli ingredienti avanzati", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    const dispensa = new Map([
      ["spinaci__g", 450],
      ["riso__g", 670],
    ]);
    await generateMealPlan(profiloBase, "routine", dispensa);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/avanzati in dispensa/i);
    expect(richiesta.system).toContain("spinaci (450g)");
    expect(richiesta.system).toContain("riso (670g)");
  });

  it("include l'istruzione specifica per la colazione e chiede colazione+pranzo+cena per 7 giorni", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan(profiloBase);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/piatti tipici da colazione/i);

    const testoMessaggio = richiesta.messages[0].content as string;
    expect(testoMessaggio).toMatch(/colazione, pranzo e cena per ognuno dei 7 giorni/i);
  });

  it("con obiettivi nutrizionali impostati, il messaggio elenca il target ridistribuito per i tre pasti (25%/35%/40%)", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: { giorni: creaPianoEsempio() } });

    await generateMealPlan({
      ...profiloBase,
      obiettivi_nutrizionali: { calorie_min: null, calorie_max: 700, proteine_min_g: 30, carboidrati_max_g: null, grassi_max_g: null },
    });

    const richiesta = mockParse.mock.calls[0][0];
    const testoMessaggio = richiesta.messages[0].content as string;
    expect(testoMessaggio).toMatch(/colazione \(circa 25%.*pranzo \(circa 35%.*cena \(circa 40%/i);
  });
});

describe("modificaPiano — risposta AI simulata", () => {
  it("applica la modifica quando l'AI la accetta", async () => {
    mockParse.mockResolvedValueOnce({
      // Solo il pasto toccato, non l'intero piano (vedi ModificaOutputSchema
      // in claude.ts) — modificaPiano ricompone da sé il piano completo.
      parsed_output: {
        modifica_applicata: true,
        motivo_rifiuto: null,
        pasti_modificati: [{ giorno: "Lunedì", pasto: pasto({ tipo: "pranzo", nome: "Riso al pollo", ingredienti: [] }) }],
      },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const risultato = await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    expect(risultato.modificaApplicata).toBe(true);
    expect(risultato.motivoRifiuto).toBeNull();
    expect(risultato.giorni).toHaveLength(7);
    expect(risultato.costoStimatoUsd).toBeGreaterThan(0);
  });

  it("rifiuta la modifica e riporta il motivo quando è incompatibile con le restrizioni", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: {
        modifica_applicata: false,
        motivo_rifiuto: "Non posso aggiungere pasta di grano: contiene glutine.",
        pasti_modificati: [],
      },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const risultato = await modificaPiano(profiloBase, creaPianoEsempio(), "aggiungi pasta al forno");

    expect(risultato.modificaApplicata).toBe(false);
    expect(risultato.motivoRifiuto).toMatch(/glutine/i);
    // Nessun pasto nel piano cambia quando la modifica è rifiutata.
    expect(risultato.giorni).toEqual(creaPianoEsempio());
  });

  it("lancia un errore chiaro se il parsing dell'output fallisce", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: null });

    await expect(
      modificaPiano(profiloBase, creaPianoEsempio(), "qualsiasi richiesta"),
    ).rejects.toThrow("Claude non ha restituito un piano valido.");
  });

  it("include anche qui il criterio di stagionalità nel prompt", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, pasti_modificati: [] },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/di stagione/i);
  });

  it("include anche qui il vincolo di varietà, per non reintrodurre un doppione modificando un pasto", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, pasti_modificati: [] },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/21 ricette DISTINTE/);
  });

  it("include anche qui l'istruzione specifica per la colazione e il vincolo di 21 pasti fissi", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, pasti_modificati: [] },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/piatti tipici da colazione/i);
    expect(richiesta.system).toMatch(/21 pasti fissi/);
  });

  it("include anche qui gli ingredienti avanzati in dispensa, quando passati (es. pulsante \"Proponine un altro\")", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, pasti_modificati: [] },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const dispensa = new Map([["uova__pz", 1]]);
    await modificaPiano(profiloBase, creaPianoEsempio(), "proponi un piatto diverso per Lunedì pranzo", dispensa);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toContain("uova (1pz)");
  });

  it("senza cronologia passata, non menziona richieste precedenti nel messaggio", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, pasti_modificati: [] },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    const richiesta = mockParse.mock.calls[0][0];
    const testoMessaggio = richiesta.messages[0].content as string;
    expect(testoMessaggio).not.toMatch(/Richieste di modifica/i);
  });

  it("con una cronologia passata, include le richieste precedenti e il loro esito nel messaggio", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, pasti_modificati: [] },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "fallo anche per cena", new Map(), [
      { messaggio: "metti più proteine a pranzo", applicata: true },
      { messaggio: "aggiungi pasta di grano", applicata: false },
    ]);

    const richiesta = mockParse.mock.calls[0][0];
    const testoMessaggio = richiesta.messages[0].content as string;
    expect(testoMessaggio).toContain('"metti più proteine a pranzo" → applicata');
    expect(testoMessaggio).toContain('"aggiungi pasta di grano" → rifiutata');
  });

  it("restituisce i token usati e il costo stimato della chiamata", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, pasti_modificati: [] },
      usage: { input_tokens: 2000, output_tokens: 1000 },
    });

    const risultato = await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    expect(risultato.usage.input_tokens).toBe(2000);
    expect(risultato.usage.output_tokens).toBe(1000);
    // claude-sonnet-5-5: $2/1M input, $10/1M output -> 2000*2/1e6 + 1000*10/1e6
    expect(risultato.costoStimatoUsd).toBeCloseTo(0.004 + 0.01, 6);
  });

  // Scenario "ho già comprato [ingrediente]" (14 ottobre 2026): verificato dal
  // vivo che modificaApplicata torna true ma l'ingrediente resta nella lista
  // della spesa ricostruita. Questi due test isolano DOVE sta il
  // comportamento: non c'è nessuna chiamata reale qui (AI simulata), quindi
  // non possono dimostrare cosa fa l'AI vera — dimostrano solo che il
  // ricalcolo deterministico della lista (buildGroceryList) è corretto dato
  // un output di modificaPiano, in entrambi i casi possibili.
  it("se l'AI toglie l'ingrediente da un SOLO piatto condiviso, la lista riflette il fabbisogno residuo (non zero, non il totale originale)", async () => {
    const giorniPrima: Giorno[] = [
      {
        giorno: "Lunedì",
        pasti: [
          pasto({
            tipo: "pranzo",
            nome: "Pollo al forno",
            ingredienti: [ingrediente({ nome: "Petto di pollo", quantita: 300, unita: "g", reparto: "Carne e pesce", prezzo_stimato_eur: 3 })],
          }),
        ],
      },
      {
        giorno: "Mercoledì",
        pasti: [
          pasto({
            tipo: "cena",
            nome: "Pollo alla griglia",
            ingredienti: [ingrediente({ nome: "Petto di pollo", quantita: 400, unita: "g", reparto: "Carne e pesce", prezzo_stimato_eur: 4 })],
          }),
        ],
      },
    ];

    // Risposta AI simulata plausibile: modifica SOLO il piatto di lunedì
    // (quello più "ovvio" nel contesto della richiesta), lascia mercoledì
    // fuori da pasti_modificati — il prompt chiede di restituire solo i
    // pasti davvero cambiati, senza istruzione esplicita a cercare
    // l'ingrediente in TUTTI i pasti della settimana. modificaPiano ricompone
    // da sé il piano completo, tenendo mercoledì invariato da giorniPrima.
    mockParse.mockResolvedValueOnce({
      parsed_output: {
        modifica_applicata: true,
        motivo_rifiuto: null,
        pasti_modificati: [
          {
            giorno: "Lunedì",
            pasto: pasto({
              tipo: "pranzo",
              nome: "Tofu saltato",
              ingredienti: [ingrediente({ nome: "Tofu", quantita: 300, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 2 })],
            }),
          },
        ],
      },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const risultato = await modificaPiano(profiloBase, giorniPrima, "ho già comprato il pollo");
    expect(risultato.modificaApplicata).toBe(true);

    const { groceryList } = buildGroceryList(risultato.giorni, null);
    const pollo = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Petto di pollo");

    // Il pollo resta in lista: serve ancora per il piatto di mercoledì non
    // toccato dalla modifica — comportamento CORRETTO del ricalcolo
    // deterministico (rifletterebbe fedelmente l'ingrediente ancora
    // richiesto), non il bug segnalato dal 14 ottobre.
    expect(pollo).toBeDefined();
    expect(pollo?.quantitaNecessaria).toBe(400); // solo mercoledì, non più 700 (300+400) né 0
  });

  it("se l'AI toglie l'ingrediente da TUTTI i piatti che lo usano, sparisce correttamente dalla lista", async () => {
    const giorniPrima: Giorno[] = [
      {
        giorno: "Lunedì",
        pasti: [
          pasto({
            tipo: "pranzo",
            nome: "Pollo al forno",
            ingredienti: [ingrediente({ nome: "Petto di pollo", quantita: 300, unita: "g", reparto: "Carne e pesce", prezzo_stimato_eur: 3 })],
          }),
        ],
      },
      {
        giorno: "Mercoledì",
        pasti: [
          pasto({
            tipo: "cena",
            nome: "Pollo alla griglia",
            ingredienti: [ingrediente({ nome: "Petto di pollo", quantita: 400, unita: "g", reparto: "Carne e pesce", prezzo_stimato_eur: 4 })],
          }),
        ],
      },
    ];

    mockParse.mockResolvedValueOnce({
      parsed_output: {
        modifica_applicata: true,
        motivo_rifiuto: null,
        pasti_modificati: [
          {
            giorno: "Lunedì",
            pasto: pasto({
              tipo: "pranzo",
              nome: "Tofu saltato",
              ingredienti: [ingrediente({ nome: "Tofu", quantita: 300, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 2 })],
            }),
          },
          {
            giorno: "Mercoledì",
            pasto: pasto({
              tipo: "cena",
              nome: "Tempeh alla griglia",
              ingredienti: [ingrediente({ nome: "Tempeh", quantita: 400, unita: "g", reparto: "Dispensa", prezzo_stimato_eur: 3 })],
            }),
          },
        ],
      },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const risultato = await modificaPiano(profiloBase, giorniPrima, "ho già comprato il pollo");
    const { groceryList } = buildGroceryList(risultato.giorni, null);
    const pollo = groceryList.reparti.flatMap((r) => r.items).find((i) => i.nome === "Petto di pollo");

    expect(pollo).toBeUndefined();
  });

  // Verifica mirata del merge pasti_modificati -> piano completo (6 novembre
  // 2026, dopo aver trovato due regressioni reali in conservazione.ts con lo
  // stesso approccio "ricontrolla i casi limite su dati sintetici").
  it("un pasto_modificato tocca SOLO il giorno/pasto indicato: tutti gli altri 13 pasti restano byte-per-byte identici", async () => {
    // creaPianoEsempio() copre tutti e 7 i giorni (14 pasti, pranzo+cena):
    // un merge corretto deve toccare ESATTAMENTE 1 pasto su 14.
    mockParse.mockResolvedValueOnce({
      parsed_output: {
        modifica_applicata: true,
        motivo_rifiuto: null,
        pasti_modificati: [{ giorno: "Venerdì", pasto: pasto({ tipo: "pranzo", nome: "Nuovo piatto di Venerdì", ingredienti: [] }) }],
      },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const pianoOriginale = creaPianoEsempio();
    const risultato = await modificaPiano(profiloBase, pianoOriginale, "qualsiasi richiesta");

    const venerdì = risultato.giorni.find((g) => g.giorno === "Venerdì");
    expect(venerdì?.pasti.find((p) => p.tipo === "pranzo")?.nome).toBe("Nuovo piatto di Venerdì");

    const altriGiorniInvariati = risultato.giorni.every((g, i) => {
      if (g.giorno === "Venerdì") return true;
      return JSON.stringify(g) === JSON.stringify(pianoOriginale[i]);
    });
    const cenaVenerdìInvariata =
      venerdì?.pasti.find((p) => p.tipo === "cena")?.nome ===
      pianoOriginale.find((g) => g.giorno === "Venerdì")?.pasti.find((p) => p.tipo === "cena")?.nome;
    expect(altriGiorniInvariati).toBe(true);
    expect(cenaVenerdìInvariata).toBe(true);
  });

  it("due pasti_modificati per lo STESSO giorno+tipo: vince il primo, nessun crash", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: {
        modifica_applicata: true,
        motivo_rifiuto: null,
        pasti_modificati: [
          { giorno: "Lunedì", pasto: pasto({ tipo: "pranzo", nome: "Primo candidato", ingredienti: [] }) },
          { giorno: "Lunedì", pasto: pasto({ tipo: "pranzo", nome: "Secondo candidato", ingredienti: [] }) },
        ],
      },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const risultato = await modificaPiano(profiloBase, creaPianoEsempio(), "qualsiasi richiesta");
    const lunedì = risultato.giorni.find((g) => g.giorno === "Lunedì");
    const pranzo = lunedì?.pasti.find((p) => p.tipo === "pranzo");

    expect(pranzo?.nome).toBe("Primo candidato");
  });

  it("modifica_applicata true ma pasti_modificati vuoto: il piano resta invariato, nessun crash (anomalia AI, non un errore di merge)", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, pasti_modificati: [] },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const pianoOriginale = creaPianoEsempio();
    const risultato = await modificaPiano(profiloBase, pianoOriginale, "qualsiasi richiesta");

    expect(risultato.modificaApplicata).toBe(true);
    expect(risultato.giorni).toEqual(pianoOriginale);
  });
});

describe("confrontaScontrino — risposta AI simulata", () => {
  const listaSpesa = [
    { nome: "Petto di pollo", prezzo_stimato_eur: 4.5 },
    { nome: "Pasta di riso", prezzo_stimato_eur: 2.1 },
  ];

  it("invia l'immagine e la lista della spesa nel messaggio", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: {
        totale_scontrino_eur: 6.6,
        leggibile: true,
        corrispondenze: [],
        extra_non_in_lista: [],
      },
      usage: { input_tokens: 1500, output_tokens: 300 },
    });

    await confrontaScontrino("ZmFrZS1pbWFnZQ==", "image/jpeg", listaSpesa);

    const richiesta = mockParse.mock.calls[0][0];
    const content = richiesta.messages[0].content as { type: string; text?: string; [k: string]: unknown }[];
    const bloccoImmagine = content.find((b) => b.type === "image");
    const bloccoTesto = content.find((b) => b.type === "text");

    expect(bloccoImmagine).toMatchObject({ source: { type: "base64", media_type: "image/jpeg", data: "ZmFrZS1pbWFnZQ==" } });
    expect(bloccoTesto?.text).toContain("Petto di pollo");
  });

  it("restituisce le corrispondenze trovate e i prodotti extra", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: {
        totale_scontrino_eur: 6.6,
        leggibile: true,
        corrispondenze: [
          {
            nome_lista: "Petto di pollo",
            prezzo_stimato_eur: 4.5,
            trovato_sullo_scontrino: true,
            nome_scontrino: "POLLO PETTO",
            prezzo_scontrino_eur: 4.8,
          },
          {
            nome_lista: "Pasta di riso",
            prezzo_stimato_eur: 2.1,
            trovato_sullo_scontrino: false,
            nome_scontrino: null,
            prezzo_scontrino_eur: null,
          },
        ],
        extra_non_in_lista: [{ nome: "Acqua minerale", prezzo_eur: 0.5 }],
      },
      usage: { input_tokens: 1500, output_tokens: 300 },
    });

    const risultato = await confrontaScontrino("ZmFrZS1pbWFnZQ==", "image/jpeg", listaSpesa);

    expect(risultato.leggibile).toBe(true);
    expect(risultato.corrispondenze[0].trovato_sullo_scontrino).toBe(true);
    expect(risultato.corrispondenze[0].prezzo_scontrino_eur).toBe(4.8);
    expect(risultato.corrispondenze[1].trovato_sullo_scontrino).toBe(false);
    expect(risultato.extra_non_in_lista).toEqual([{ nome: "Acqua minerale", prezzo_eur: 0.5 }]);
    expect(risultato.costoStimatoUsd).toBeGreaterThan(0);
  });

  it("segnala leggibile a false senza inventare un totale quando la foto non è leggibile", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: {
        totale_scontrino_eur: null,
        leggibile: false,
        corrispondenze: [],
        extra_non_in_lista: [],
      },
      usage: { input_tokens: 1200, output_tokens: 100 },
    });

    const risultato = await confrontaScontrino("ZmFrZS1pbWFnZQ==", "image/jpeg", listaSpesa);

    expect(risultato.leggibile).toBe(false);
    expect(risultato.totale_scontrino_eur).toBeNull();
  });

  it("lancia un errore chiaro se il parsing dell'output fallisce", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: null });

    await expect(confrontaScontrino("ZmFrZS1pbWFnZQ==", "image/jpeg", listaSpesa)).rejects.toThrow(
      "Claude non ha restituito un confronto valido.",
    );
  });
});
