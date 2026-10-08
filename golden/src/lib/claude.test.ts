import { describe, it, expect, vi, beforeEach } from "vitest";
import { creaPianoEsempio } from "@/test/fixtures/piano-esempio";
import { generateMealPlan, modificaPiano, confrontaScontrino, type ProfiloPerPiano } from "./claude";

// Il client Anthropic reale non viene mai istanziato in questi test: si
// sostituisce l'intero SDK con una classe finta il cui .messages.parse()
// restituisce una risposta "simulata" decisa da ogni test, così la
// generazione/modifica del piano si può verificare senza una chiave API
// né una chiamata di rete reale. vi.hoisted serve perché vi.mock viene
// issato sopra ogni import/const del file. Serve una vera "function" (non
// una arrow function) perché claude.ts fa `new Anthropic()`.
const { mockParse } = vi.hoisted(() => ({ mockParse: vi.fn() }));

vi.mock("@anthropic-ai/sdk", () => ({
  default: vi.fn().mockImplementation(function FakeAnthropic() {
    return { messages: { parse: mockParse } };
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
    expect(testoMessaggio).toContain("al massimo 700 kcal");
    expect(testoMessaggio).toContain("almeno 30g di proteine");
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
});

describe("modificaPiano — risposta AI simulata", () => {
  it("applica la modifica quando l'AI la accetta", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, giorni: creaPianoEsempio() },
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
        giorni: creaPianoEsempio(),
      },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const risultato = await modificaPiano(profiloBase, creaPianoEsempio(), "aggiungi pasta al forno");

    expect(risultato.modificaApplicata).toBe(false);
    expect(risultato.motivoRifiuto).toMatch(/glutine/i);
  });

  it("lancia un errore chiaro se il parsing dell'output fallisce", async () => {
    mockParse.mockResolvedValueOnce({ parsed_output: null });

    await expect(
      modificaPiano(profiloBase, creaPianoEsempio(), "qualsiasi richiesta"),
    ).rejects.toThrow("Claude non ha restituito un piano valido.");
  });

  it("include anche qui il criterio di stagionalità nel prompt", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, giorni: creaPianoEsempio() },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/di stagione/i);
  });

  it("include anche qui il vincolo di varietà, per non reintrodurre un doppione modificando un pasto", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, giorni: creaPianoEsempio() },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/14 ricette DISTINTE/);
  });

  it("include anche qui gli ingredienti avanzati in dispensa, quando passati (es. pulsante \"Proponine un altro\")", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, giorni: creaPianoEsempio() },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    const dispensa = new Map([["uova__pz", 1]]);
    await modificaPiano(profiloBase, creaPianoEsempio(), "proponi un piatto diverso per Lunedì pranzo", dispensa);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toContain("uova (1pz)");
  });

  it("senza cronologia passata, non menziona richieste precedenti nel messaggio", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, giorni: creaPianoEsempio() },
      usage: { input_tokens: 1000, output_tokens: 500 },
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    const richiesta = mockParse.mock.calls[0][0];
    const testoMessaggio = richiesta.messages[0].content as string;
    expect(testoMessaggio).not.toMatch(/Richieste di modifica/i);
  });

  it("con una cronologia passata, include le richieste precedenti e il loro esito nel messaggio", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, giorni: creaPianoEsempio() },
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
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, giorni: creaPianoEsempio() },
      usage: { input_tokens: 2000, output_tokens: 1000 },
    });

    const risultato = await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    expect(risultato.usage.input_tokens).toBe(2000);
    expect(risultato.usage.output_tokens).toBe(1000);
    // claude-sonnet-5-5: $2/1M input, $10/1M output -> 2000*2/1e6 + 1000*10/1e6
    expect(risultato.costoStimatoUsd).toBeCloseTo(0.004 + 0.01, 6);
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
