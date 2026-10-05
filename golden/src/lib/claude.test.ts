import { describe, it, expect, vi, beforeEach } from "vitest";
import { creaPianoEsempio } from "@/test/fixtures/piano-esempio";
import { generateMealPlan, modificaPiano, type ProfiloPerPiano } from "./claude";

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
    });

    const risultato = await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    expect(risultato.modificaApplicata).toBe(true);
    expect(risultato.motivoRifiuto).toBeNull();
    expect(risultato.giorni).toHaveLength(7);
  });

  it("rifiuta la modifica e riporta il motivo quando è incompatibile con le restrizioni", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: {
        modifica_applicata: false,
        motivo_rifiuto: "Non posso aggiungere pasta di grano: contiene glutine.",
        giorni: creaPianoEsempio(),
      },
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
    });

    await modificaPiano(profiloBase, creaPianoEsempio(), "ho già comprato il pollo");

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toMatch(/di stagione/i);
  });

  it("include anche qui gli ingredienti avanzati in dispensa, quando passati (es. pulsante \"Proponine un altro\")", async () => {
    mockParse.mockResolvedValueOnce({
      parsed_output: { modifica_applicata: true, motivo_rifiuto: null, giorni: creaPianoEsempio() },
    });

    const dispensa = new Map([["uova__pz", 1]]);
    await modificaPiano(profiloBase, creaPianoEsempio(), "proponi un piatto diverso per Lunedì pranzo", dispensa);

    const richiesta = mockParse.mock.calls[0][0];
    expect(richiesta.system).toContain("uova (1pz)");
  });
});
