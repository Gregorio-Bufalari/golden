import { describe, it, expect, vi, beforeEach } from "vitest";
import { creaPianoEsempio, ingrediente, pasto } from "@/test/fixtures/piano-esempio";
import type { ProfiloPerPiano, Pasto } from "@/lib/claude";
import { validaGiorni, assicuraVarieta, adattaEntroBudget } from "./piano-validazione";

// Mock dell'intero modulo claude.ts: questi test verificano che la pipeline
// di validazione (controllo glutine, adattamento al budget) reagisca
// correttamente a quello che l'AI potrebbe restituire, SENZA mai chiamare
// l'API Anthropic reale né istanziare il client (che richiederebbe una
// chiave API). Le risposte dell'AI sono simulate qui sotto. vi.hoisted
// serve perché vi.mock viene issato sopra ogni import/const del file.
const { mockRegeneratePasto, mockAdattaBudget } = vi.hoisted(() => ({
  mockRegeneratePasto: vi.fn(),
  mockAdattaBudget: vi.fn(),
}));

vi.mock("@/lib/claude", async (importOriginal) => {
  // REPARTI è un valore reale usato anche da grocery.ts: va preservato,
  // solo le funzioni che chiamano l'AI vengono sostituite.
  const originale = await importOriginal<typeof import("@/lib/claude")>();
  return {
    ...originale,
    regeneratePasto: mockRegeneratePasto,
    adattaBudget: mockAdattaBudget,
  };
});

const profiloCeliaco: ProfiloPerPiano = {
  restrizioni: ["Glutine (celiachia)"],
  obiettivo: null,
  preferenze: null,
  tempo_max_cucina: null,
  household_size: 2,
  budget_settimanale: null,
};

const profiloSenzaRestrizioni: ProfiloPerPiano = {
  ...profiloCeliaco,
  restrizioni: [],
};

function pastoRiso(): Pasto {
  // Sostituto sicuro per il pasto a rischio "Pasta al pomodoro" del piano
  // di esempio: stessa forma, ingrediente senza glutine.
  return pasto({
    tipo: "pranzo",
    nome: "Riso al pomodoro",
    ingredienti: [
      ingrediente({ nome: "Riso", quantita: 100, unita: "g", reparto: "Dispensa" }),
      ingrediente({ nome: "Pomodoro", quantita: 200, unita: "g", reparto: "Frutta e verdura" }),
    ],
  });
}

beforeEach(() => {
  mockRegeneratePasto.mockReset();
  mockAdattaBudget.mockReset();
});

describe("validaGiorni — controllo glutine con rigenerazione simulata dall'AI", () => {
  it("non chiama mai l'AI se il profilo non ha restrizioni al glutine", async () => {
    const risultato = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());

    expect(mockRegeneratePasto).not.toHaveBeenCalled();
    // Il pasto a rischio ("Pasta al pomodoro") resta tale quale, non viene toccato.
    expect(risultato[0].pasti[0].nome).toBe("Pasta al pomodoro");
  });

  it("rigenera il pasto a rischio e lo sostituisce con la versione sicura simulata", async () => {
    mockRegeneratePasto.mockResolvedValueOnce(pastoRiso());

    const risultato = await validaGiorni(profiloCeliaco, creaPianoEsempio());
    const lunediPranzo = risultato[0].pasti[0];

    expect(mockRegeneratePasto).toHaveBeenCalledTimes(1);
    expect(lunediPranzo.nome).toBe("Riso al pomodoro");
    expect(lunediPranzo.verificare).toBeUndefined();

    // Gli altri 13 pasti, mai a rischio, non vengono toccati.
    const altriPasti = risultato.flatMap((g) => g.pasti).filter((p) => p !== lunediPranzo);
    expect(altriPasti).toHaveLength(13);
  });

  it("se l'AI continua a proporre glutine, segnala il pasto per la verifica manuale dopo i tentativi massimi", async () => {
    // L'AI simulata "sbaglia" sempre: ripropone un ingrediente a rischio.
    mockRegeneratePasto.mockResolvedValue(
      pasto({
        tipo: "pranzo",
        nome: "Pasta al pesto (tentativo fallito)",
        ingredienti: [ingrediente({ nome: "Pasta", quantita: 100, unita: "g", reparto: "Pane e cereali" })],
      }),
    );

    const risultato = await validaGiorni(profiloCeliaco, creaPianoEsempio());
    const lunediPranzo = risultato[0].pasti[0];

    // Al massimo 2 tentativi di rigenerazione (MAX_RIGENERAZIONI), non all'infinito.
    expect(mockRegeneratePasto).toHaveBeenCalledTimes(2);
    expect(lunediPranzo.verificare).toBe(true);
    expect(lunediPranzo.ingredienti_a_rischio).toContain("Pasta");
  });

  it("se la rigenerazione fallisce (errore di rete), il pasto viene comunque segnalato invece di far crashare tutto", async () => {
    mockRegeneratePasto.mockRejectedValue(new Error("rete non disponibile"));

    const risultato = await validaGiorni(profiloCeliaco, creaPianoEsempio());
    const lunediPranzo = risultato[0].pasti[0];

    expect(lunediPranzo.verificare).toBe(true);
    expect(lunediPranzo.nome).toBe("Pasta al pomodoro"); // resta il pasto originale, non sostituito
  });

  it("segnala un ingrediente 'da verificare' (es. dado vegetale) senza chiamare l'AI per rigenerarlo", async () => {
    const giorni = [
      {
        giorno: "Lunedì" as const,
        pasti: [
          pasto({
            tipo: "pranzo",
            nome: "Zuppa di verdure",
            ingredienti: [ingrediente({ nome: "Dado vegetale", quantita: 10, unita: "g", reparto: "Dispensa" })],
          }),
          pasto({
            tipo: "cena",
            nome: "Insalata",
            ingredienti: [ingrediente({ nome: "Insalata mista", quantita: 100, unita: "g", reparto: "Frutta e verdura" })],
          }),
        ],
      },
    ];

    const risultato = await validaGiorni(profiloCeliaco, giorni);
    const pranzo = risultato[0].pasti[0];

    // "Da verificare" non blocca il pasto: nessun tentativo di rigenerazione.
    expect(mockRegeneratePasto).not.toHaveBeenCalled();
    expect(pranzo.nome).toBe("Zuppa di verdure");
    expect(pranzo.verificare).toBe(true);
    expect(pranzo.ingredienti_a_rischio).toContain("Dado vegetale");
    // Non è "non adatto": è solo da controllare in etichetta.
    expect(pranzo.ingredienti_non_adatti).toBeUndefined();
  });

  it("imposta ingredienti_non_adatti solo per la categoria 'non adatto', dopo i tentativi massimi", async () => {
    mockRegeneratePasto.mockResolvedValue(
      pasto({
        tipo: "pranzo",
        nome: "Pasta al pesto (tentativo fallito)",
        ingredienti: [ingrediente({ nome: "Pasta", quantita: 100, unita: "g", reparto: "Pane e cereali" })],
      }),
    );

    const risultato = await validaGiorni(profiloCeliaco, creaPianoEsempio());
    const lunediPranzo = risultato[0].pasti[0];

    expect(lunediPranzo.ingredienti_non_adatti).toEqual(["Pasta"]);
  });
});

describe("assicuraVarieta — niente due pasti con lo stesso nome nella settimana", () => {
  it("non chiama mai l'AI se i 14 pasti sono già tutti distinti", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());
    const risultato = await assicuraVarieta(profiloSenzaRestrizioni, giorni);

    expect(mockRegeneratePasto).not.toHaveBeenCalled();
    expect(risultato).toEqual(giorni);
  });

  it("rigenera i doppioni mantenendo la prima occorrenza di ogni piatto", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());
    // Forza un piano degenere come quello segnalato dall'utente: lo stesso
    // piatto ("Pasta al pomodoro") ripetuto su più pasti della settimana.
    giorni[1].pasti[0] = { ...giorni[1].pasti[0], nome: "Pasta al pomodoro" };
    giorni[2].pasti[1] = { ...giorni[2].pasti[1], nome: "Pasta al pomodoro" };

    // Ogni rigenerazione propone un nome diverso dalle altre, come farebbe
    // l'AI reale conoscendo l'elenco dei piatti da evitare.
    let contatore = 0;
    mockRegeneratePasto.mockImplementation(async () => {
      contatore += 1;
      return pasto({
        tipo: "pranzo",
        nome: `Riso al pomodoro ${contatore}`,
        ingredienti: [ingrediente({ nome: "Riso", quantita: 100, unita: "g", reparto: "Dispensa" })],
      });
    });

    const risultato = await assicuraVarieta(profiloSenzaRestrizioni, giorni);

    // 2 doppioni da sistemare (la prima occorrenza, al Lunedì, resta).
    expect(mockRegeneratePasto).toHaveBeenCalledTimes(2);
    expect(risultato[0].pasti[0].nome).toBe("Pasta al pomodoro");
    expect(risultato[1].pasti[0].nome).toBe("Riso al pomodoro 1");
    expect(risultato[2].pasti[1].nome).toBe("Riso al pomodoro 2");

    // All'AI viene comunicato l'elenco dei piatti già usati da evitare.
    const nomiDaEvitare = mockRegeneratePasto.mock.calls[0][5] as string[];
    expect(nomiDaEvitare).toContain("Pasta al pomodoro");
  });

  it("si ferma dopo i tentativi massimi se l'AI continua a riproporre doppioni", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());
    giorni[1].pasti[0] = { ...giorni[1].pasti[0], nome: "Pasta al pomodoro" };

    // L'AI simulata "sbaglia" sempre: ripropone lo stesso nome già in uso.
    mockRegeneratePasto.mockResolvedValue(
      pasto({
        tipo: "pranzo",
        nome: "Pasta al pomodoro",
        ingredienti: [ingrediente({ nome: "Pasta", quantita: 100, unita: "g", reparto: "Pane e cereali" })],
      }),
    );

    const risultato = await assicuraVarieta(profiloSenzaRestrizioni, giorni);

    // Al massimo 2 tentativi (MAX_RIGENERAZIONI_VARIETA), non all'infinito.
    expect(mockRegeneratePasto).toHaveBeenCalledTimes(2);
    expect(risultato[1].pasti[0].nome).toBe("Pasta al pomodoro");
  });

  it("se la rigenerazione fallisce, lascia il doppione invece di far crashare tutto", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());
    giorni[1].pasti[0] = { ...giorni[1].pasti[0], nome: "Pasta al pomodoro" };

    mockRegeneratePasto.mockRejectedValue(new Error("rete non disponibile"));

    const risultato = await assicuraVarieta(profiloSenzaRestrizioni, giorni);

    expect(risultato[1].pasti[0].nome).toBe("Pasta al pomodoro");
  });
});

describe("adattaEntroBudget — adattamento costi con risposta AI simulata", () => {
  it("senza un budget impostato non chiama mai l'AI", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());
    const risultato = await adattaEntroBudget(profiloSenzaRestrizioni, giorni, "Conad", null);

    expect(mockAdattaBudget).not.toHaveBeenCalled();
    expect(risultato.budgetSuperato).toBe(false);
  });

  it("se il piano rientra già nel budget, non chiama l'AI", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());
    const risultato = await adattaEntroBudget(profiloSenzaRestrizioni, giorni, "Conad", 1000);

    expect(mockAdattaBudget).not.toHaveBeenCalled();
    expect(risultato.budgetSuperato).toBe(false);
  });

  it("se il piano sfora, chiede all'AI un piano più economico e converge se la proposta rientra nel budget", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());

    // Piano "economico" simulato: stessi 14 pasti ma con un solo
    // ingrediente a prezzo molto basso, ben sotto qualsiasi budget realistico.
    const pianoEconomico = creaPianoEsempio().map((g) => ({
      ...g,
      pasti: g.pasti.map((p) => ({
        ...p,
        ingredienti: [ingrediente({ nome: "Pasta economica", quantita: 50, unita: "g" as const, reparto: "Dispensa", prezzo_stimato_eur: 0.05 })],
      })),
    }));
    mockAdattaBudget.mockResolvedValueOnce({ giorni: pianoEconomico });

    const risultato = await adattaEntroBudget(profiloSenzaRestrizioni, giorni, "Conad", 1);

    expect(mockAdattaBudget).toHaveBeenCalledTimes(1);
    expect(risultato.budgetSuperato).toBe(false);
    expect(risultato.groceryList.totale_stimato).toBeLessThanOrEqual(1);
  });

  it("se l'AI non riesce a rientrare nel budget nemmeno dopo i tentativi massimi, segnala budgetSuperato", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());

    // L'AI simulata restituisce sempre lo stesso piano costoso: non converge mai.
    mockAdattaBudget.mockResolvedValue({ giorni: creaPianoEsempio() });

    const risultato = await adattaEntroBudget(profiloSenzaRestrizioni, giorni, "Esselunga", 0.01);

    // Al massimo 2 tentativi (MAX_TENTATIVI_BUDGET), non all'infinito.
    expect(mockAdattaBudget).toHaveBeenCalledTimes(2);
    expect(risultato.budgetSuperato).toBe(true);
  });

  it("se adattaBudget fallisce (errore), si ferma e segnala lo sforamento invece di crashare", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());
    mockAdattaBudget.mockRejectedValueOnce(new Error("rete non disponibile"));

    const risultato = await adattaEntroBudget(profiloSenzaRestrizioni, giorni, "Conad", 0.01);

    expect(mockAdattaBudget).toHaveBeenCalledTimes(1);
    expect(risultato.budgetSuperato).toBe(true);
  });

  it("deduplica un doppione già nei giorni iniziali, anche senza sforare il budget (nessuna chiamata ad adattaBudget)", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());
    giorni[1].pasti[0] = { ...giorni[1].pasti[0], nome: "Pasta al pomodoro" };

    mockRegeneratePasto.mockResolvedValueOnce(
      pasto({
        tipo: "pranzo",
        nome: "Riso al pomodoro",
        ingredienti: [ingrediente({ nome: "Riso", quantita: 100, unita: "g", reparto: "Dispensa" })],
      }),
    );

    const risultato = await adattaEntroBudget(profiloSenzaRestrizioni, giorni, "Conad", null);

    expect(mockAdattaBudget).not.toHaveBeenCalled();
    expect(mockRegeneratePasto).toHaveBeenCalledTimes(1);
    expect(risultato.giorni[1].pasti[0].nome).toBe("Riso al pomodoro");
  });

  it("deduplica un doppione introdotto dal piano \"economico\" proposto da adattaBudget", async () => {
    const giorni = await validaGiorni(profiloSenzaRestrizioni, creaPianoEsempio());

    // adattaBudget risolve il budget ma, rivedendo liberamente tutti i 14
    // pasti, introduce per errore un doppione (stesso nome a due pasti).
    const pianoEconomicoConDoppione = creaPianoEsempio().map((g, gi) => ({
      ...g,
      pasti: g.pasti.map((p, pi) => ({
        ...p,
        nome: gi === 0 && pi === 0 ? "Riso in bianco" : gi === 1 && pi === 0 ? "Riso in bianco" : p.nome,
        ingredienti: [ingrediente({ nome: "Riso", quantita: 50, unita: "g" as const, reparto: "Dispensa", prezzo_stimato_eur: 0.05 })],
      })),
    }));
    mockAdattaBudget.mockResolvedValue({ giorni: pianoEconomicoConDoppione });
    mockRegeneratePasto.mockResolvedValue(
      pasto({
        tipo: "pranzo",
        nome: "Pasta in bianco",
        ingredienti: [ingrediente({ nome: "Pasta", quantita: 50, unita: "g", reparto: "Pane e cereali", prezzo_stimato_eur: 0.05 })],
      }),
    );

    const risultato = await adattaEntroBudget(profiloSenzaRestrizioni, giorni, "Conad", 5);

    expect(mockAdattaBudget).toHaveBeenCalled();
    const nomi = risultato.giorni.flatMap((g) => g.pasti.map((p) => p.nome));
    const duplicati = nomi.filter((n, i) => nomi.indexOf(n) !== i);
    expect(duplicati).toEqual([]);
  });
});
