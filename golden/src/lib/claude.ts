import "server-only";
import Anthropic from "@anthropic-ai/sdk";
import { z } from "zod";
import { zodOutputFormat } from "@anthropic-ai/sdk/helpers/zod";
import { istruzioneStagionalita } from "./stagionalita";
import { istruzioneDispensa } from "./dispensa";

// Sonnet invece di Opus: elencare pasti/ingredienti/prezzi non richiede un
// ragionamento complesso, e Sonnet genera molto più velocemente — importante
// per restare entro i tempi di esecuzione di una funzione serverless. La
// sicurezza celiaca non dipende dal modello: resta garantita dal controllo
// statico in glutine-check.ts, applicato comunque dopo la generazione.
const MODEL = process.env.ANTHROPIC_MODEL || "claude-sonnet-5-5";

const client = new Anthropic();

export const REPARTI = [
  "Frutta e verdura",
  "Carne e pesce",
  "Latticini e uova",
  "Pane e cereali",
  "Dispensa",
  "Surgelati",
  "Altro",
] as const;

const IngredienteSchema = z.object({
  nome: z.string(),
  quantita: z.number(),
  unita: z.enum(["g", "kg", "ml", "l", "pz", "confezione"]),
  reparto: z.enum(REPARTI),
  prezzo_stimato_eur: z.number(),
});

const NutrizioneSchema = z.object({
  calorie: z.number(),
  proteine_g: z.number(),
  carboidrati_g: z.number(),
  grassi_g: z.number(),
  fibre_g: z.number(),
});

const PastoSchema = z.object({
  tipo: z.enum(["pranzo", "cena"]),
  nome: z.string(),
  ingredienti: z.array(IngredienteSchema),
  tempo_preparazione_min: z.number(),
  nutrizione: NutrizioneSchema,
  preparazione: z.array(z.string()),
});

const GiornoSchema = z.object({
  giorno: z.enum([
    "Lunedì",
    "Martedì",
    "Mercoledì",
    "Giovedì",
    "Venerdì",
    "Sabato",
    "Domenica",
  ]),
  pasti: z.array(PastoSchema).length(2),
});

const MealPlanSchema = z.object({
  giorni: z.array(GiornoSchema).length(7),
});

export type Ingrediente = z.infer<typeof IngredienteSchema>;
export type Nutrizione = z.infer<typeof NutrizioneSchema>;
export type Pasto = z.infer<typeof PastoSchema>;
export type Giorno = z.infer<typeof GiornoSchema>;
export type MealPlan = z.infer<typeof MealPlanSchema>;

// Target nutrizionali per singolo pasto, impostati esplicitamente
// dall'utente in Profilo — diversi dal confronto LARN (automatico, dai
// dati biometrici, solo informativo): questi sono un vincolo che entra nel
// prompt di generazione. Tutti i campi opzionali: solo quelli impostati
// vengono passati come vincolo.
export type ObiettiviNutrizionaliPerPasto = {
  calorie_min: number | null;
  calorie_max: number | null;
  proteine_min_g: number | null;
  carboidrati_max_g: number | null;
  grassi_max_g: number | null;
};

export type ProfiloPerPiano = {
  restrizioni: string[];
  obiettivo: string | null;
  preferenze: { cucina?: string[]; graditi?: string; non_graditi?: string } | null;
  tempo_max_cucina: number | null;
  household_size: number | null;
  budget_settimanale: number | null;
  obiettivi_nutrizionali?: ObiettiviNutrizionaliPerPasto | null;
};

const NOTA_COSTO_CONFEZIONI =
  "Attenzione a come si traduce in costo reale: al supermercato si compra una confezione INTERA per ogni " +
  "ingrediente distinto, anche se in ricetta ne servono pochi grammi (es. anche solo 5g di cumino richiedono " +
  "comunque di comprare l'intero barattolo di spezie). Quindi più ingredienti diversi e specifici introduci " +
  "nella settimana, più sale il costo reale, indipendentemente dalle quantità per ricetta. La leva più efficace " +
  "per restare nel budget è RIUSARE le stesse spezie/condimenti/ingredienti di base in PIÙ RICETTE DIVERSE tra " +
  "loro (es. lo stesso pollo o lo stesso riso base, cucinati in modi diversi in pasti diversi) invece di " +
  "introdurre un ingrediente nuovo ogni volta, oltre a scegliere ingredienti più economici. Questo NON significa " +
  "proporre lo stesso piatto più volte: il piatto (nome, preparazione, combinazione) deve restare distinto da " +
  "pasto a pasto, è l'ingrediente di base che si ripete tra ricette diverse.";

const ISTRUZIONE_VARIETA =
  "Varietà (vincolo rigido quanto le restrizioni e il budget, mai sacrificabile per risparmiare o ridurre gli " +
  "sprechi): i 14 pasti della settimana devono essere 14 ricette DISTINTE, mai lo stesso piatto ripetuto due " +
  "volte (stesso nome o stessa combinazione con solo variazioni cosmetiche). Varia proteine, tipo di cottura, " +
  "cucina ed elaborazione dei piatti durante la settimana. Se l'obiettivo è ridurre gli sprechi o il costo, " +
  "ottienilo riusando gli stessi ingredienti di base in ricette diverse (vedi sopra), MAI riducendo il numero di " +
  "ricette distinte o ripetendo un piatto già usato in un altro giorno.";

// Criterio di scelta aggiuntivo, non un vincolo: entra in gioco solo
// quando più combinazioni di ricette sono già altrettanto valide secondo
// le priorità sopra (restrizioni, budget, obiettivo/preferenze/tempo,
// varietà) — tra quelle, premia il batch cooking.
const ISTRUZIONE_BATCH_COOKING =
  "Criterio aggiuntivo, da applicare solo a parità delle priorità sopra (mai sopra restrizioni, budget, " +
  "obiettivo, preferenze o varietà): quando più combinazioni di ricette sono altrettanto valide, preferisci " +
  "quelle che permettono il batch cooking — condividere lo stesso ingrediente principale (una proteina o un " +
  "altro ingrediente centrale della ricetta, es. lo stesso taglio di pollo, lo stesso pesce, gli stessi legumi) " +
  "tra due o più ricette DIVERSE della settimana, così da comprarne una quantità maggiore in un'unica volta " +
  "invece di tante quantità piccole di ingredienti diversi — riduce sia il costo reale (meno confezioni aperte " +
  "e sprecate in parte) sia lo spreco alimentare. Le ricette restano comunque distinte tra loro (vedi varietà " +
  "sopra): cambia la preparazione o il resto del piatto, non l'ingrediente principale condiviso.";

function obiettiviNutrizionaliTesto(obiettivi: ObiettiviNutrizionaliPerPasto | null | undefined): string {
  if (!obiettivi) return "non specificati";

  const parti: string[] = [];
  if (obiettivi.calorie_min != null) parti.push(`almeno ${obiettivi.calorie_min} kcal`);
  if (obiettivi.calorie_max != null) parti.push(`al massimo ${obiettivi.calorie_max} kcal`);
  if (obiettivi.proteine_min_g != null) parti.push(`almeno ${obiettivi.proteine_min_g}g di proteine`);
  if (obiettivi.carboidrati_max_g != null) parti.push(`al massimo ${obiettivi.carboidrati_max_g}g di carboidrati`);
  if (obiettivi.grassi_max_g != null) parti.push(`al massimo ${obiettivi.grassi_max_g}g di grassi`);

  return parti.length > 0 ? parti.join(", ") : "non specificati";
}

function obiettivoConNota(obiettivo: string | null): string {
  if (!obiettivo) return "non specificato";
  if (obiettivo === "Ridurre gli sprechi") {
    // Senza questa precisazione l'AI tende a interpretare "ridurre gli
    // sprechi" come "comprare meno ingredienti diversi", collassando le
    // ricette su pochissimi piatti ripetuti — esattamente l'effetto
    // opposto a quello voluto: lo spreco si riduce usando bene quello che
    // si compra, non riducendo la varietà dei pasti.
    return (
      `${obiettivo} (significa: non far avanzare ingredienti inutilizzati e usare bene le confezioni intere ` +
      "comprate, RIUSANDO gli stessi ingredienti di base in ricette diverse — non significa ridurre il numero " +
      "di ricette distinte o ripetere gli stessi piatti)"
    );
  }
  return obiettivo;
}

function buildContestoProfilo(profilo: ProfiloPerPiano): string {
  const righe = [
    `Restrizioni alimentari (vincolo rigido, da rispettare SEMPRE senza eccezioni): ${
      profilo.restrizioni.length > 0 ? profilo.restrizioni.join(", ") : "nessuna"
    }`,
    `Obiettivo: ${obiettivoConNota(profilo.obiettivo)}`,
    `Obiettivi nutrizionali per pasto (vincolo aggiuntivo, stessa priorità dell'obiettivo generale — sempre ` +
      `sotto restrizioni alimentari e budget, da rispettare quando possibile senza violarli): ` +
      obiettiviNutrizionaliTesto(profilo.obiettivi_nutrizionali),
    `Cucina preferita: ${profilo.preferenze?.cucina?.join(", ") || "qualsiasi"}`,
    `Alimenti graditi: ${profilo.preferenze?.graditi || "nessuna preferenza specifica"}`,
    `Alimenti non graditi (da evitare): ${profilo.preferenze?.non_graditi || "nessuno"}`,
    `Tempo massimo di preparazione per pasto: ${profilo.tempo_max_cucina || 30} minuti`,
    `Numero di persone per cui cucinare: ${profilo.household_size || 1}`,
    `Budget settimanale per la spesa: ${
      profilo.budget_settimanale
        ? `€${profilo.budget_settimanale} (vincolo rigido: il totale stimato della spesa, somma di tutti i prezzo_stimato_eur, NON deve superarlo). ${NOTA_COSTO_CONFEZIONI}`
        : "non specificato"
    }`,
  ];
  return righe.join("\n");
}

const ISTRUZIONI_INGREDIENTI =
  "Per ogni ingrediente indica: nome (semplice, in italiano), quantità (numero) e unità " +
  '("g", "kg", "ml", "l", "pz" per i pezzi interi come uova o confezioni standard, "confezione" per prodotti confezionati), ' +
  "scalata per il numero di persone indicato. Indica anche il reparto del supermercato a cui appartiene " +
  `(uno tra: ${REPARTI.join(", ")}). ` +
  "Indica infine prezzo_stimato_eur: il prezzo realistico in euro per QUELLA quantità specifica di QUEL " +
  "preciso ingrediente, basato sui prezzi medi reali dei supermercati italiani (fascia media, es. Conad/Coop) — " +
  "non un prezzo medio di reparto. Ogni ingrediente ha un prezzo diverso: es. 150g di merluzzo e 150g di piselli " +
  "surgelati NON costano uguale anche se stanno entrambi nei surgelati; il salmone costa più del pollo; il parmigiano " +
  "più della mozzarella. Stima con buon senso in base al tipo di prodotto specifico, fresco o surgelato, standard o pregiato.";

const ISTRUZIONI_NUTRIZIONE =
  "Per ogni pasto (non per singolo ingrediente) indica anche il campo nutrizione: calorie totali del piatto (kcal), " +
  "proteine_g, carboidrati_g, grassi_g e fibre_g (grammi), per la porzione così come preparata (per persona, non per l'intera pentola). " +
  "Sono valori stimati con buon senso nutrizionale, non da un database ufficiale — va bene un'approssimazione ragionevole.";

const ISTRUZIONI_PREPARAZIONE =
  "Per ogni pasto indica anche il campo preparazione: un array di 3-6 passaggi brevi e chiari, in italiano, " +
  "che spiegano come cucinare il piatto dall'inizio alla fine, scritti per chi ha poca esperienza in cucina.";

const ISTRUZIONE_SCOPERTA =
  "L'utente ha scelto la modalità 'Scoperta': non vuole il piano più prevedibile e sicuro, vuole provare cose " +
  "diverse dal solito. Proponi ricette, cucine, tecniche di cottura e ingredienti che normalmente non sceglieresti " +
  "di default per questo profilo — più varietà rispetto a un piano standard — sempre nel rispetto di restrizioni, " +
  "preferenze e budget. Evita i piatti più ovvi e ripetitivi per questo tipo di richiesta.";

export async function generateMealPlan(
  profilo: ProfiloPerPiano,
  modalita: "routine" | "scoperta" = "routine",
  dispensa: Map<string, number> = new Map(),
): Promise<MealPlan> {
  const response = await client.messages.parse({
    model: MODEL,
    max_tokens: 16000,
    system:
      "Sei un assistente che genera piani settimanali di pasti (pranzo e cena, 7 giorni) in italiano. " +
      "Le restrizioni alimentari sono un vincolo rigido e non negoziabile: non includere MAI, nemmeno in tracce dichiarate, un ingrediente incompatibile con le restrizioni indicate. " +
      "Se è indicato un budget settimanale, è anch'esso un vincolo rigido: il totale stimato della spesa (somma di tutti i prezzo_stimato_eur dell'intero piano) non deve superarlo. " +
      "Rispetta anche l'obiettivo generale e gli eventuali obiettivi nutrizionali per pasto (stessa priorità " +
      "dell'obiettivo, mai sopra restrizioni o budget), poi preferenze e tempo di preparazione, in questo ordine " +
      "di priorità, scegliendo ingredienti e porzioni che permettano di rientrare nel budget. " +
      ISTRUZIONE_VARIETA + " " + ISTRUZIONE_BATCH_COOKING + " " +
      (modalita === "scoperta" ? ISTRUZIONE_SCOPERTA + " " : "") +
      ISTRUZIONI_INGREDIENTI + " " + ISTRUZIONI_NUTRIZIONE + " " + ISTRUZIONI_PREPARAZIONE + " " +
      istruzioneDispensa(dispensa) + " " + istruzioneStagionalita(),
    messages: [
      {
        role: "user",
        content: `Genera il piano settimanale (pranzo e cena per ognuno dei 7 giorni) per questo profilo:\n\n${buildContestoProfilo(
          profilo,
        )}`,
      },
    ],
    output_config: {
      effort: "low",
      format: zodOutputFormat(MealPlanSchema),
    },
  });

  if (!response.parsed_output) {
    throw new Error("Claude non ha restituito un piano valido.");
  }

  return response.parsed_output;
}

const ModificaOutputSchema = z.object({
  modifica_applicata: z.boolean(),
  motivo_rifiuto: z.string().nullable(),
  giorni: z.array(GiornoSchema).length(7),
});

export type RisultatoModifica = {
  modificaApplicata: boolean;
  motivoRifiuto: string | null;
  giorni: Giorno[];
};

export async function modificaPiano(
  profilo: ProfiloPerPiano,
  giorniAttuali: Giorno[],
  messaggioUtente: string,
  dispensa: Map<string, number> = new Map(),
): Promise<RisultatoModifica> {
  const response = await client.messages.parse({
    model: MODEL,
    max_tokens: 16000,
    system:
      "Sei un assistente che modifica un piano settimanale di pasti già esistente, in base a una richiesta " +
      "dell'utente in linguaggio naturale, in italiano. Applica SOLO la modifica richiesta, lasciando invariato " +
      "il resto del piano quando possibile. Le restrizioni alimentari restano un vincolo rigido e non negoziabile: " +
      "se la richiesta dell'utente include un ingrediente o un pasto incompatibile con le restrizioni, NON applicarla. " +
      "In quel caso imposta modifica_applicata a false, spiega brevemente il motivo in motivo_rifiuto (in italiano, " +
      "rivolgendoti direttamente all'utente) e restituisci il piano INVARIATO. Se invece la richiesta è compatibile, " +
      "applicala, imposta modifica_applicata a true e motivo_rifiuto a null. " +
      "Il piano ha sempre esattamente 14 pasti fissi (7 giorni, pranzo e cena): non puoi aggiungere o togliere " +
      "pasti, ma puoi cambiarne liberamente la composizione. Se la richiesta implica un cambiamento significativo " +
      "nella quantità settimanale di un ingrediente o di un nutriente (es. usarne molto di più o di meno), la " +
      "soluzione migliore è quasi sempre cambiare QUALI pasti lo contengono — sostituendo un piatto con un altro " +
      "che usa di più (o di meno) quell'ingrediente — piuttosto che alterare le porzioni di una singola ricetta " +
      "fino a renderle irrealistiche per una persona (es. non proporre mai 800g di pollo in un solo piatto). " +
      ISTRUZIONI_INGREDIENTI + " " + ISTRUZIONI_NUTRIZIONE + " " + ISTRUZIONI_PREPARAZIONE + " " +
      istruzioneDispensa(dispensa) + " " + istruzioneStagionalita(),
    messages: [
      {
        role: "user",
        content:
          `Profilo:\n${buildContestoProfilo(profilo)}\n\n` +
          `Piano attuale (JSON):\n${JSON.stringify({ giorni: giorniAttuali })}\n\n` +
          `Richiesta dell'utente: "${messaggioUtente}"\n\n` +
          "Restituisci il piano completo (tutti i 7 giorni): aggiornato se la richiesta è compatibile con le restrizioni, invariato altrimenti.",
      },
    ],
    output_config: {
      // "medium", non "low" come le generazioni massive: qui l'AI deve
      // interpretare con più cura una richiesta mirata (es. "aumenta le
      // proteine di tot grammi"), non solo elencare 14 pasti.
      effort: "medium",
      format: zodOutputFormat(ModificaOutputSchema),
    },
  });

  if (!response.parsed_output) {
    throw new Error("Claude non ha restituito un piano valido.");
  }

  return {
    modificaApplicata: response.parsed_output.modifica_applicata,
    motivoRifiuto: response.parsed_output.motivo_rifiuto,
    giorni: response.parsed_output.giorni,
  };
}

export async function adattaBudget(
  profilo: ProfiloPerPiano,
  giorniAttuali: Giorno[],
  totaleAttualeEur: number,
  budgetEur: number,
  dispensa: Map<string, number> = new Map(),
): Promise<MealPlan> {
  const response = await client.messages.parse({
    model: MODEL,
    max_tokens: 16000,
    system:
      "Sei un assistente che riduce il costo di un piano settimanale di pasti già esistente, in italiano, " +
      "senza violare le restrizioni alimentari (vincolo rigido, non negoziabile) e senza stravolgere le preferenze. " +
      "Riduci il costo totale stimato sostituendo ingredienti costosi con alternative più economiche (es. proteine " +
      "meno pregiate, prodotti di stagione, porzioni più ragionevoli), mantenendo varietà e qualità nutrizionale. " +
      NOTA_COSTO_CONFEZIONI + " " + ISTRUZIONE_VARIETA + " " + ISTRUZIONE_BATCH_COOKING + " " +
      ISTRUZIONI_INGREDIENTI + " " + ISTRUZIONI_NUTRIZIONE + " " + ISTRUZIONI_PREPARAZIONE + " " +
      istruzioneDispensa(dispensa) + " " + istruzioneStagionalita(),
    messages: [
      {
        role: "user",
        content:
          `Profilo:\n${buildContestoProfilo(profilo)}\n\n` +
          `Piano attuale (JSON):\n${JSON.stringify({ giorni: giorniAttuali })}\n\n` +
          `Il costo REALE di questo piano, calcolato dopo l'acquisto (confezioni intere e fascia del supermercato), è €${totaleAttualeEur.toFixed(2)}, ma il budget settimanale è €${budgetEur}. ` +
          "Il modo più efficace per abbassarlo non è ridurre di poco ogni quantità, ma RIDURRE IL NUMERO DI INGREDIENTI DI BASE DIVERSI E SPECIFICI usati nella settimana " +
          "(riusa le stesse spezie/condimenti/basi in RICETTE DIVERSE invece di introdurre un ingrediente nuovo ogni volta) e sostituire gli ingredienti più costosi — " +
          "le 14 ricette devono però restare 14 piatti distinti, mai lo stesso piatto ripetuto più volte nella settimana. " +
          "Punta a un costo comodamente sotto il budget (non appena sotto), perché l'arrotondamento alle confezioni reali può far risalire il totale. " +
          "Rivedi il piano per rientrare nel budget, restituendo tutti i 7 giorni.",
      },
    ],
    output_config: {
      effort: "low",
      format: zodOutputFormat(MealPlanSchema),
    },
  });

  if (!response.parsed_output) {
    throw new Error("Claude non ha restituito un piano valido.");
  }

  return response.parsed_output;
}

export async function regeneratePasto(
  profilo: ProfiloPerPiano,
  giorno: string,
  pasto: Pasto,
  ingredientiDaEvitare: string[],
  dispensa: Map<string, number> = new Map(),
  nomiDaEvitare: string[] = [],
): Promise<Pasto> {
  const response = await client.messages.parse({
    model: MODEL,
    max_tokens: 4000,
    system:
      "Sei un assistente che rigenera un singolo pasto di un piano settimanale, in italiano. " +
      "Le restrizioni alimentari sono un vincolo rigido: non includere MAI un ingrediente incompatibile. " +
      ISTRUZIONI_INGREDIENTI + " " + ISTRUZIONI_NUTRIZIONE + " " + ISTRUZIONI_PREPARAZIONE + " " +
      istruzioneDispensa(dispensa) + " " + istruzioneStagionalita(),
    messages: [
      {
        role: "user",
        content:
          `Rigenera il ${pasto.tipo} di ${giorno} per questo profilo:\n\n${buildContestoProfilo(profilo)}\n\n` +
          (ingredientiDaEvitare.length > 0
            ? `Il pasto precedente proposto (${pasto.nome}) conteneva questi ingredienti a rischio, da evitare assolutamente nella nuova proposta: ${ingredientiDaEvitare.join(", ")}.\n\n`
            : "") +
          (nomiDaEvitare.length > 0
            ? `Questi piatti sono già usati in altri giorni della stessa settimana: ${nomiDaEvitare.join(", ")}. ` +
              "La nuova proposta deve essere una ricetta distinta da tutte queste, non una variante dello stesso piatto " +
              "(va bene riusare gli stessi ingredienti di base, ma la ricetta/il piatto dev'essere diverso)."
            : ""),
      },
    ],
    output_config: {
      effort: "low",
      format: zodOutputFormat(PastoSchema),
    },
  });

  if (!response.parsed_output) {
    throw new Error("Claude non ha restituito un pasto valido.");
  }

  return response.parsed_output;
}
