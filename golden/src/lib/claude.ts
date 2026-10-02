import "server-only";
import Anthropic from "@anthropic-ai/sdk";
import { z } from "zod";
import { zodOutputFormat } from "@anthropic-ai/sdk/helpers/zod";

const MODEL = process.env.ANTHROPIC_MODEL || "claude-opus-5-5";

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

const PastoSchema = z.object({
  tipo: z.enum(["pranzo", "cena"]),
  nome: z.string(),
  ingredienti: z.array(IngredienteSchema),
  tempo_preparazione_min: z.number(),
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
export type Pasto = z.infer<typeof PastoSchema>;
export type Giorno = z.infer<typeof GiornoSchema>;
export type MealPlan = z.infer<typeof MealPlanSchema>;

export type ProfiloPerPiano = {
  restrizioni: string[];
  obiettivo: string | null;
  preferenze: { cucina?: string[]; graditi?: string; non_graditi?: string } | null;
  tempo_max_cucina: number | null;
  household_size: number | null;
};

function buildContestoProfilo(profilo: ProfiloPerPiano): string {
  const righe = [
    `Restrizioni alimentari (vincolo rigido, da rispettare SEMPRE senza eccezioni): ${
      profilo.restrizioni.length > 0 ? profilo.restrizioni.join(", ") : "nessuna"
    }`,
    `Obiettivo: ${profilo.obiettivo || "non specificato"}`,
    `Cucina preferita: ${profilo.preferenze?.cucina?.join(", ") || "qualsiasi"}`,
    `Alimenti graditi: ${profilo.preferenze?.graditi || "nessuna preferenza specifica"}`,
    `Alimenti non graditi (da evitare): ${profilo.preferenze?.non_graditi || "nessuno"}`,
    `Tempo massimo di preparazione per pasto: ${profilo.tempo_max_cucina || 30} minuti`,
    `Numero di persone per cui cucinare: ${profilo.household_size || 1}`,
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

export async function generateMealPlan(profilo: ProfiloPerPiano): Promise<MealPlan> {
  const response = await client.messages.parse({
    model: MODEL,
    max_tokens: 8000,
    system:
      "Sei un assistente che genera piani settimanali di pasti (pranzo e cena, 7 giorni) in italiano. " +
      "Le restrizioni alimentari sono un vincolo rigido e non negoziabile: non includere MAI, nemmeno in tracce dichiarate, un ingrediente incompatibile con le restrizioni indicate. " +
      "Rispetta anche obiettivo, preferenze e tempo di preparazione, in questo ordine di priorità. " +
      ISTRUZIONI_INGREDIENTI,
    messages: [
      {
        role: "user",
        content: `Genera il piano settimanale (pranzo e cena per ognuno dei 7 giorni) per questo profilo:\n\n${buildContestoProfilo(
          profilo,
        )}`,
      },
    ],
    output_config: {
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
): Promise<RisultatoModifica> {
  const response = await client.messages.parse({
    model: MODEL,
    max_tokens: 8000,
    system:
      "Sei un assistente che modifica un piano settimanale di pasti già esistente, in base a una richiesta " +
      "dell'utente in linguaggio naturale, in italiano. Applica SOLO la modifica richiesta, lasciando invariato " +
      "il resto del piano quando possibile. Le restrizioni alimentari restano un vincolo rigido e non negoziabile: " +
      "se la richiesta dell'utente include un ingrediente o un pasto incompatibile con le restrizioni, NON applicarla. " +
      "In quel caso imposta modifica_applicata a false, spiega brevemente il motivo in motivo_rifiuto (in italiano, " +
      "rivolgendoti direttamente all'utente) e restituisci il piano INVARIATO. Se invece la richiesta è compatibile, " +
      "applicala, imposta modifica_applicata a true e motivo_rifiuto a null. " +
      ISTRUZIONI_INGREDIENTI,
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

export async function regeneratePasto(
  profilo: ProfiloPerPiano,
  giorno: string,
  pasto: Pasto,
  ingredientiDaEvitare: string[],
): Promise<Pasto> {
  const response = await client.messages.parse({
    model: MODEL,
    max_tokens: 2000,
    system:
      "Sei un assistente che rigenera un singolo pasto di un piano settimanale, in italiano. " +
      "Le restrizioni alimentari sono un vincolo rigido: non includere MAI un ingrediente incompatibile. " +
      ISTRUZIONI_INGREDIENTI,
    messages: [
      {
        role: "user",
        content:
          `Rigenera il ${pasto.tipo} di ${giorno} per questo profilo:\n\n${buildContestoProfilo(profilo)}\n\n` +
          `Il pasto precedente proposto (${pasto.nome}) conteneva questi ingredienti a rischio, da evitare assolutamente nella nuova proposta: ${ingredientiDaEvitare.join(", ")}.`,
      },
    ],
    output_config: {
      format: zodOutputFormat(PastoSchema),
    },
  });

  if (!response.parsed_output) {
    throw new Error("Claude non ha restituito un pasto valido.");
  }

  return response.parsed_output;
}
