// Test "smoke": chiamano DAVVERO l'API Anthropic (costano token veri e
// richiedono ANTHROPIC_API_KEY nell'ambiente). Non vengono eseguiti da
// `npm test` — solo manualmente con `npm run test:smoke`, quando serve
// verificare che l'integrazione reale funzioni ancora (es. dopo un cambio
// di modello o di prompt). Usano direttamente le funzioni esportate da
// claude.ts, lo stesso codice usato in produzione — non reimplementano le
// chiamate.
import { describe, it, expect } from "vitest";
import {
  generateMealPlan,
  modificaPiano,
  adattaBudget,
  regeneratePasto,
  type ProfiloPerPiano,
  type Pasto,
} from "./claude";
import { ingredientiARischio } from "./glutine-check";
import { creaPianoEsempio } from "@/test/fixtures/piano-esempio";

const haChiaveApi = Boolean(process.env.ANTHROPIC_API_KEY || process.env.ANTHROPIC_AUTH_TOKEN);

const profiloCeliaco: ProfiloPerPiano = {
  restrizioni: ["Glutine (celiachia)"],
  obiettivo: "Mangiare meglio",
  preferenze: { cucina: ["Italiana"] },
  tempo_max_cucina: 30,
  household_size: 2,
  budget_settimanale: null,
};

describe.skipIf(!haChiaveApi)("Smoke test — API Anthropic reale", () => {
  it(
    "generateMealPlan produce un piano di 7 giorni senza ingredienti a rischio glutine",
    async () => {
      const piano = await generateMealPlan(profiloCeliaco, "routine");

      expect(piano.giorni).toHaveLength(7);
      for (const giorno of piano.giorni) {
        expect(giorno.pasti).toHaveLength(2);
        for (const pasto of giorno.pasti) {
          const rischi = ingredientiARischio(pasto.ingredienti.map((i) => i.nome));
          expect(rischi, `${giorno.giorno} ${pasto.tipo} (${pasto.nome}): ${rischi.join(", ")}`).toEqual([]);
        }
      }
    },
    30_000,
  );

  it(
    "modificaPiano rifiuta una richiesta incompatibile con le restrizioni, spiegando il motivo",
    async () => {
      const risultato = await modificaPiano(
        profiloCeliaco,
        creaPianoEsempio(),
        "aggiungi un piatto di pasta al forno con farina di grano tradizionale",
      );

      expect(risultato.modificaApplicata).toBe(false);
      expect(risultato.motivoRifiuto).toBeTruthy();
    },
    30_000,
  );

  it(
    "modificaPiano applica una richiesta compatibile con le restrizioni",
    async () => {
      const risultato = await modificaPiano(
        profiloCeliaco,
        creaPianoEsempio(),
        "giovedì mangio fuori, non serve cucinare",
      );

      expect(risultato.modificaApplicata).toBe(true);
      expect(risultato.giorni).toHaveLength(7);
    },
    30_000,
  );

  it(
    "adattaBudget restituisce un piano valido quando gli viene chiesto di abbassare il costo",
    async () => {
      const piano = await adattaBudget(profiloCeliaco, creaPianoEsempio(), 60, 25);

      expect(piano.giorni).toHaveLength(7);
      piano.giorni.forEach((g) => expect(g.pasti).toHaveLength(2));
    },
    30_000,
  );

  it(
    "regeneratePasto propone un pasto senza l'ingrediente a rischio segnalato",
    async () => {
      const pastoRischioso: Pasto = {
        tipo: "pranzo",
        nome: "Pasta al pomodoro",
        tempo_preparazione_min: 20,
        nutrizione: { calorie: 500, proteine_g: 15, carboidrati_g: 80, grassi_g: 10, fibre_g: 4 },
        preparazione: ["Cuoci la pasta.", "Condisci col pomodoro."],
        ingredienti: [
          { nome: "Pasta", quantita: 100, unita: "g", reparto: "Pane e cereali", prezzo_stimato_eur: 0.3 },
          { nome: "Pomodoro", quantita: 200, unita: "g", reparto: "Frutta e verdura", prezzo_stimato_eur: 0.6 },
        ],
      };

      const nuovoPasto = await regeneratePasto(profiloCeliaco, "Lunedì", pastoRischioso, ["Pasta"]);

      const rischi = ingredientiARischio(nuovoPasto.ingredienti.map((i) => i.nome));
      expect(rischi).toEqual([]);
    },
    30_000,
  );
});

if (!haChiaveApi) {
  console.log(
    "\nSmoke test saltati: nessuna ANTHROPIC_API_KEY (o ANTHROPIC_AUTH_TOKEN) nell'ambiente.\n" +
      "Impostala prima di eseguire `npm run test:smoke` per far girare davvero questi test.\n",
  );
}
