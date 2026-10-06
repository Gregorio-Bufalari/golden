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
import { ingredientiDaSegnalare } from "./glutine-check";
import { validaGiorni, assicuraVarieta, adattaEntroBudget } from "./piano-validazione";
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
          const rischi = ingredientiDaSegnalare(pasto.ingredienti.map((i) => i.nome));
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

      const rischi = ingredientiDaSegnalare(nuovoPasto.ingredienti.map((i) => i.nome));
      expect(rischi).toEqual([]);
    },
    30_000,
  );

  it(
    // Stessa pipeline del pulsante "Non l'ho trovato" nella lista della
    // spesa: modificaPiano per sostituire l'ingrediente, poi validaGiorni
    // (sempre, come fa /api/piano/modifica) a garanzia che il sostituto
    // proposto dall'AI non reintroduca un rischio glutine non segnalato.
    "\"Non l'ho trovato\": sostituire un ingrediente a rischio (Pasta) passa comunque dalla validazione glutine",
    async () => {
      const pianoConRischio = creaPianoEsempio();
      const risultato = await modificaPiano(
        profiloCeliaco,
        pianoConRischio,
        "Sostituisci Pasta con un'alternativa sicura e simile.",
      );

      expect(risultato.modificaApplicata).toBe(true);

      const giorniValidati = await validaGiorni(profiloCeliaco, risultato.giorni);

      for (const giorno of giorniValidati) {
        for (const pasto of giorno.pasti) {
          const rischi = ingredientiDaSegnalare(pasto.ingredienti.map((i) => i.nome));
          if (rischi.length > 0) {
            // Un rischio residuo è accettabile solo se la pipeline di
            // sicurezza lo ha segnalato esplicitamente per la verifica
            // manuale — mai in silenzio.
            expect(pasto.verificare, `${giorno.giorno} ${pasto.tipo}: ${rischi.join(", ")}`).toBe(true);
            expect(pasto.ingredienti_a_rischio).toEqual(expect.arrayContaining(rischi));
          }
        }
      }
    },
    30_000,
  );

  it(
    // Scenario reale segnalato dall'utente: spinaci, riso e uova avanzati in
    // dispensa da settimane precedenti. Verifica che generateMealPlan non si
    // limiti a menzionare il criterio nel prompt (già coperto dai test con
    // risposta simulata in claude.test.ts) ma che l'AI reale lo segua
    // davvero, usando almeno uno di questi ingredienti nel nuovo piano
    // invece di lasciarli completamente fuori.
    "generateMealPlan usa davvero gli ingredienti avanzati in dispensa quando possibile (riduzione sprechi)",
    async () => {
      const dispensa = new Map([
        ["spinaci__g", 450],
        ["riso__g", 670],
        ["uova__pz", 1],
      ]);

      const piano = await generateMealPlan(profiloCeliaco, "routine", dispensa);

      const tuttiIngredienti = piano.giorni
        .flatMap((g) => g.pasti)
        .flatMap((p) => p.ingredienti.map((i) => i.nome.toLowerCase()));

      const usaAlmenoUnAvanzo =
        tuttiIngredienti.some((n) => n.includes("spinaci")) ||
        tuttiIngredienti.some((n) => n.includes("riso")) ||
        tuttiIngredienti.some((n) => n.includes("uov"));

      expect(usaAlmenoUnAvanzo, `ingredienti nel piano: ${[...new Set(tuttiIngredienti)].join(", ")}`).toBe(
        true,
      );
    },
    60_000,
  );

  it(
    // Stessa pipeline del pulsante "Proponine un altro" nel Menu:
    // modificaPiano per un piatto diverso sullo stesso giorno/pasto, poi
    // validaGiorni (sempre, come fa /api/piano/modifica). Punta
    // deliberatamente al pranzo di Lunedì ("Pasta al pomodoro", a rischio
    // glutine) per verificare che anche il NUOVO piatto proposto non
    // reintroduca un rischio senza segnalazione.
    "\"Proponine un altro\": un piatto diverso per un pasto a rischio passa comunque dalla validazione glutine",
    async () => {
      const pianoConRischio = creaPianoEsempio();
      const risultato = await modificaPiano(
        profiloCeliaco,
        pianoConRischio,
        "Proponi un piatto diverso per Lunedì pranzo, stesse restrizioni e preferenze.",
      );

      expect(risultato.modificaApplicata).toBe(true);

      const giorniValidati = await validaGiorni(profiloCeliaco, risultato.giorni);
      const pranzoLunedi = giorniValidati[0].pasti.find((p) => p.tipo === "pranzo")!;

      // Il piatto deve essere stato effettivamente cambiato, non solo
      // "ri-approvato" uguale a prima.
      expect(pranzoLunedi.nome).not.toBe("Pasta al pomodoro");

      for (const giorno of giorniValidati) {
        for (const pasto of giorno.pasti) {
          const rischi = ingredientiDaSegnalare(pasto.ingredienti.map((i) => i.nome));
          if (rischi.length > 0) {
            expect(pasto.verificare, `${giorno.giorno} ${pasto.tipo}: ${rischi.join(", ")}`).toBe(true);
            expect(pasto.ingredienti_a_rischio).toEqual(expect.arrayContaining(rischi));
          }
        }
      }
    },
    30_000,
  );

  it(
    // Scenario esatto segnalato dall'utente: profilo celiachia + obiettivo
    // "Ridurre gli sprechi" produceva praticamente lo stesso piatto
    // ripetuto tutta la settimana e solo 3 ingredienti da comprare in
    // tutto. Verifica la pipeline di produzione COMPLETA (generazione +
    // controllo glutine + controllo varietà + adattamento budget), non solo
    // generateMealPlan da sola, perché il collasso nasceva dall'interazione
    // tra "ridurre gli sprechi" e l'adattamento al budget.
    "profilo celiachia + \"Ridurre gli sprechi\" produce 14 ricette distinte e una spesa con più di 3 ingredienti",
    async () => {
      const profiloSprechi: ProfiloPerPiano = {
        restrizioni: ["Glutine (celiachia)"],
        obiettivo: "Ridurre gli sprechi",
        preferenze: null,
        tempo_max_cucina: 30,
        household_size: 2,
        budget_settimanale: 60,
      };

      const piano = await generateMealPlan(profiloSprechi, "routine");
      const giorniBase = await validaGiorni(profiloSprechi, piano.giorni);
      const giorniVari = await assicuraVarieta(profiloSprechi, giorniBase);
      const risultato = await adattaEntroBudget(profiloSprechi, giorniVari, "Conad", 60);

      const nomi = risultato.giorni.flatMap((g) => g.pasti.map((p) => p.nome));
      const nomiDistinti = new Set(nomi.map((n) => n.trim().toLowerCase()));
      expect(nomiDistinti.size, `piatti: ${nomi.join(", ")}`).toBe(nomi.length);

      const ingredientiDistinti = new Set(
        risultato.groceryList.reparti.flatMap((r) => r.items.map((i) => i.nome.toLowerCase())),
      );
      expect(ingredientiDistinti.size, `ingredienti: ${[...ingredientiDistinti].join(", ")}`).toBeGreaterThan(5);
    },
    120_000,
  );
});

if (!haChiaveApi) {
  console.log(
    "\nSmoke test saltati: nessuna ANTHROPIC_API_KEY (o ANTHROPIC_AUTH_TOKEN) nell'ambiente.\n" +
      "Impostala prima di eseguire `npm run test:smoke` per far girare davvero questi test.\n",
  );
}
