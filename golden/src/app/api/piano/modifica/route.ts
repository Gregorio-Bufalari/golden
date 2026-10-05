import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { modificaPiano, type ProfiloPerPiano, type Giorno } from "@/lib/claude";
import { validaGiorni, adattaEntroBudget } from "@/lib/piano-validazione";
import { leggiDispensa, dispensaSenzaVersione, sostituisciConsumiDispensa } from "@/lib/dispensa";
import type { GroceryList, ConsumoDispensa } from "@/lib/grocery";

// Vedi la stessa impostazione in /api/piano/generate: più chiamate a Claude
// in sequenza possono superare il limite di default di Vercel.
export const maxDuration = 300;

export async function POST(request: Request) {
  const { token, messaggio } = await request.json();

  if (!token || typeof token !== "string") {
    return NextResponse.json({ error: "Token mancante." }, { status: 400 });
  }
  if (!messaggio || typeof messaggio !== "string" || !messaggio.trim()) {
    return NextResponse.json({ error: "Messaggio mancante." }, { status: 400 });
  }

  const supabase = createAdminClient();

  const { data: profile, error: profileError } = await supabase
    .from("profiles")
    .select(
      "id, restrizioni, obiettivo, preferenze, tempo_max_cucina, household_size, supermercato, budget_settimanale",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    return NextResponse.json({ error: "Profilo non trovato." }, { status: 404 });
  }

  const { data: pianoAttuale } = await supabase
    .from("weekly_plans")
    .select("id, settimana, meal_plan, grocery_list, consumi_dispensa")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false })
    .limit(1)
    .maybeSingle();

  if (!pianoAttuale?.meal_plan?.giorni) {
    return NextResponse.json(
      { error: "Nessun piano da modificare: generane uno prima." },
      { status: 400 },
    );
  }

  const profiloInput: ProfiloPerPiano = {
    restrizioni: profile.restrizioni || [],
    obiettivo: profile.obiettivo,
    preferenze: profile.preferenze,
    tempo_max_cucina: profile.tempo_max_cucina,
    household_size: profile.household_size,
    budget_settimanale: profile.budget_settimanale,
  };

  const profileId: string = profile.id;
  const vecchioRimasto = (pianoAttuale.grocery_list as GroceryList | null)?.rimasto ?? [];
  const vecchiConsumi = (pianoAttuale.consumi_dispensa as ConsumoDispensa[] | null) ?? [];

  let giorniValidati;
  let groceryList;
  let budgetSuperato = false;
  let nuoviConsumi: ConsumoDispensa[] = [];

  // La dispensa attuale include già l'effetto della versione precedente di
  // QUESTO piano (consumi/avanzi applicati quando fu generato). Per
  // ricalcolare la nuova lista della spesa — e per segnalare all'AI cosa è
  // già disponibile — serve la dispensa "vera" di prima, altrimenti gli
  // avanzi non ancora reali di questa versione verrebbero trattati come
  // scorte già disponibili. Letta PRIMA di modificaPiano, così anche la
  // modifica stessa (es. "Proponi un piatto diverso", "Sostituisci X") può
  // tenerne conto.
  const dispensaAttuale = await leggiDispensa(supabase, profileId);
  const dispensaBase = dispensaSenzaVersione(dispensaAttuale, vecchiConsumi, vecchioRimasto);

  try {
    const risultato = await modificaPiano(
      profiloInput,
      pianoAttuale.meal_plan.giorni as Giorno[],
      messaggio.trim(),
      dispensaBase,
    );

    if (!risultato.modificaApplicata) {
      return NextResponse.json({
        modifica_applicata: false,
        motivo_rifiuto:
          risultato.motivoRifiuto ||
          "Non posso applicare questa modifica perché è incompatibile con le tue restrizioni alimentari.",
        settimana: pianoAttuale.settimana,
        giorni: pianoAttuale.meal_plan.giorni,
      });
    }

    const giorniBase = await validaGiorni(profiloInput, risultato.giorni, dispensaBase);
    const adattato = await adattaEntroBudget(
      profiloInput,
      giorniBase,
      profile.supermercato,
      profile.budget_settimanale,
      dispensaBase,
    );
    giorniValidati = adattato.giorni;
    groceryList = adattato.groceryList;
    budgetSuperato = adattato.budgetSuperato;
    nuoviConsumi = adattato.consumiDispensa;
  } catch (err) {
    console.error("modificaPiano error:", err);
    return NextResponse.json(
      { error: "Non sono riuscito ad applicare la modifica. Riprova." },
      { status: 502 },
    );
  }

  await sostituisciConsumiDispensa(
    supabase,
    profileId,
    dispensaAttuale,
    vecchiConsumi,
    vecchioRimasto,
    nuoviConsumi,
    groceryList.rimasto,
    pianoAttuale.settimana,
  );

  const { error: updateError } = await supabase
    .from("weekly_plans")
    .update({
      meal_plan: { giorni: giorniValidati },
      grocery_list: groceryList,
      consumi_dispensa: nuoviConsumi,
      budget_stimato: groceryList.totale_stimato,
    })
    .eq("id", pianoAttuale.id);

  if (updateError) {
    console.error("weekly_plans update error:", updateError);
    return NextResponse.json(
      { error: "Modifica applicata ma non sono riuscito a salvarla." },
      { status: 500 },
    );
  }

  return NextResponse.json({
    modifica_applicata: true,
    settimana: pianoAttuale.settimana,
    giorni: giorniValidati,
    grocery_list: groceryList,
    budget_superato: budgetSuperato,
    budget_settimanale: profile.budget_settimanale,
  });
}
