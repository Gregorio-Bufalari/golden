import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { MealPlanSchema, type ProfiloPerPiano } from "@/lib/claude";
import { validaGiorni, type GiornoValidato } from "@/lib/piano-validazione";
import { leggiDispensa, applicaConsumiDispensa } from "@/lib/dispensa";
import { calcolaFattoreCalibrazionePerProfilo } from "@/lib/calibrazione-prezzi";
import { segnaPreferitoProposto } from "@/lib/preferiti-scoperta";
import { buildGroceryList } from "@/lib/grocery";

export const maxDuration = 60;

/**
 * Salva lo scenario di budget scelto dall'utente tra quelli proposti da
 * /api/piano/generate (vedi "Scenari multipli di budget"). Riceve dal
 * client solo `giorni` (i 14 pasti dello scenario scelto, non modificati
 * dall'utente — solo scelto tra quelli mostrati): la lista della spesa e i
 * consumi dispensa si RICALCOLANO sempre qui lato server, non ci si fida
 * di quella mostrata nel confronto, così restano coerenti anche nel raro
 * caso in cui la rivalidazione di sicurezza sotto rigeneri un pasto.
 */
export async function POST(request: Request) {
  const { token, settimana, giorni, favorito_incluso_id } = await request.json();

  if (!token || typeof token !== "string") {
    return NextResponse.json({ error: "Token mancante." }, { status: 400 });
  }
  if (!settimana || typeof settimana !== "string") {
    return NextResponse.json({ error: "Settimana mancante." }, { status: 400 });
  }

  const pianoValido = MealPlanSchema.safeParse({ giorni });
  if (!pianoValido.success) {
    return NextResponse.json(
      { error: "Lo scenario scelto non è valido. Genera di nuovo il piano." },
      { status: 400 },
    );
  }

  const supabase = createAdminClient();

  const { data: profile, error: profileError } = await supabase
    .from("profiles")
    .select(
      "id, restrizioni, obiettivo, preferenze, tempo_max_cucina, household_size, modalita, supermercato, budget_settimanale, obiettivi_nutrizionali",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    return NextResponse.json({ error: "Profilo non trovato." }, { status: 404 });
  }

  const profileId: string = profile.id;
  const profiloInput: ProfiloPerPiano = {
    restrizioni: profile.restrizioni || [],
    obiettivo: profile.obiettivo,
    preferenze: profile.preferenze,
    tempo_max_cucina: profile.tempo_max_cucina,
    household_size: profile.household_size,
    budget_settimanale: profile.budget_settimanale,
    obiettivi_nutrizionali: profile.obiettivi_nutrizionali,
  };

  const dispensa = await leggiDispensa(supabase, profileId);
  const fattore = await calcolaFattoreCalibrazionePerProfilo(supabase, profileId, profile.supermercato);

  // Unica validazione di sicurezza dell'app (vedi piano-validazione.ts):
  // lo scenario era già stato controllato alla generazione, ma qui è la
  // prima volta che un piano arriva "echeggiato" dal client invece che da
  // Claude — stesso controllo di ogni altro punto che tocca il piano, mai
  // saltato.
  const giorniValidati: GiornoValidato[] = await validaGiorni(profiloInput, pianoValido.data.giorni, dispensa);
  const { groceryList, consumiDispensa } = buildGroceryList(giorniValidati, profile.supermercato, dispensa, fattore);

  await applicaConsumiDispensa(supabase, profileId, dispensa, consumiDispensa, groceryList.rimasto, settimana);

  if (favorito_incluso_id && typeof favorito_incluso_id === "string") {
    await segnaPreferitoProposto(supabase, favorito_incluso_id);
  }

  const { data: weeklyPlan, error: insertError } = await supabase
    .from("weekly_plans")
    .insert({
      profile_id: profileId,
      settimana,
      meal_plan: { giorni: giorniValidati },
      modalita_usata: profile.modalita,
      budget_stimato: groceryList.totale_stimato,
      grocery_list: groceryList,
      consumi_dispensa: consumiDispensa,
    })
    .select("id, settimana")
    .single();

  if (insertError || !weeklyPlan) {
    console.error("weekly_plans insert error:", insertError);
    return NextResponse.json({ error: "Piano scelto ma non sono riuscito a salvarlo." }, { status: 500 });
  }

  return NextResponse.json({
    weekly_plan_id: weeklyPlan.id,
    settimana: weeklyPlan.settimana,
    giorni: giorniValidati,
    grocery_list: groceryList,
    budget_superato: Boolean(profile.budget_settimanale && groceryList.totale_stimato > profile.budget_settimanale),
    budget_settimanale: profile.budget_settimanale,
  });
}
