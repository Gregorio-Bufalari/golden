import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { generateMealPlan, type ProfiloPerPiano } from "@/lib/claude";
import { validaGiorni, adattaEntroBudget, type GiornoValidato } from "@/lib/piano-validazione";
import { leggiDispensa, applicaConsumiDispensa } from "@/lib/dispensa";
import type { GroceryList } from "@/lib/grocery";

// Generare un piano può richiedere diverse chiamate a Claude in sequenza
// (generazione, eventuali rigenerazioni per il glutine, adattamento al
// budget): il limite di default di Vercel per una funzione serverless è
// troppo basso e interromperebbe la richiesta a metà.
export const maxDuration = 60;

function mondayOfThisWeek(d = new Date()): string {
  const day = d.getDay();
  const diffToMonday = day === 0 ? -6 : 1 - day;
  const monday = new Date(d);
  monday.setDate(d.getDate() + diffToMonday);
  return monday.toISOString().slice(0, 10);
}

export async function POST(request: Request) {
  const { token } = await request.json();

  if (!token || typeof token !== "string") {
    return NextResponse.json({ error: "Token mancante." }, { status: 400 });
  }

  const supabase = createAdminClient();

  const { data: profile, error: profileError } = await supabase
    .from("profiles")
    .select(
      "id, restrizioni, obiettivo, preferenze, tempo_max_cucina, household_size, modalita, supermercato, budget_settimanale",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    return NextResponse.json({ error: "Profilo non trovato." }, { status: 404 });
  }

  const profileId: string = profile.id;
  const supermercato = profile.supermercato;
  const budgetSettimanale = profile.budget_settimanale;
  const profiloInput: ProfiloPerPiano = {
    restrizioni: profile.restrizioni || [],
    obiettivo: profile.obiettivo,
    preferenze: profile.preferenze,
    tempo_max_cucina: profile.tempo_max_cucina,
    household_size: profile.household_size,
    budget_settimanale: profile.budget_settimanale,
  };

  const settimana = mondayOfThisWeek();

  let giorniValidati: GiornoValidato[];
  let groceryList: GroceryList;
  let riusato = false;
  let budgetSuperato = false;

  async function generaFresco() {
    const dispensa = await leggiDispensa(supabase, profileId);
    const plan = await generateMealPlan(profiloInput);
    const giorniBase = await validaGiorni(profiloInput, plan.giorni);
    const risultato = await adattaEntroBudget(
      profiloInput,
      giorniBase,
      supermercato,
      budgetSettimanale,
      dispensa,
    );
    await applicaConsumiDispensa(
      supabase,
      profileId,
      dispensa,
      risultato.consumiDispensa,
      risultato.groceryList.rimasto,
      settimana,
    );
    return risultato;
  }

  if (profile.modalita === "routine") {
    const { data: ultimoPiano } = await supabase
      .from("weekly_plans")
      .select("meal_plan, grocery_list")
      .eq("profile_id", profile.id)
      .eq("modalita_usata", "routine")
      .order("settimana", { ascending: false })
      .limit(1)
      .maybeSingle();

    const groceryListRiusabile = (ultimoPiano?.grocery_list as GroceryList | null)?.reparti;

    if (ultimoPiano?.meal_plan?.giorni && groceryListRiusabile) {
      giorniValidati = ultimoPiano.meal_plan.giorni;
      groceryList = ultimoPiano.grocery_list as GroceryList;
      riusato = true;
    } else {
      try {
        const risultato = await generaFresco();
        giorniValidati = risultato.giorni;
        groceryList = risultato.groceryList;
        budgetSuperato = risultato.budgetSuperato;
      } catch (err) {
        console.error("generateMealPlan error:", err);
        return NextResponse.json(
          { error: "Non sono riuscito a generare il piano. Riprova." },
          { status: 502 },
        );
      }
    }
  } else {
    try {
      const risultato = await generaFresco();
      giorniValidati = risultato.giorni;
      groceryList = risultato.groceryList;
      budgetSuperato = risultato.budgetSuperato;
    } catch (err) {
      console.error("generateMealPlan error:", err);
      return NextResponse.json(
        { error: "Non sono riuscito a generare il piano. Riprova." },
        { status: 502 },
      );
    }
  }

  const { data: weeklyPlan, error: insertError } = await supabase
    .from("weekly_plans")
    .insert({
      profile_id: profile.id,
      settimana,
      meal_plan: { giorni: giorniValidati },
      modalita_usata: profile.modalita,
      budget_stimato: groceryList.totale_stimato,
      grocery_list: groceryList,
    })
    .select("id, settimana")
    .single();

  if (insertError || !weeklyPlan) {
    console.error("weekly_plans insert error:", insertError);
    return NextResponse.json(
      { error: "Piano generato ma non sono riuscito a salvarlo." },
      { status: 500 },
    );
  }

  return NextResponse.json({
    weekly_plan_id: weeklyPlan.id,
    settimana: weeklyPlan.settimana,
    giorni: giorniValidati,
    grocery_list: groceryList,
    riusato,
    budget_superato: budgetSuperato,
    budget_settimanale: profile.budget_settimanale,
  });
}
