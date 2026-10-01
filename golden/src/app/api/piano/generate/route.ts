import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { generateMealPlan, type ProfiloPerPiano } from "@/lib/claude";
import { validaGiorni, type GiornoValidato } from "@/lib/piano-validazione";
import { buildGroceryList, type GroceryList } from "@/lib/grocery";

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
      "id, restrizioni, obiettivo, preferenze, tempo_max_cucina, household_size, modalita, supermercato",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    return NextResponse.json({ error: "Profilo non trovato." }, { status: 404 });
  }

  const profiloInput: ProfiloPerPiano = {
    restrizioni: profile.restrizioni || [],
    obiettivo: profile.obiettivo,
    preferenze: profile.preferenze,
    tempo_max_cucina: profile.tempo_max_cucina,
    household_size: profile.household_size,
  };

  let giorniValidati: GiornoValidato[];
  let groceryList: GroceryList;
  let riusato = false;

  if (profile.modalita === "routine") {
    const { data: ultimoPiano } = await supabase
      .from("weekly_plans")
      .select("meal_plan, grocery_list")
      .eq("profile_id", profile.id)
      .order("settimana", { ascending: false })
      .limit(1)
      .maybeSingle();

    if (ultimoPiano?.meal_plan?.giorni) {
      giorniValidati = ultimoPiano.meal_plan.giorni;
      groceryList = ultimoPiano.grocery_list as GroceryList;
      riusato = true;
    } else {
      try {
        const plan = await generateMealPlan(profiloInput);
        giorniValidati = await validaGiorni(profiloInput, plan.giorni);
      } catch (err) {
        console.error("generateMealPlan error:", err);
        return NextResponse.json(
          { error: "Non sono riuscito a generare il piano. Riprova." },
          { status: 502 },
        );
      }
      groceryList = buildGroceryList(giorniValidati, profile.supermercato);
    }
  } else {
    try {
      const plan = await generateMealPlan(profiloInput);
      giorniValidati = await validaGiorni(profiloInput, plan.giorni);
    } catch (err) {
      console.error("generateMealPlan error:", err);
      return NextResponse.json(
        { error: "Non sono riuscito a generare il piano. Riprova." },
        { status: 502 },
      );
    }
    groceryList = buildGroceryList(giorniValidati, profile.supermercato);
  }

  const settimana = mondayOfThisWeek();

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
  });
}
