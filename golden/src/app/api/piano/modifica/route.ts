import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { modificaPiano, type ProfiloPerPiano, type Giorno } from "@/lib/claude";
import { validaGiorni } from "@/lib/piano-validazione";
import { buildGroceryList } from "@/lib/grocery";

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
      "id, restrizioni, obiettivo, preferenze, tempo_max_cucina, household_size, supermercato",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    return NextResponse.json({ error: "Profilo non trovato." }, { status: 404 });
  }

  const { data: pianoAttuale } = await supabase
    .from("weekly_plans")
    .select("id, settimana, meal_plan")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
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
  };

  let giorniValidati;
  try {
    const nuovoPiano = await modificaPiano(
      profiloInput,
      pianoAttuale.meal_plan.giorni as Giorno[],
      messaggio.trim(),
    );
    giorniValidati = await validaGiorni(profiloInput, nuovoPiano.giorni);
  } catch (err) {
    console.error("modificaPiano error:", err);
    return NextResponse.json(
      { error: "Non sono riuscito ad applicare la modifica. Riprova." },
      { status: 502 },
    );
  }

  const groceryList = buildGroceryList(giorniValidati, profile.supermercato);

  const { error: updateError } = await supabase
    .from("weekly_plans")
    .update({
      meal_plan: { giorni: giorniValidati },
      grocery_list: groceryList,
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
    settimana: pianoAttuale.settimana,
    giorni: giorniValidati,
    grocery_list: groceryList,
  });
}
