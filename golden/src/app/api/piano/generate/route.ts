import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { generateMealPlan, regeneratePasto, type Pasto } from "@/lib/claude";
import { ingredientiARischio } from "@/lib/glutine-check";

const MAX_RIGENERAZIONI = 2;

type PastoValidato = Pasto & {
  verificare?: boolean;
  ingredienti_a_rischio?: string[];
};

type GiornoValidato = {
  giorno: string;
  pasti: PastoValidato[];
};

type ProfiloRow = {
  id: string;
  restrizioni: string[];
  obiettivo: string | null;
  preferenze: { cucina?: string[]; graditi?: string; non_graditi?: string } | null;
  tempo_max_cucina: number | null;
  household_size: number | null;
  modalita: "routine" | "scoperta";
};

function mondayOfThisWeek(d = new Date()): string {
  const day = d.getDay();
  const diffToMonday = day === 0 ? -6 : 1 - day;
  const monday = new Date(d);
  monday.setDate(d.getDate() + diffToMonday);
  return monday.toISOString().slice(0, 10);
}

async function generaPianoValidato(profile: ProfiloRow): Promise<GiornoValidato[]> {
  const richiedeControlloGlutine = profile.restrizioni?.includes("Glutine (celiachia)");

  const plan = await generateMealPlan({
    restrizioni: profile.restrizioni || [],
    obiettivo: profile.obiettivo,
    preferenze: profile.preferenze,
    tempo_max_cucina: profile.tempo_max_cucina,
    household_size: profile.household_size,
  });

  const giorniValidati: GiornoValidato[] = [];

  for (const giorno of plan.giorni) {
    const pastiValidati: PastoValidato[] = [];

    for (const pasto of giorno.pasti) {
      let pastoCorrente: PastoValidato = pasto;

      if (richiedeControlloGlutine) {
        let rischi = ingredientiARischio(pastoCorrente.ingredienti);
        let tentativi = 0;

        while (rischi.length > 0 && tentativi < MAX_RIGENERAZIONI) {
          tentativi += 1;
          try {
            pastoCorrente = await regeneratePasto(
              {
                restrizioni: profile.restrizioni || [],
                obiettivo: profile.obiettivo,
                preferenze: profile.preferenze,
                tempo_max_cucina: profile.tempo_max_cucina,
                household_size: profile.household_size,
              },
              giorno.giorno,
              pastoCorrente,
              rischi,
            );
            rischi = ingredientiARischio(pastoCorrente.ingredienti);
          } catch (err) {
            console.error("regeneratePasto error:", err);
            break;
          }
        }

        if (rischi.length > 0) {
          pastoCorrente = { ...pastoCorrente, verificare: true, ingredienti_a_rischio: rischi };
        }
      }

      pastiValidati.push(pastoCorrente);
    }

    giorniValidati.push({ giorno: giorno.giorno, pasti: pastiValidati });
  }

  return giorniValidati;
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
      "id, restrizioni, obiettivo, preferenze, tempo_max_cucina, household_size, modalita",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    return NextResponse.json({ error: "Profilo non trovato." }, { status: 404 });
  }

  let giorniValidati: GiornoValidato[];
  let budgetStimato: number | null = null;
  let riusato = false;

  if (profile.modalita === "routine") {
    const { data: ultimoPiano } = await supabase
      .from("weekly_plans")
      .select("meal_plan, budget_stimato")
      .eq("profile_id", profile.id)
      .order("settimana", { ascending: false })
      .limit(1)
      .maybeSingle();

    if (ultimoPiano?.meal_plan?.giorni) {
      giorniValidati = ultimoPiano.meal_plan.giorni;
      budgetStimato = ultimoPiano.budget_stimato;
      riusato = true;
    } else {
      try {
        giorniValidati = await generaPianoValidato(profile);
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
      giorniValidati = await generaPianoValidato(profile);
    } catch (err) {
      console.error("generateMealPlan error:", err);
      return NextResponse.json(
        { error: "Non sono riuscito a generare il piano. Riprova." },
        { status: 502 },
      );
    }
  }

  const settimana = mondayOfThisWeek();

  const { data: weeklyPlan, error: insertError } = await supabase
    .from("weekly_plans")
    .insert({
      profile_id: profile.id,
      settimana,
      meal_plan: { giorni: giorniValidati },
      modalita_usata: profile.modalita,
      budget_stimato: budgetStimato,
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
    riusato,
  });
}
