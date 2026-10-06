"use server";

import { createAdminClient } from "@/lib/supabase/admin";

export async function setModalita(
  token: string,
  modalita: "routine" | "scoperta",
): Promise<{ success: true } | { error: string }> {
  const supabase = createAdminClient();

  const { error } = await supabase
    .from("profiles")
    .update({ modalita })
    .eq("link_token", token);

  if (error) {
    console.error("setModalita error:", error);
    return { error: "Non sono riuscito ad aggiornare la modalità." };
  }

  return { success: true };
}

type GiornoPiano = { giorno: string; pasti: unknown[] };

/**
 * Scambia due pasti tra giorni diversi dell'ultimo piano: puro riordino,
 * nessuna chiamata AI, nessun ricalcolo di lista della spesa o budget
 * (stessi 14 pasti, stessi ingredienti totali in settimana — cambia solo
 * in che giorno si mangia cosa). Lo scambio avviene lato server sui dati
 * già salvati (mai sul meal_plan intero mandato dal client), così non può
 * mai introdurre contenuto nuovo nel piano, solo riordinarlo.
 */
export async function scambiaPasti(
  token: string,
  giornoA: string,
  indiceA: number,
  giornoB: string,
  indiceB: number,
): Promise<{ success: true; giorni: GiornoPiano[] } | { error: string }> {
  if (giornoA === giornoB) {
    return { error: "Scegli due giorni diversi." };
  }

  const supabase = createAdminClient();

  const { data: profile } = await supabase
    .from("profiles")
    .select("id")
    .eq("link_token", token)
    .single();

  if (!profile) {
    return { error: "Profilo non trovato." };
  }

  const { data: ultimoPiano } = await supabase
    .from("weekly_plans")
    .select("id, meal_plan")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false })
    .limit(1)
    .maybeSingle();

  if (!ultimoPiano) {
    return { error: "Nessun piano trovato." };
  }

  const giorni: GiornoPiano[] | undefined = ultimoPiano.meal_plan?.giorni;
  if (!giorni) {
    return { error: "Nessun piano trovato." };
  }

  const giornoARecord = giorni.find((g) => g.giorno === giornoA);
  const giornoBRecord = giorni.find((g) => g.giorno === giornoB);
  const pastoA = giornoARecord?.pasti[indiceA];
  const pastoB = giornoBRecord?.pasti[indiceB];

  if (!giornoARecord || !giornoBRecord || !pastoA || !pastoB) {
    return { error: "Pasto non trovato." };
  }

  giornoARecord.pasti[indiceA] = pastoB;
  giornoBRecord.pasti[indiceB] = pastoA;

  const { error } = await supabase
    .from("weekly_plans")
    .update({ meal_plan: { giorni } })
    .eq("id", ultimoPiano.id);

  if (error) {
    console.error("scambiaPasti error:", error);
    return { error: "Non sono riuscito a salvare lo scambio." };
  }

  return { success: true, giorni };
}
