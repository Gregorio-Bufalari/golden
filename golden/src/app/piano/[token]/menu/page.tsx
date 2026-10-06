import { createAdminClient } from "@/lib/supabase/admin";
import { MenuView } from "./menu-view";

export default async function MenuPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  const supabase = createAdminClient();

  const { data: profile, error: profileError } = await supabase
    .from("profiles")
    .select(
      "id, nome, restrizioni, modalita, budget_settimanale, household_size, sesso, eta, peso_kg, altezza_cm, livello_attivita",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    console.error("MenuPage profile fetch error:", profileError);
    return (
      <div className="flex flex-1 flex-col items-center justify-center px-6 py-10 text-center">
        <p className="text-sm text-ink/60">Non sono riuscito a caricare il profilo. Riprova tra poco.</p>
      </div>
    );
  }

  const { data: ultimoPiano } = await supabase
    .from("weekly_plans")
    .select("settimana, meal_plan, budget_stimato")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false })
    .limit(1)
    .maybeSingle();

  const { data: preferiti } = await supabase.from("preferiti").select("nome").eq("profile_id", profile.id);

  return (
    <div className="flex flex-1 flex-col">
      <MenuView
        token={token}
        nome={profile.nome}
        initialModalita={profile.modalita as "routine" | "scoperta"}
        initialGiorni={ultimoPiano?.meal_plan?.giorni || null}
        initialSettimana={ultimoPiano?.settimana || ""}
        preferitiIniziali={(preferiti || []).map((p) => p.nome)}
        householdSize={profile.household_size}
        budgetSettimanale={profile.budget_settimanale}
        budgetStimatoIniziale={ultimoPiano?.budget_stimato ?? null}
        datiBiometrici={
          profile.sesso && profile.eta && profile.peso_kg && profile.altezza_cm && profile.livello_attivita
            ? {
                sesso: profile.sesso as "M" | "F",
                eta: profile.eta,
                peso_kg: Number(profile.peso_kg),
                altezza_cm: Number(profile.altezza_cm),
                livello_attivita: profile.livello_attivita as "sedentario" | "moderato" | "attivo",
              }
            : null
        }
      />
    </div>
  );
}
