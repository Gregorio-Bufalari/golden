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
      "id, nome, restrizioni, modalita, budget_settimanale, sesso, eta, peso_kg, altezza_cm, livello_attivita",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    console.error("MenuPage profile fetch error:", profileError);
    return (
      <div className="flex flex-1 flex-col items-center justify-center px-6 py-10 text-center">
        <p className="text-sm text-zinc-500 dark:text-zinc-400">
          Non sono riuscito a caricare il profilo. Riprova tra poco.
        </p>
      </div>
    );
  }

  const { data: ultimoPiano } = await supabase
    .from("weekly_plans")
    .select("settimana, meal_plan, budget_stimato")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .limit(1)
    .maybeSingle();

  return (
    <div className="flex flex-1 flex-col items-center px-6 py-10 text-center">
      <h1 className="text-2xl font-semibold text-zinc-950 dark:text-zinc-50">
        Ciao {profile.nome}!
      </h1>
      {profile.restrizioni?.length > 0 && (
        <p className="mt-2 text-sm text-zinc-500 dark:text-zinc-500">
          Restrizioni: {profile.restrizioni.join(", ")}
        </p>
      )}

      <MenuView
        token={token}
        initialModalita={profile.modalita as "routine" | "scoperta"}
        initialGiorni={ultimoPiano?.meal_plan?.giorni || null}
        initialSettimana={ultimoPiano?.settimana || ""}
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
