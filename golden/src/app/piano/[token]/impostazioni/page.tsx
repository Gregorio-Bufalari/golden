import { createAdminClient } from "@/lib/supabase/admin";
import { ImpostazioniView } from "./impostazioni-view";
import { PageHeader } from "../page-header";

export default async function ImpostazioniPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  const supabase = createAdminClient();

  const { data: profile, error: profileError } = await supabase
    .from("profiles")
    .select(
      "nome, restrizioni, household_size, obiettivo, preferenze, tempo_max_cucina, budget_settimanale, supermercato, sesso, eta, peso_kg, altezza_cm, livello_attivita",
    )
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    console.error("ImpostazioniPage profile fetch error:", profileError);
    return (
      <div className="flex flex-1 flex-col items-center justify-center px-6 py-10 text-center">
        <p className="text-sm text-ink/60">Non sono riuscito a caricare il profilo. Riprova tra poco.</p>
      </div>
    );
  }

  return (
    <div className="flex flex-1 flex-col">
      <PageHeader
        token={token}
        title="Profilo"
        subtitle="Restrizioni, obiettivo, preferenze, budget e supermercato. Si applicano dal prossimo piano generato."
      />

      <div className="mx-auto w-full max-w-md flex-1 px-5">
        <ImpostazioniView token={token} profile={profile} />
      </div>
    </div>
  );
}
