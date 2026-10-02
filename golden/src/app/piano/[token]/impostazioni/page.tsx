import { createAdminClient } from "@/lib/supabase/admin";
import { ImpostazioniForm } from "./impostazioni-form";

export default async function ImpostazioniPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  const supabase = createAdminClient();

  const { data: profile } = await supabase
    .from("profiles")
    .select(
      "nome, restrizioni, household_size, obiettivo, preferenze, tempo_max_cucina, budget_settimanale, supermercato",
    )
    .eq("link_token", token)
    .single();

  if (!profile) {
    return null;
  }

  return (
    <div className="flex flex-1 flex-col items-center px-6 py-10">
      <h1 className="text-2xl font-semibold text-zinc-950 dark:text-zinc-50">Impostazioni</h1>
      <p className="mt-2 max-w-md text-center text-sm text-zinc-500 dark:text-zinc-400">
        Restrizioni, obiettivo, preferenze, budget e supermercato — si applicano dal prossimo
        piano generato.
      </p>

      <div className="mt-6 w-full max-w-md">
        <ImpostazioniForm token={token} profile={profile} />
      </div>
    </div>
  );
}
