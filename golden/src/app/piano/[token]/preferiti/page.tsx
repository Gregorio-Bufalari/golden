import { createAdminClient } from "@/lib/supabase/admin";
import { PageHeader } from "../page-header";
import { PreferitiView } from "./preferiti-view";

export default async function PreferitiPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  const supabase = createAdminClient();

  const { data: profile } = await supabase
    .from("profiles")
    .select("id")
    .eq("link_token", token)
    .single();

  if (!profile) {
    return null;
  }

  const { data: preferiti } = await supabase
    .from("preferiti")
    .select("nome, tipo, ingredienti, tempo_preparazione_min, nutrizione, preparazione")
    .eq("profile_id", profile.id)
    .order("created_at", { ascending: false });

  return (
    <div className="flex flex-1 flex-col">
      <PageHeader
        token={token}
        title="Preferiti"
        subtitle="I piatti che hai salvato dal Menu. Versione semplice: non influenzano i piani futuri."
      />

      <div className="mx-auto w-full max-w-2xl flex-1 px-5 pb-10">
        <PreferitiView token={token} initialPreferiti={preferiti || []} />
      </div>
    </div>
  );
}
