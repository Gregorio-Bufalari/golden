import Link from "next/link";
import { createAdminClient } from "@/lib/supabase/admin";
import { PageHeader } from "../page-header";
import { formattaData } from "@/lib/settimana";

// Elenco delle settimane passate (versione semplice, sola lettura): nessuna
// modifica/scambio/chat qui, solo consultazione. I dati esistono già in
// weekly_plans (una riga per settimana, con la sua `settimana` reale), non
// serve nessuna tabella nuova.
export default async function ArchivioPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  const supabase = createAdminClient();

  const { data: profile, error: profileError } = await supabase
    .from("profiles")
    .select("id")
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    console.error("ArchivioPage profile fetch error:", profileError);
    return (
      <div className="flex flex-1 flex-col items-center justify-center px-6 py-10 text-center">
        <p className="text-sm text-ink/60">Non sono riuscito a caricare il profilo. Riprova tra poco.</p>
      </div>
    );
  }

  const { data: piani } = await supabase
    .from("weekly_plans")
    .select("settimana")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false });

  const settimaneUniche = Array.from(new Set((piani || []).map((p) => p.settimana as string)));

  return (
    <div className="flex flex-1 flex-col">
      <PageHeader token={token} title="Archivio" subtitle="Le settimane passate, con le date reali di allora." />

      <div className="mx-auto w-full max-w-md flex-1 px-5 pb-10">
        {settimaneUniche.length === 0 ? (
          <p className="text-sm text-ink/60">Nessun piano salvato ancora.</p>
        ) : (
          <div className="flex flex-col gap-2.5">
            {settimaneUniche.map((settimana) => (
              <Link
                key={settimana}
                href={`/piano/${token}/archivio/${settimana}`}
                className="flex min-h-11 items-center rounded-[14px] bg-paper px-5 py-[18px] text-[15px] font-medium text-ink"
              >
                Settimana del {formattaData(new Date(`${settimana}T00:00:00`))}
              </Link>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
