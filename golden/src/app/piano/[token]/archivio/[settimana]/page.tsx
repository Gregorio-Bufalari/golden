import { createAdminClient } from "@/lib/supabase/admin";
import { PageHeader } from "../../page-header";
import { etichettaGiorno } from "@/lib/settimana";
import { CITAZIONE_CREA } from "@/lib/nutrizione-crea";
import type { GiornoValidato } from "@/lib/piano-validazione";

// Dettaglio di sola lettura di una settimana passata: stesse date reali di
// allora (derivate da QUESTA `settimana`, mai da oggi — vedi settimana.ts),
// nessuna interazione (niente scambio pasti, niente chat di modifica):
// questa è storia, non il piano attivo.
export default async function ArchivioSettimanaPage({
  params,
}: {
  params: Promise<{ token: string; settimana: string }>;
}) {
  const { token, settimana } = await params;
  const supabase = createAdminClient();

  const { data: profile, error: profileError } = await supabase
    .from("profiles")
    .select("id")
    .eq("link_token", token)
    .single();

  if (profileError || !profile) {
    console.error("ArchivioSettimanaPage profile fetch error:", profileError);
    return (
      <div className="flex flex-1 flex-col items-center justify-center px-6 py-10 text-center">
        <p className="text-sm text-ink/60">Non sono riuscito a caricare il profilo. Riprova tra poco.</p>
      </div>
    );
  }

  const { data: piano } = await supabase
    .from("weekly_plans")
    .select("meal_plan")
    .eq("profile_id", profile.id)
    .eq("settimana", settimana)
    .order("created_at", { ascending: false })
    .limit(1)
    .maybeSingle();

  const giorni = piano?.meal_plan?.giorni as GiornoValidato[] | undefined;

  return (
    <div className="flex flex-1 flex-col">
      <PageHeader token={token} title="Archivio" subtitle="Sola lettura — le date sono quelle reali di questa settimana." />

      <div className="mx-auto w-full max-w-md flex-1 px-5 pb-10">
        {!giorni ? (
          <p className="text-sm text-ink/60">Nessun piano trovato per questa settimana.</p>
        ) : (
          <div className="flex flex-col gap-7">
            {giorni.map((giorno) => (
              <div key={giorno.giorno}>
                <h3 className="mb-2.5 text-[15px] font-semibold text-ink">
                  {etichettaGiorno(giorno.giorno, settimana)}
                </h3>
                <div className="flex flex-col gap-3">
                  {giorno.pasti.map((pasto, i) => (
                    <div key={i} className="rounded-[14px] bg-panel px-5 py-[18px]">
                      <div className="text-[13px] font-medium text-ink/60">
                        {pasto.tipo === "pranzo" ? "Pranzo" : "Cena"} · {pasto.tempo_preparazione_min} min
                      </div>
                      <div className="mt-1 text-[17px] font-bold leading-tight tracking-tight text-ink">
                        {pasto.nome}
                      </div>
                      <p className="mt-1.5 text-[13px] text-ink/65">
                        {pasto.ingredienti.map((ing) => ing.nome).join(", ")}
                      </p>
                      <div className="mt-2 font-mono text-[13px] text-ink/75">{pasto.nutrizione.calorie} kcal</div>
                      <p className="mt-1 text-[11px] text-ink/45">
                        {pasto.fonte_nutrizione === "crea_inran" ? CITAZIONE_CREA : "Stima AI"}
                      </p>
                    </div>
                  ))}
                </div>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
