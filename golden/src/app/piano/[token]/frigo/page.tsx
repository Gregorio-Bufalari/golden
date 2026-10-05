import { createAdminClient } from "@/lib/supabase/admin";
import { conservazioneTipica, gruppoAcquisto, coloreScadenza, type ColoreScadenza } from "@/lib/conservazione";
import { PageHeader } from "../page-header";

const DOT_PER_COLORE: Record<ColoreScadenza, string> = {
  rosso: "bg-clay",
  arancione: "bg-honey",
  verde: "bg-accent",
};

function formatQuantita(quantita: number, unita: string): string {
  if (unita === "g" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} kg`;
  }
  if (unita === "ml" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} l`;
  }
  const arrotondata = Math.round(quantita * 10) / 10;
  return `${arrotondata} ${unita}`;
}

type Rimanenza = { ingrediente: string; unita: string; quantita: number };

function Sezione({ titolo, righe }: { titolo: string; righe: Rimanenza[] }) {
  if (righe.length === 0) return null;
  return (
    <div>
      <h2 className="mb-0.5 text-[13px] font-semibold text-ink/60">{titolo}</h2>
      <ul>
        {righe.map((r) => (
          <li
            key={`${r.ingrediente}-${r.unita}`}
            className="flex items-start justify-between gap-3 border-t border-ink/10 py-3 first:border-t-0"
          >
            <div className="min-w-0">
              <div className="flex items-center gap-1.5">
                <span
                  className={`h-[7px] w-[7px] shrink-0 rounded-full ${DOT_PER_COLORE[coloreScadenza(r.ingrediente)]}`}
                  aria-hidden="true"
                />
                <span className="text-[15px] text-ink">{r.ingrediente}</span>
              </div>
              <div className="mt-0.5 text-xs text-ink/55">{conservazioneTipica(r.ingrediente)}</div>
            </div>
            <span className="shrink-0 font-mono text-sm text-ink/70">
              {formatQuantita(r.quantita, r.unita)}
            </span>
          </li>
        ))}
      </ul>
    </div>
  );
}

export default async function FrigoPage({
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

  const { data: rimanenze } = await supabase
    .from("rimanenze")
    .select("ingrediente, unita, quantita, settimana")
    .eq("profile_id", profile.id)
    .order("ingrediente", { ascending: true });

  const righe: Rimanenza[] = (rimanenze || []).map((r) => ({
    ingrediente: r.ingrediente,
    unita: r.unita,
    quantita: Number(r.quantita),
  }));

  // Stessa classificazione già usata per dividere la Spesa per urgenza
  // d'acquisto: qui si traduce in urgenza di consumo.
  const presto = righe.filter((r) => gruppoAcquisto(r.ingrediente) === "subito");
  const dopo = righe.filter((r) => gruppoAcquisto(r.ingrediente) === "puo_aspettare");

  return (
    <div className="flex flex-1 flex-col">
      <PageHeader
        token={token}
        title="Frigo"
        subtitle="Quello che avanza dalla spesa di questa settimana, calcolato dai formati delle confezioni"
      />

      <div className="mx-auto flex w-full max-w-2xl flex-1 flex-col gap-5 px-5 pb-10">
        {righe.length > 0 ? (
          <>
            <Sezione titolo="Da consumare presto" righe={presto} />
            <Sezione titolo="Dura più a lungo" righe={dopo} />
          </>
        ) : (
          <p className="pt-10 text-center text-sm text-ink/55">Niente in dispensa al momento.</p>
        )}
      </div>
    </div>
  );
}
