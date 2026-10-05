import { createAdminClient } from "@/lib/supabase/admin";
import { PageHeader } from "../page-header";

type CheckinRow = {
  seguito_piano: boolean | null;
  spreco: boolean | null;
  categoria_spreco: string | null;
  spesa_reale: number | null;
  retailer_usato: string | null;
};

type WeeklyPlanRow = {
  id: string;
  settimana: string;
  budget_stimato: number | null;
  checkins: CheckinRow[];
};

export default async function AndamentoPage({
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

  const { data: piani } = await supabase
    .from("weekly_plans")
    .select("id, settimana, budget_stimato, checkins(seguito_piano, spreco, categoria_spreco, spesa_reale, retailer_usato)")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false });

  const settimane = (piani || []) as unknown as WeeklyPlanRow[];
  // Risparmio = quanto l'app aveva stimato per QUELLA settimana meno quanto hai
  // dichiarato di aver speso davvero — non il budget fisso impostato una volta
  // nel profilo, che è solo un vincolo per la generazione, non un termine di
  // paragone settimanale.
  const checkinsConSpesa = settimane
    .flatMap((s) => s.checkins.map((c) => ({ ...c, settimana: s.settimana, budget_stimato: s.budget_stimato })))
    .filter((c) => c.spesa_reale !== null && c.budget_stimato !== null);

  const risparmioCumulativo =
    checkinsConSpesa.length > 0
      ? checkinsConSpesa.reduce(
          (sum, c) => sum + ((c.budget_stimato as number) - (c.spesa_reale as number)),
          0,
        )
      : null;

  // Ultime (al massimo) 6 settimane con un risparmio calcolabile, in ordine
  // cronologico, per la barra sotto il totale.
  const ultimeSettimane = [...checkinsConSpesa]
    .reverse()
    .slice(-6)
    .map((c) => ({
      settimana: c.settimana,
      risparmio: (c.budget_stimato as number) - (c.spesa_reale as number),
    }));
  const massimoRisparmio = Math.max(1, ...ultimeSettimane.map((s) => Math.abs(s.risparmio)));

  const checkinsTotali = settimane.flatMap((s) => s.checkins);
  const checkinsConRisposta = checkinsTotali.filter((c) => c.spreco !== null);
  const percentualeSenzaSprechi =
    checkinsConRisposta.length > 0
      ? Math.round(
          (checkinsConRisposta.filter((c) => c.spreco === false).length / checkinsConRisposta.length) * 100,
        )
      : null;

  const categorieSpreco = checkinsTotali
    .filter((c) => c.spreco === true && c.categoria_spreco)
    .map((c) => c.categoria_spreco as string);
  const conteggioCategorie = categorieSpreco.reduce<Record<string, number>>((acc, cat) => {
    acc[cat] = (acc[cat] || 0) + 1;
    return acc;
  }, {});
  const categoriaPiuFrequente =
    Object.entries(conteggioCategorie).sort((a, b) => b[1] - a[1])[0]?.[0] || null;

  return (
    <div className="flex flex-1 flex-col">
      <PageHeader token={token} title="Andamento" />

      <div className="mx-auto flex w-full max-w-2xl flex-1 flex-col gap-4 px-5 pb-10">
        <div className="bg-panel rounded-[14px] px-5 py-[18px]">
          <div className="text-[13px] font-medium text-ink/65">Risparmiato rispetto al budget</div>
          <div className="mt-1 font-mono text-[28px] font-semibold text-ink">
            {risparmioCumulativo !== null ? `€${risparmioCumulativo.toFixed(2)}` : "—"}
          </div>
          <div className="mt-0.5 text-xs text-ink/55">
            {checkinsConSpesa.length > 0
              ? `Ultime ${checkinsConSpesa.length} settimane con check-in`
              : "Nessun check-in con spesa reale ancora"}
          </div>

          {ultimeSettimane.length > 1 && (
            <div className="mt-5 flex h-[110px] items-end gap-2.5">
              {ultimeSettimane.map((s, i) => {
                const positivo = s.risparmio >= 0;
                const altezza = Math.max(6, Math.round((Math.abs(s.risparmio) / massimoRisparmio) * 100));
                return (
                  <div key={`${s.settimana}-${i}`} className="flex flex-1 flex-col items-center justify-end gap-1.5">
                    <div
                      className={`w-full rounded-t-[3px] ${positivo ? "bg-accent" : "bg-clay"}`}
                      style={{ height: `${altezza}px` }}
                      title={`${s.settimana}: €${s.risparmio.toFixed(2)}`}
                    />
                    <span className="text-[10px] text-ink/45">{i + 1}</span>
                  </div>
                );
              })}
            </div>
          )}
        </div>

        <div className="bg-panel rounded-[14px] px-5 py-[18px]">
          <div className="text-[13px] font-medium text-ink/65">Sprechi dichiarati</div>
          <div className="mt-1 text-lg font-bold text-ink">
            {percentualeSenzaSprechi !== null
              ? `${percentualeSenzaSprechi}% delle settimane senza sprechi`
              : "Ancora nessun check-in"}
          </div>
          {categoriaPiuFrequente && (
            <p className="mt-1.5 text-[13px] text-ink/60">
              Quando capita, è quasi sempre {categoriaPiuFrequente.toLowerCase()}.
            </p>
          )}
        </div>

        <div>
          <h2 className="mb-0.5 text-[13px] font-semibold text-ink/60">
            Spesa stimata vs reale, settimana per settimana
          </h2>
          {settimane.length === 0 ? (
            <p className="py-3 text-sm text-ink/55">Nessun piano ancora generato.</p>
          ) : (
            <ul>
              {settimane.map((s) => {
                const checkin = s.checkins[0];
                return (
                  <li
                    key={s.id}
                    className="flex items-center justify-between gap-3 border-t border-ink/10 py-3 first:border-t-0"
                  >
                    <span className="text-[15px] text-ink">Settimana del {s.settimana}</span>
                    <span className="font-mono text-sm text-ink/70">
                      stimato €{s.budget_stimato?.toFixed(2) ?? "—"}
                      {checkin?.spesa_reale != null && <> · reale €{checkin.spesa_reale.toFixed(2)}</>}
                    </span>
                  </li>
                );
              })}
            </ul>
          )}
        </div>
      </div>
    </div>
  );
}
