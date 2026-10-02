import { createAdminClient } from "@/lib/supabase/admin";

type CheckinRow = {
  seguito_piano: boolean | null;
  spreco: boolean | null;
  categoria_spreco: string | null;
  spesa_reale: number | null;
  retailer_usato: string | null;
};

type WeeklyPlanRow = {
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
    .select("settimana, budget_stimato, checkins(seguito_piano, spreco, categoria_spreco, spesa_reale, retailer_usato)")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false });

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
    <div className="flex flex-1 flex-col items-center px-6 py-10">
      <h1 className="text-2xl font-semibold text-zinc-950 dark:text-zinc-50">Il tuo andamento</h1>
      <p className="mt-2 max-w-md text-center text-sm text-zinc-500 dark:text-zinc-400">
        Basato sui check-in di fine settimana — nessun dato aggiuntivo da inserire.
      </p>

      <div className="mt-6 grid w-full max-w-2xl grid-cols-2 gap-4 sm:grid-cols-3">
        <div className="rounded-xl border border-zinc-200 p-4 text-center dark:border-zinc-800">
          <p className="text-xs text-zinc-500 dark:text-zinc-400">Risparmio cumulativo</p>
          <p className="mt-1 text-xl font-semibold text-zinc-950 dark:text-zinc-50">
            {risparmioCumulativo !== null ? `€${risparmioCumulativo.toFixed(2)}` : "—"}
          </p>
        </div>
        <div className="rounded-xl border border-zinc-200 p-4 text-center dark:border-zinc-800">
          <p className="text-xs text-zinc-500 dark:text-zinc-400">Settimane senza sprechi</p>
          <p className="mt-1 text-xl font-semibold text-zinc-950 dark:text-zinc-50">
            {percentualeSenzaSprechi !== null ? `${percentualeSenzaSprechi}%` : "—"}
          </p>
        </div>
        <div className="rounded-xl border border-zinc-200 p-4 text-center dark:border-zinc-800">
          <p className="text-xs text-zinc-500 dark:text-zinc-400">Categoria più sprecata</p>
          <p className="mt-1 text-xl font-semibold text-zinc-950 dark:text-zinc-50">
            {categoriaPiuFrequente || "—"}
          </p>
        </div>
      </div>

      <div className="mt-8 w-full max-w-2xl">
        <h2 className="mb-3 text-left text-sm font-semibold text-zinc-800 dark:text-zinc-200">
          Spesa stimata vs reale, settimana per settimana
        </h2>
        {settimane.length === 0 ? (
          <p className="text-sm text-zinc-500 dark:text-zinc-400">
            Nessun piano ancora generato.
          </p>
        ) : (
          <ul className="flex flex-col gap-2">
            {settimane.map((s) => {
              const checkin = s.checkins[0];
              return (
                <li
                  key={s.settimana}
                  className="flex items-center justify-between rounded-lg border border-zinc-200 px-4 py-2.5 text-sm dark:border-zinc-800"
                >
                  <span className="text-zinc-600 dark:text-zinc-400">
                    Settimana del {s.settimana}
                  </span>
                  <span className="text-zinc-800 dark:text-zinc-200">
                    stimato €{s.budget_stimato?.toFixed(2) ?? "—"}
                    {checkin?.spesa_reale != null && (
                      <> · reale €{checkin.spesa_reale.toFixed(2)}</>
                    )}
                  </span>
                </li>
              );
            })}
          </ul>
        )}
      </div>
    </div>
  );
}
