import { createAdminClient } from "@/lib/supabase/admin";
import { conservazioneTipica } from "@/lib/conservazione";

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

  return (
    <div className="flex flex-1 flex-col items-center px-6 py-10">
      <h1 className="text-2xl font-semibold text-zinc-950 dark:text-zinc-50">Frigo e dispensa</h1>
      <p className="mt-2 max-w-md text-center text-sm text-zinc-500 dark:text-zinc-400">
        Quello che è avanzato comprando le confezioni intere nelle settimane scorse — viene
        sottratto automaticamente dal fabbisogno dei prossimi piani, finché non si esaurisce.
      </p>

      <div className="mt-6 w-full max-w-md">
        {rimanenze && rimanenze.length > 0 ? (
          <ul className="flex flex-col gap-2">
            {rimanenze.map((r) => (
              <li
                key={`${r.ingrediente}-${r.unita}`}
                className="flex items-center justify-between gap-3 rounded-lg border border-zinc-200 px-4 py-2.5 text-sm dark:border-zinc-800"
              >
                <div className="flex flex-col">
                  <span className="text-zinc-800 dark:text-zinc-200">{r.ingrediente}</span>
                  <span className="text-xs text-zinc-400">{conservazioneTipica(r.ingrediente)}</span>
                </div>
                <span className="shrink-0 text-zinc-500 dark:text-zinc-400">
                  {formatQuantita(Number(r.quantita), r.unita)}
                </span>
              </li>
            ))}
          </ul>
        ) : (
          <p className="text-center text-sm text-zinc-500 dark:text-zinc-400">
            Niente in dispensa al momento.
          </p>
        )}
      </div>
    </div>
  );
}
