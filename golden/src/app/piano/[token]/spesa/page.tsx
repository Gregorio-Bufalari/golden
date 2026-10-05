import { createAdminClient } from "@/lib/supabase/admin";
import { GroceryList } from "../grocery-list";
import { ingredientiARischioSettimana } from "../grocery-risk";
import { PageHeader } from "../page-header";

export default async function SpesaPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  const supabase = createAdminClient();

  const { data: profile } = await supabase
    .from("profiles")
    .select("id, budget_settimanale")
    .eq("link_token", token)
    .single();

  if (!profile) {
    return null;
  }

  const { data: ultimoPiano } = await supabase
    .from("weekly_plans")
    .select("id, settimana, grocery_list, meal_plan")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false })
    .limit(1)
    .maybeSingle();

  // Tollerante a una tabella spesa_stato non ancora creata sul database
  // (va eseguita a mano in Supabase, vedi supabase/migrations): senza
  // questo try/catch una query a una tabella mancante farebbe crashare
  // tutta la pagina invece di mostrare la lista con le spunte azzerate.
  let statoAcquisti: Record<string, boolean> = {};
  if (ultimoPiano?.id) {
    try {
      const { data: righeStato } = await supabase
        .from("spesa_stato")
        .select("prodotto, acquistato")
        .eq("weekly_plan_id", ultimoPiano.id);

      statoAcquisti = Object.fromEntries((righeStato || []).map((r) => [r.prodotto, r.acquistato]));
    } catch {
      statoAcquisti = {};
    }
  }

  let ingredientiARischio: string[] = [];
  try {
    ingredientiARischio = ingredientiARischioSettimana(ultimoPiano?.meal_plan?.giorni || []);
  } catch {
    ingredientiARischio = [];
  }

  return (
    <div className="flex flex-1 flex-col">
      <PageHeader token={token} title="Spesa" />

      <div className="mx-auto w-full max-w-2xl flex-1 px-5 pb-10">
        {ultimoPiano?.grocery_list ? (
          <GroceryList
            token={token}
            initialData={ultimoPiano.grocery_list}
            settimana={ultimoPiano.settimana}
            initialStatoAcquisti={statoAcquisti}
            budgetSettimanale={profile.budget_settimanale}
            ingredientiARischio={ingredientiARischio}
          />
        ) : (
          <p className="pt-10 text-center text-sm text-ink/55">
            Nessuna lista della spesa ancora. Genera prima il piano nella sezione{" "}
            <span className="font-medium text-ink">Menu</span>.
          </p>
        )}
      </div>
    </div>
  );
}
