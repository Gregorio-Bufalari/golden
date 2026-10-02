import { createAdminClient } from "@/lib/supabase/admin";
import { GroceryList } from "../grocery-list";

export default async function SpesaPage({
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

  const { data: ultimoPiano } = await supabase
    .from("weekly_plans")
    .select("settimana, grocery_list")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false })
    .limit(1)
    .maybeSingle();

  return (
    <div className="flex flex-1 flex-col items-center px-6 py-10">
      <h1 className="text-2xl font-semibold text-zinc-950 dark:text-zinc-50 print:hidden">
        Lista della spesa
      </h1>

      <div className="mt-6 w-full max-w-2xl">
        {ultimoPiano?.grocery_list ? (
          <GroceryList
            token={token}
            initialData={ultimoPiano.grocery_list}
            settimana={ultimoPiano.settimana}
          />
        ) : (
          <p className="text-center text-sm text-zinc-500 dark:text-zinc-400">
            Nessuna lista della spesa ancora — genera prima il piano nella sezione{" "}
            <span className="font-medium">Menu</span>.
          </p>
        )}
      </div>
    </div>
  );
}
