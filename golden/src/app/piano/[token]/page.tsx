import { notFound } from "next/navigation";
import { createAdminClient } from "@/lib/supabase/admin";

export default async function PianoPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  const supabase = createAdminClient();

  const { data: profile } = await supabase
    .from("profiles")
    .select("nome, restrizioni, obiettivo, link_token")
    .eq("link_token", token)
    .single();

  if (!profile) {
    notFound();
  }

  return (
    <div className="flex flex-1 flex-col items-center justify-center bg-zinc-50 px-6 py-24 text-center font-sans dark:bg-black">
      <h1 className="text-3xl font-semibold text-zinc-950 dark:text-zinc-50">
        Ciao {profile.nome}!
      </h1>
      <p className="mt-3 max-w-md text-zinc-600 dark:text-zinc-400">
        Il tuo profilo è stato creato. Il tuo piano settimanale arriverà qui a breve.
      </p>
      {profile.restrizioni?.length > 0 && (
        <p className="mt-6 text-sm text-zinc-500 dark:text-zinc-500">
          Restrizioni registrate: {profile.restrizioni.join(", ")}
        </p>
      )}
    </div>
  );
}
