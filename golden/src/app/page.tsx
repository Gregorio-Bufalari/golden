import Link from "next/link";
import { createClient } from "@/lib/supabase/server";

async function getSupabaseStatus() {
  const url = process.env.NEXT_PUBLIC_SUPABASE_URL;
  const anonKey = process.env.NEXT_PUBLIC_SUPABASE_ANON_KEY;

  if (!url || !anonKey) {
    return { connected: false, message: "Variabili d'ambiente Supabase non impostate." };
  }

  try {
    const supabase = await createClient();
    const { error } = await supabase.auth.getSession();
    if (error) throw error;
    return { connected: true, message: "Connesso al progetto Supabase." };
  } catch (err) {
    return {
      connected: false,
      message: err instanceof Error ? err.message : "Connessione a Supabase fallita.",
    };
  }
}

export default async function Home() {
  const status = await getSupabaseStatus();

  return (
    <div className="flex flex-1 flex-col items-center justify-center bg-zinc-50 font-sans dark:bg-black">
      <main className="flex w-full max-w-xl flex-col items-center gap-6 px-6 py-24 text-center">
        <h1 className="text-4xl font-semibold tracking-tight text-black dark:text-zinc-50">
          Groci
        </h1>
        <p className="text-lg text-zinc-600 dark:text-zinc-400">
          Next.js (App Router) + TypeScript + Tailwind, pronto per Vercel.
        </p>
        <div
          className={`flex items-center gap-2 rounded-full border px-4 py-2 text-sm font-medium ${
            status.connected
              ? "border-green-200 bg-green-50 text-green-700 dark:border-green-900 dark:bg-green-950 dark:text-green-400"
              : "border-amber-200 bg-amber-50 text-amber-700 dark:border-amber-900 dark:bg-amber-950 dark:text-amber-400"
          }`}
        >
          <span
            className={`h-2 w-2 rounded-full ${
              status.connected ? "bg-green-500" : "bg-amber-500"
            }`}
          />
          {status.message}
        </div>
        <Link
          href="/onboarding"
          className="rounded-full bg-black px-6 py-2.5 text-sm font-medium text-white dark:bg-white dark:text-black"
        >
          Inizia l&apos;onboarding
        </Link>
      </main>
    </div>
  );
}
