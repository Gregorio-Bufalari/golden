"use client";

import { useState } from "react";
import { useRouter } from "next/navigation";
import { createProfile, type OnboardingInput } from "./actions";
import {
  RESTRIZIONI_OPTIONS,
  OBIETTIVO_OPTIONS,
  CUCINA_OPTIONS,
  TEMPO_OPTIONS,
  SUPERMERCATO_OPTIONS,
  LIVELLO_ATTIVITA_OPTIONS,
} from "@/lib/opzioni-profilo";

type FormState = {
  nome: string;
  restrizioni: string[];
  household_size: string;
  obiettivo: string;
  cucina: string[];
  graditi: string;
  non_graditi: string;
  tempo_max_cucina: number | null;
  budget_settimanale: string;
  supermercato: string;
  sesso: "M" | "F" | "";
  eta: string;
  peso_kg: string;
  altezza_cm: string;
  livello_attivita: "sedentario" | "moderato" | "attivo" | "";
};

const initialState: FormState = {
  nome: "",
  restrizioni: [],
  household_size: "",
  obiettivo: "",
  cucina: [],
  graditi: "",
  non_graditi: "",
  tempo_max_cucina: null,
  budget_settimanale: "",
  supermercato: "",
  sesso: "",
  eta: "",
  peso_kg: "",
  altezza_cm: "",
  livello_attivita: "",
};

function toggleInArray(list: string[], value: string): string[] {
  return list.includes(value) ? list.filter((v) => v !== value) : [...list, value];
}

export default function OnboardingPage() {
  const router = useRouter();
  const [step, setStep] = useState(0);
  const [form, setForm] = useState<FormState>(initialState);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const totalSteps = 8;

  function toggleRestrizione(value: string) {
    setForm((prev) => {
      if (value === "Nessuna restrizione") {
        return { ...prev, restrizioni: prev.restrizioni.includes(value) ? [] : [value] };
      }
      const withoutNessuna = prev.restrizioni.filter((v) => v !== "Nessuna restrizione");
      return { ...prev, restrizioni: toggleInArray(withoutNessuna, value) };
    });
  }

  const canAdvance = (() => {
    switch (step) {
      case 0:
        return form.nome.trim().length > 0 && form.restrizioni.length > 0;
      default:
        return true;
    }
  })();

  async function handleSubmit() {
    setSubmitting(true);
    setError(null);

    const input: OnboardingInput = {
      nome: form.nome,
      restrizioni: form.restrizioni,
      household_size: Number(form.household_size) || 0,
      obiettivo: form.obiettivo,
      preferenze: {
        cucina: form.cucina,
        graditi: form.graditi,
        non_graditi: form.non_graditi,
      },
      tempo_max_cucina: form.tempo_max_cucina ?? 0,
      budget_settimanale: Number(form.budget_settimanale) || 0,
      supermercato: form.supermercato,
      sesso: form.sesso || null,
      eta: form.eta ? Number(form.eta) : null,
      peso_kg: form.peso_kg ? Number(form.peso_kg) : null,
      altezza_cm: form.altezza_cm ? Number(form.altezza_cm) : null,
      livello_attivita: form.livello_attivita || null,
    };

    const result = await createProfile(input);

    if ("error" in result) {
      setError(result.error);
      setSubmitting(false);
      return;
    }

    router.push(`/piano/${result.token}`);
  }

  return (
    <div className="flex flex-1 flex-col items-center bg-zinc-50 px-6 py-16 font-sans dark:bg-black">
      <div className="w-full max-w-xl">
        <div className="mb-8 flex items-center justify-between text-sm text-zinc-500 dark:text-zinc-400">
          <span>
            Step {step + 1} di {totalSteps}
          </span>
          <div className="h-1.5 flex-1 mx-4 rounded-full bg-zinc-200 dark:bg-zinc-800">
            <div
              className="h-1.5 rounded-full bg-black dark:bg-white transition-all"
              style={{ width: `${((step + 1) / totalSteps) * 100}%` }}
            />
          </div>
        </div>

        <div className="rounded-2xl border border-zinc-200 bg-white p-8 dark:border-zinc-800 dark:bg-zinc-950">
          {step === 0 && (
            <div className="flex flex-col gap-6">
              <div>
                <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                  Come ti chiami?
                </label>
                <input
                  type="text"
                  value={form.nome}
                  onChange={(e) => setForm((p) => ({ ...p, nome: e.target.value }))}
                  placeholder="Il tuo nome"
                  className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-zinc-950 placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:text-zinc-50 dark:focus:border-white"
                />
              </div>

              <div>
                <h2 className="text-xl font-semibold text-zinc-950 dark:text-zinc-50">
                  Restrizioni alimentari
                </h2>
                <p className="mt-1 text-sm text-zinc-500 dark:text-zinc-400">
                  Le rispettiamo sempre, senza eccezioni. Seleziona tutte quelle che ti riguardano.
                </p>
              </div>
              <div className="flex flex-col gap-2">
                {RESTRIZIONI_OPTIONS.map((opt) => (
                  <label
                    key={opt}
                    className="flex cursor-pointer items-center gap-3 rounded-lg border border-zinc-200 px-4 py-3 hover:bg-zinc-50 dark:border-zinc-800 dark:hover:bg-zinc-900"
                  >
                    <input
                      type="checkbox"
                      checked={form.restrizioni.includes(opt)}
                      onChange={() => toggleRestrizione(opt)}
                      className="h-4 w-4"
                    />
                    <span className="text-sm text-zinc-800 dark:text-zinc-200">{opt}</span>
                  </label>
                ))}
              </div>
            </div>
          )}

          {step === 1 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-semibold text-zinc-950 dark:text-zinc-50">
                  Il tuo household
                </h2>
                <p className="mt-1 text-sm text-zinc-500 dark:text-zinc-400">
                  Quante persone mangiano abitualmente insieme a te?
                </p>
              </div>
              <input
                type="number"
                min={1}
                value={form.household_size}
                onChange={(e) => setForm((p) => ({ ...p, household_size: e.target.value }))}
                placeholder="Es. 2"
                className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-zinc-950 placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:text-zinc-50 dark:focus:border-white"
              />
            </div>
          )}

          {step === 2 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-semibold text-zinc-950 dark:text-zinc-50">
                  Qual è il tuo obiettivo principale?
                </h2>
              </div>
              <div className="flex flex-col gap-2">
                {OBIETTIVO_OPTIONS.map((opt) => (
                  <label
                    key={opt}
                    className="flex cursor-pointer items-center gap-3 rounded-lg border border-zinc-200 px-4 py-3 hover:bg-zinc-50 dark:border-zinc-800 dark:hover:bg-zinc-900"
                  >
                    <input
                      type="radio"
                      name="obiettivo"
                      checked={form.obiettivo === opt}
                      onChange={() => setForm((p) => ({ ...p, obiettivo: opt }))}
                      className="h-4 w-4"
                    />
                    <span className="text-sm text-zinc-800 dark:text-zinc-200">{opt}</span>
                  </label>
                ))}
              </div>
            </div>
          )}

          {step === 3 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-semibold text-zinc-950 dark:text-zinc-50">
                  Le tue preferenze
                </h2>
              </div>
              <div>
                <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                  Cucina preferita
                </label>
                <div className="flex flex-wrap gap-2">
                  {CUCINA_OPTIONS.map((opt) => (
                    <button
                      type="button"
                      key={opt}
                      onClick={() => setForm((p) => ({ ...p, cucina: toggleInArray(p.cucina, opt) }))}
                      className={`rounded-full border px-4 py-1.5 text-sm transition-colors ${
                        form.cucina.includes(opt)
                          ? "border-black bg-black text-white dark:border-white dark:bg-white dark:text-black"
                          : "border-zinc-300 text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
                      }`}
                    >
                      {opt}
                    </button>
                  ))}
                </div>
              </div>
              <div>
                <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                  Alimenti che ti piacciono
                </label>
                <input
                  type="text"
                  value={form.graditi}
                  onChange={(e) => setForm((p) => ({ ...p, graditi: e.target.value }))}
                  placeholder="Es. pollo, legumi, pesce"
                  className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-zinc-950 placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:text-zinc-50 dark:focus:border-white"
                />
              </div>
              <div>
                <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                  Alimenti che non ti piacciono
                </label>
                <input
                  type="text"
                  value={form.non_graditi}
                  onChange={(e) => setForm((p) => ({ ...p, non_graditi: e.target.value }))}
                  placeholder="Es. funghi, melanzane"
                  className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-zinc-950 placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:text-zinc-50 dark:focus:border-white"
                />
              </div>
            </div>
          )}

          {step === 4 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-semibold text-zinc-950 dark:text-zinc-50">
                  Quanto tempo hai per cucinare?
                </h2>
                <p className="mt-1 text-sm text-zinc-500 dark:text-zinc-400">
                  Per pasto, in media.
                </p>
              </div>
              <div className="flex flex-col gap-2">
                {TEMPO_OPTIONS.map((opt) => (
                  <label
                    key={opt.value}
                    className="flex cursor-pointer items-center gap-3 rounded-lg border border-zinc-200 px-4 py-3 hover:bg-zinc-50 dark:border-zinc-800 dark:hover:bg-zinc-900"
                  >
                    <input
                      type="radio"
                      name="tempo"
                      checked={form.tempo_max_cucina === opt.value}
                      onChange={() => setForm((p) => ({ ...p, tempo_max_cucina: opt.value }))}
                      className="h-4 w-4"
                    />
                    <span className="text-sm text-zinc-800 dark:text-zinc-200">{opt.label}</span>
                  </label>
                ))}
              </div>
            </div>
          )}

          {step === 5 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-semibold text-zinc-950 dark:text-zinc-50">
                  Budget settimanale indicativo
                </h2>
              </div>
              <div className="relative">
                <span className="absolute left-4 top-1/2 -translate-y-1/2 text-zinc-500">€</span>
                <input
                  type="number"
                  min={0}
                  value={form.budget_settimanale}
                  onChange={(e) => setForm((p) => ({ ...p, budget_settimanale: e.target.value }))}
                  placeholder="80"
                  className="w-full rounded-lg border border-zinc-300 bg-transparent py-2.5 pl-8 pr-4 text-zinc-950 placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:text-zinc-50 dark:focus:border-white"
                />
              </div>
            </div>
          )}

          {step === 6 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-semibold text-zinc-950 dark:text-zinc-50">
                  Dove fai la spesa di solito?
                </h2>
                <p className="mt-1 text-sm text-zinc-500 dark:text-zinc-400">
                  Opzionale — ci serve solo per stimare meglio i prezzi, mai per favorirlo.
                </p>
              </div>
              <div className="flex flex-wrap gap-2">
                {SUPERMERCATO_OPTIONS.map((opt) => (
                  <button
                    type="button"
                    key={opt}
                    onClick={() => setForm((p) => ({ ...p, supermercato: opt }))}
                    className={`rounded-full border px-4 py-1.5 text-sm transition-colors ${
                      form.supermercato === opt
                        ? "border-black bg-black text-white dark:border-white dark:bg-white dark:text-black"
                        : "border-zinc-300 text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
                    }`}
                  >
                    {opt}
                  </button>
                ))}
              </div>
            </div>
          )}

          {step === 7 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-semibold text-zinc-950 dark:text-zinc-50">
                  Dati biometrici{" "}
                  <span className="text-sm font-normal text-zinc-400">(facoltativo)</span>
                </h2>
                <p className="mt-1 text-sm text-zinc-500 dark:text-zinc-400">
                  Se li compili, nella sezione Menu ti mostriamo un confronto indicativo tra il
                  piano e i valori di riferimento nutrizionali generali. Puoi saltare questo passo
                  e compilarlo in un secondo momento dalle Impostazioni.
                </p>
              </div>

              <div>
                <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                  Sesso
                </label>
                <div className="flex gap-2">
                  {(["M", "F"] as const).map((opt) => (
                    <button
                      type="button"
                      key={opt}
                      onClick={() => setForm((p) => ({ ...p, sesso: p.sesso === opt ? "" : opt }))}
                      className={`rounded-full border px-4 py-1.5 text-sm transition-colors ${
                        form.sesso === opt
                          ? "border-black bg-black text-white dark:border-white dark:bg-white dark:text-black"
                          : "border-zinc-300 text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
                      }`}
                    >
                      {opt === "M" ? "Maschio" : "Femmina"}
                    </button>
                  ))}
                </div>
              </div>

              <div>
                <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                  Età
                </label>
                <input
                  type="number"
                  min={1}
                  value={form.eta}
                  onChange={(e) => setForm((p) => ({ ...p, eta: e.target.value }))}
                  className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-zinc-950 placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:text-zinc-50 dark:focus:border-white"
                />
              </div>

              <div className="flex gap-3">
                <div className="flex-1">
                  <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                    Peso (kg)
                  </label>
                  <input
                    type="number"
                    min={1}
                    step="0.1"
                    value={form.peso_kg}
                    onChange={(e) => setForm((p) => ({ ...p, peso_kg: e.target.value }))}
                    className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-zinc-950 placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:text-zinc-50 dark:focus:border-white"
                  />
                </div>
                <div className="flex-1">
                  <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                    Altezza (cm)
                  </label>
                  <input
                    type="number"
                    min={1}
                    step="0.5"
                    value={form.altezza_cm}
                    onChange={(e) => setForm((p) => ({ ...p, altezza_cm: e.target.value }))}
                    className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-zinc-950 placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:text-zinc-50 dark:focus:border-white"
                  />
                </div>
              </div>

              <div>
                <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
                  Livello di attività fisica
                </label>
                <div className="flex flex-col gap-2">
                  {LIVELLO_ATTIVITA_OPTIONS.map((opt) => (
                    <label
                      key={opt.value}
                      className="flex cursor-pointer items-center gap-3 rounded-lg border border-zinc-200 px-4 py-3 hover:bg-zinc-50 dark:border-zinc-800 dark:hover:bg-zinc-900"
                    >
                      <input
                        type="radio"
                        name="livello_attivita"
                        checked={form.livello_attivita === opt.value}
                        onChange={() => setForm((p) => ({ ...p, livello_attivita: opt.value }))}
                        className="h-4 w-4"
                      />
                      <span className="text-sm text-zinc-800 dark:text-zinc-200">{opt.label}</span>
                    </label>
                  ))}
                </div>
              </div>
            </div>
          )}

          {error && (
            <p className="mt-4 text-sm text-red-600 dark:text-red-400">{error}</p>
          )}

          <div className="mt-8 flex items-center justify-between">
            <button
              type="button"
              onClick={() => setStep((s) => Math.max(0, s - 1))}
              disabled={step === 0 || submitting}
              className="rounded-full px-5 py-2.5 text-sm font-medium text-zinc-600 disabled:opacity-0 dark:text-zinc-400"
            >
              Indietro
            </button>

            {step < totalSteps - 1 ? (
              <button
                type="button"
                onClick={() => setStep((s) => s + 1)}
                disabled={!canAdvance}
                className="rounded-full bg-black px-6 py-2.5 text-sm font-medium text-white transition-opacity disabled:opacity-40 dark:bg-white dark:text-black"
              >
                Avanti
              </button>
            ) : (
              <button
                type="button"
                onClick={handleSubmit}
                disabled={submitting}
                className="rounded-full bg-black px-6 py-2.5 text-sm font-medium text-white transition-opacity disabled:opacity-40 dark:bg-white dark:text-black"
              >
                {submitting ? "Creazione in corso..." : "Crea il mio piano"}
              </button>
            )}
          </div>
        </div>

        <div className="mt-8 rounded-xl border border-zinc-200 bg-zinc-100/60 p-5 text-center text-sm text-zinc-600 dark:border-zinc-800 dark:bg-zinc-900/60 dark:text-zinc-400">
          Non vendiamo i tuoi dati ai supermercati. Non ti spingiamo prodotti a margine.
          L&apos;abbonamento è la nostra unica fonte di guadagno.
        </div>
      </div>
    </div>
  );
}
