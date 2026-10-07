"use client";

import { useState } from "react";
import Image from "next/image";
import { useRouter } from "next/navigation";
import { createProfile, type OnboardingInput } from "./actions";
import { Spinner } from "@/components/spinner";
import { inputClass, labelClass, checkboxClass, optionRowClass, Pill } from "@/components/form-kit";
import { SupermercatoSelector } from "@/components/supermercato-selector";
import {
  RESTRIZIONI_OPTIONS,
  OBIETTIVO_OPTIONS,
  CUCINA_OPTIONS,
  TEMPO_OPTIONS,
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
    <div className="flex flex-1 flex-col items-center bg-paper px-6 py-10">
      <div className="w-full max-w-xl">
        <div className="mb-8 flex flex-col items-center gap-6">
          <Image src="/logo.png" alt="Groci" height={40} width={132} style={{ height: 40, width: "auto" }} priority />
          <div className="flex w-full items-center justify-between text-sm text-ink/60">
            <span>
              Passo {step + 1} di {totalSteps}
            </span>
            <div className="mx-4 h-1.5 flex-1 rounded-full bg-panel">
              <div
                className="h-1.5 rounded-full bg-accent transition-all"
                style={{ width: `${((step + 1) / totalSteps) * 100}%` }}
              />
            </div>
          </div>
        </div>

        <div className="flex flex-col gap-6">
          {step === 0 && (
            <div className="flex flex-col gap-6">
              <div>
                <label className={labelClass}>Come ti chiami?</label>
                <input
                  type="text"
                  value={form.nome}
                  onChange={(e) => setForm((p) => ({ ...p, nome: e.target.value }))}
                  placeholder="Il tuo nome"
                  className={inputClass}
                />
              </div>

              <div>
                <h2 className="text-xl font-bold text-ink">Restrizioni alimentari</h2>
                <p className="mt-1 text-sm text-ink/60">
                  Le rispettiamo sempre, senza eccezioni. Seleziona tutte quelle che ti riguardano.
                </p>
              </div>
              <div className="flex flex-col gap-1.5">
                {RESTRIZIONI_OPTIONS.map((opt) => (
                  <label key={opt} className={optionRowClass}>
                    <input
                      type="checkbox"
                      checked={form.restrizioni.includes(opt)}
                      onChange={() => toggleRestrizione(opt)}
                      className={checkboxClass}
                    />
                    {opt}
                  </label>
                ))}
              </div>
            </div>
          )}

          {step === 1 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-bold text-ink">Quante persone a tavola</h2>
                <p className="mt-1 text-sm text-ink/60">Quante persone mangiano abitualmente insieme a te?</p>
              </div>
              <input
                type="number"
                min={1}
                value={form.household_size}
                onChange={(e) => setForm((p) => ({ ...p, household_size: e.target.value }))}
                placeholder="Es. 2"
                className={inputClass}
              />
            </div>
          )}

          {step === 2 && (
            <div className="flex flex-col gap-6">
              <h2 className="text-xl font-bold text-ink">Qual è il tuo obiettivo principale?</h2>
              <div className="flex flex-col gap-1.5">
                {OBIETTIVO_OPTIONS.map((opt) => (
                  <label key={opt} className={optionRowClass}>
                    <input
                      type="radio"
                      name="obiettivo"
                      checked={form.obiettivo === opt}
                      onChange={() => setForm((p) => ({ ...p, obiettivo: opt }))}
                      className={checkboxClass}
                    />
                    {opt}
                  </label>
                ))}
              </div>
            </div>
          )}

          {step === 3 && (
            <div className="flex flex-col gap-6">
              <h2 className="text-xl font-bold text-ink">Le tue preferenze</h2>
              <div>
                <label className={labelClass}>Cucina preferita</label>
                <div className="flex flex-wrap gap-2">
                  {CUCINA_OPTIONS.map((opt) => (
                    <Pill
                      key={opt}
                      label={opt}
                      selected={form.cucina.includes(opt)}
                      onClick={() => setForm((p) => ({ ...p, cucina: toggleInArray(p.cucina, opt) }))}
                    />
                  ))}
                </div>
              </div>
              <div>
                <label className={labelClass}>Alimenti che ti piacciono</label>
                <input
                  type="text"
                  value={form.graditi}
                  onChange={(e) => setForm((p) => ({ ...p, graditi: e.target.value }))}
                  placeholder="Es. pollo, legumi, pesce"
                  className={inputClass}
                />
              </div>
              <div>
                <label className={labelClass}>Alimenti che non ti piacciono</label>
                <input
                  type="text"
                  value={form.non_graditi}
                  onChange={(e) => setForm((p) => ({ ...p, non_graditi: e.target.value }))}
                  placeholder="Es. funghi, melanzane"
                  className={inputClass}
                />
              </div>
            </div>
          )}

          {step === 4 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-bold text-ink">Quanto tempo hai per cucinare?</h2>
                <p className="mt-1 text-sm text-ink/60">Per pasto, in media.</p>
              </div>
              <div className="flex flex-col gap-1.5">
                {TEMPO_OPTIONS.map((opt) => (
                  <label key={opt.value} className={optionRowClass}>
                    <input
                      type="radio"
                      name="tempo"
                      checked={form.tempo_max_cucina === opt.value}
                      onChange={() => setForm((p) => ({ ...p, tempo_max_cucina: opt.value }))}
                      className={checkboxClass}
                    />
                    {opt.label}
                  </label>
                ))}
              </div>
            </div>
          )}

          {step === 5 && (
            <div className="flex flex-col gap-6">
              <h2 className="text-xl font-bold text-ink">Budget settimanale indicativo</h2>
              <div className="relative">
                <span className="absolute left-4 top-1/2 -translate-y-1/2 text-ink/50">€</span>
                <input
                  type="number"
                  min={0}
                  value={form.budget_settimanale}
                  onChange={(e) => setForm((p) => ({ ...p, budget_settimanale: e.target.value }))}
                  placeholder="80"
                  className={`${inputClass} pl-8`}
                />
              </div>
            </div>
          )}

          {step === 6 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-bold text-ink">Dove fai la spesa di solito?</h2>
                <p className="mt-1 text-sm text-ink/60">
                  Opzionale. Ci serve solo per stimare meglio i prezzi, mai per favorirlo.
                </p>
              </div>
              <SupermercatoSelector
                value={form.supermercato}
                onChange={(opt) => setForm((p) => ({ ...p, supermercato: opt }))}
              />
            </div>
          )}

          {step === 7 && (
            <div className="flex flex-col gap-6">
              <div>
                <h2 className="text-xl font-bold text-ink">
                  Dati biometrici <span className="text-sm font-normal text-ink/50">(facoltativo)</span>
                </h2>
                <p className="mt-1 text-sm text-ink/60">
                  Se li compili, nella sezione Menu ti mostriamo un confronto indicativo tra il piano e i
                  valori di riferimento nutrizionali generali. Puoi saltare questo passo e compilarlo in un
                  secondo momento dal Profilo.
                </p>
              </div>

              <div>
                <label className={labelClass}>Sesso</label>
                <div className="flex gap-2">
                  {(["M", "F"] as const).map((opt) => (
                    <Pill
                      key={opt}
                      label={opt === "M" ? "Maschio" : "Femmina"}
                      selected={form.sesso === opt}
                      onClick={() => setForm((p) => ({ ...p, sesso: p.sesso === opt ? "" : opt }))}
                    />
                  ))}
                </div>
              </div>

              <div>
                <label className={labelClass}>Età</label>
                <input
                  type="number"
                  min={1}
                  value={form.eta}
                  onChange={(e) => setForm((p) => ({ ...p, eta: e.target.value }))}
                  className={inputClass}
                />
              </div>

              <div className="flex gap-3">
                <div className="flex-1">
                  <label className={labelClass}>Peso (kg)</label>
                  <input
                    type="number"
                    min={1}
                    step="0.1"
                    value={form.peso_kg}
                    onChange={(e) => setForm((p) => ({ ...p, peso_kg: e.target.value }))}
                    className={inputClass}
                  />
                </div>
                <div className="flex-1">
                  <label className={labelClass}>Altezza (cm)</label>
                  <input
                    type="number"
                    min={1}
                    step="0.5"
                    value={form.altezza_cm}
                    onChange={(e) => setForm((p) => ({ ...p, altezza_cm: e.target.value }))}
                    className={inputClass}
                  />
                </div>
              </div>

              <div>
                <label className={labelClass}>Livello di attività fisica</label>
                <div className="flex flex-col gap-1.5">
                  {LIVELLO_ATTIVITA_OPTIONS.map((opt) => (
                    <label key={opt.value} className={optionRowClass}>
                      <input
                        type="radio"
                        name="livello_attivita"
                        checked={form.livello_attivita === opt.value}
                        onChange={() => setForm((p) => ({ ...p, livello_attivita: opt.value }))}
                        className={checkboxClass}
                      />
                      {opt.label}
                    </label>
                  ))}
                </div>
              </div>
            </div>
          )}

          {error && <p className="text-sm text-clay">{error}</p>}

          <div className="flex items-center justify-between">
            <button
              type="button"
              onClick={() => setStep((s) => Math.max(0, s - 1))}
              disabled={step === 0 || submitting}
              className="min-h-11 rounded-full px-5 text-sm font-semibold text-ink/70 disabled:opacity-0"
            >
              Indietro
            </button>

            {step < totalSteps - 1 ? (
              <button
                type="button"
                onClick={() => setStep((s) => s + 1)}
                disabled={!canAdvance}
                className="min-h-11 rounded-full bg-accent px-6 text-sm font-semibold text-accent-fill-text disabled:opacity-40"
              >
                Avanti
              </button>
            ) : (
              <button
                type="button"
                onClick={handleSubmit}
                disabled={submitting}
                className="flex min-h-11 items-center gap-2 rounded-full bg-accent px-6 text-sm font-semibold text-accent-fill-text disabled:opacity-40"
              >
                {submitting && <Spinner className="h-4 w-4" />}
                {submitting ? "Creazione in corso..." : "Crea il mio piano"}
              </button>
            )}
          </div>
        </div>

        <div className="mt-8 bg-panel px-5 py-4 text-center text-sm text-ink/70">
          Non vendiamo i tuoi dati ai supermercati. Non ti spingiamo prodotti a margine. L&apos;abbonamento
          è la nostra unica fonte di guadagno.
        </div>
      </div>
    </div>
  );
}
