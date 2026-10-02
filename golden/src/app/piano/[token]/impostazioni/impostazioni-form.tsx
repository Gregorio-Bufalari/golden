"use client";

import { useState } from "react";
import { updateProfilo } from "./actions";
import {
  RESTRIZIONI_OPTIONS,
  OBIETTIVO_OPTIONS,
  CUCINA_OPTIONS,
  TEMPO_OPTIONS,
  SUPERMERCATO_OPTIONS,
} from "@/lib/opzioni-profilo";

type ProfileData = {
  nome: string;
  restrizioni: string[];
  household_size: number | null;
  obiettivo: string | null;
  preferenze: { cucina?: string[]; graditi?: string; non_graditi?: string } | null;
  tempo_max_cucina: number | null;
  budget_settimanale: number | null;
  supermercato: string | null;
  sesso: "M" | "F" | null;
  eta: number | null;
  peso_kg: number | null;
  altezza_cm: number | null;
  livello_attivita: "sedentario" | "moderato" | "attivo" | null;
};

const LIVELLO_ATTIVITA_OPTIONS: { value: "sedentario" | "moderato" | "attivo"; label: string }[] = [
  { value: "sedentario", label: "Sedentario" },
  { value: "moderato", label: "Moderato" },
  { value: "attivo", label: "Attivo" },
];

function toggleInArray(list: string[], value: string): string[] {
  return list.includes(value) ? list.filter((v) => v !== value) : [...list, value];
}

export function ImpostazioniForm({ token, profile }: { token: string; profile: ProfileData }) {
  const [nome, setNome] = useState(profile.nome);
  const [restrizioni, setRestrizioni] = useState<string[]>(profile.restrizioni || []);
  const [householdSize, setHouseholdSize] = useState(
    profile.household_size ? String(profile.household_size) : "",
  );
  const [obiettivo, setObiettivo] = useState(profile.obiettivo || "");
  const [cucina, setCucina] = useState<string[]>(profile.preferenze?.cucina || []);
  const [graditi, setGraditi] = useState(profile.preferenze?.graditi || "");
  const [nonGraditi, setNonGraditi] = useState(profile.preferenze?.non_graditi || "");
  const [tempoMaxCucina, setTempoMaxCucina] = useState<number | null>(profile.tempo_max_cucina);
  const [budgetSettimanale, setBudgetSettimanale] = useState(
    profile.budget_settimanale ? String(profile.budget_settimanale) : "",
  );
  const [supermercato, setSupermercato] = useState(profile.supermercato || "");
  const [sesso, setSesso] = useState<"M" | "F" | "">(profile.sesso || "");
  const [eta, setEta] = useState(profile.eta ? String(profile.eta) : "");
  const [pesoKg, setPesoKg] = useState(profile.peso_kg ? String(profile.peso_kg) : "");
  const [altezzaCm, setAltezzaCm] = useState(profile.altezza_cm ? String(profile.altezza_cm) : "");
  const [livelloAttivita, setLivelloAttivita] = useState<
    "sedentario" | "moderato" | "attivo" | ""
  >(profile.livello_attivita || "");

  const [salvando, setSalvando] = useState(false);
  const [errore, setErrore] = useState<string | null>(null);
  const [salvato, setSalvato] = useState(false);

  function toggleRestrizione(value: string) {
    setRestrizioni((prev) => {
      if (value === "Nessuna restrizione") {
        return prev.includes(value) ? [] : [value];
      }
      const withoutNessuna = prev.filter((v) => v !== "Nessuna restrizione");
      return toggleInArray(withoutNessuna, value);
    });
  }

  async function handleSalva() {
    setSalvando(true);
    setErrore(null);
    setSalvato(false);

    const result = await updateProfilo(token, {
      nome,
      restrizioni,
      household_size: householdSize ? Number(householdSize) : null,
      obiettivo,
      preferenze: { cucina, graditi, non_graditi: nonGraditi },
      tempo_max_cucina: tempoMaxCucina,
      budget_settimanale: budgetSettimanale ? Number(budgetSettimanale) : null,
      supermercato,
      sesso: sesso || null,
      eta: eta ? Number(eta) : null,
      peso_kg: pesoKg ? Number(pesoKg) : null,
      altezza_cm: altezzaCm ? Number(altezzaCm) : null,
      livello_attivita: livelloAttivita || null,
    });

    if ("error" in result) {
      setErrore(result.error);
    } else {
      setSalvato(true);
    }
    setSalvando(false);
  }

  return (
    <div className="flex flex-col gap-6 text-left">
      <div>
        <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
          Nome
        </label>
        <input
          type="text"
          value={nome}
          onChange={(e) => setNome(e.target.value)}
          className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-sm focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
        />
      </div>

      <div>
        <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
          Restrizioni alimentari
        </label>
        <div className="flex flex-col gap-2">
          {RESTRIZIONI_OPTIONS.map((opt) => (
            <label
              key={opt}
              className="flex cursor-pointer items-center gap-3 rounded-lg border border-zinc-200 px-4 py-2.5 text-sm hover:bg-zinc-50 dark:border-zinc-800 dark:hover:bg-zinc-900"
            >
              <input
                type="checkbox"
                checked={restrizioni.includes(opt)}
                onChange={() => toggleRestrizione(opt)}
                className="h-4 w-4"
              />
              {opt}
            </label>
          ))}
        </div>
      </div>

      <div>
        <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
          Numero di persone
        </label>
        <input
          type="number"
          min={1}
          value={householdSize}
          onChange={(e) => setHouseholdSize(e.target.value)}
          className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-sm focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
        />
      </div>

      <div>
        <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
          Obiettivo
        </label>
        <div className="flex flex-col gap-2">
          {OBIETTIVO_OPTIONS.map((opt) => (
            <label
              key={opt}
              className="flex cursor-pointer items-center gap-3 rounded-lg border border-zinc-200 px-4 py-2.5 text-sm hover:bg-zinc-50 dark:border-zinc-800 dark:hover:bg-zinc-900"
            >
              <input
                type="radio"
                name="obiettivo"
                checked={obiettivo === opt}
                onChange={() => setObiettivo(opt)}
                className="h-4 w-4"
              />
              {opt}
            </label>
          ))}
        </div>
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
              onClick={() => setCucina((p) => toggleInArray(p, opt))}
              className={`rounded-full border px-4 py-1.5 text-sm ${
                cucina.includes(opt)
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
          value={graditi}
          onChange={(e) => setGraditi(e.target.value)}
          className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-sm focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
        />
      </div>

      <div>
        <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
          Alimenti che non ti piacciono
        </label>
        <input
          type="text"
          value={nonGraditi}
          onChange={(e) => setNonGraditi(e.target.value)}
          className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-sm focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
        />
      </div>

      <div>
        <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
          Tempo massimo per cucinare
        </label>
        <div className="flex flex-col gap-2">
          {TEMPO_OPTIONS.map((opt) => (
            <label
              key={opt.value}
              className="flex cursor-pointer items-center gap-3 rounded-lg border border-zinc-200 px-4 py-2.5 text-sm hover:bg-zinc-50 dark:border-zinc-800 dark:hover:bg-zinc-900"
            >
              <input
                type="radio"
                name="tempo"
                checked={tempoMaxCucina === opt.value}
                onChange={() => setTempoMaxCucina(opt.value)}
                className="h-4 w-4"
              />
              {opt.label}
            </label>
          ))}
        </div>
      </div>

      <div>
        <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
          Budget settimanale
        </label>
        <div className="relative">
          <span className="absolute left-4 top-1/2 -translate-y-1/2 text-zinc-500">€</span>
          <input
            type="number"
            min={0}
            value={budgetSettimanale}
            onChange={(e) => setBudgetSettimanale(e.target.value)}
            className="w-full rounded-lg border border-zinc-300 bg-transparent py-2.5 pl-8 pr-4 text-sm focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
          />
        </div>
      </div>

      <div>
        <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
          Supermercato
        </label>
        <div className="flex flex-wrap gap-2">
          {SUPERMERCATO_OPTIONS.map((opt) => (
            <button
              type="button"
              key={opt}
              onClick={() => setSupermercato(opt)}
              className={`rounded-full border px-4 py-1.5 text-sm ${
                supermercato === opt
                  ? "border-black bg-black text-white dark:border-white dark:bg-white dark:text-black"
                  : "border-zinc-300 text-zinc-700 dark:border-zinc-700 dark:text-zinc-300"
              }`}
            >
              {opt}
            </button>
          ))}
        </div>
      </div>

      <div className="border-t border-zinc-200 pt-6 dark:border-zinc-800">
        <h3 className="mb-1 text-sm font-semibold text-zinc-800 dark:text-zinc-200">
          Dati biometrici (opzionali)
        </h3>
        <p className="mb-4 text-xs text-zinc-500 dark:text-zinc-400">
          Servono solo per mostrarti, nella sezione Menu, un confronto indicativo tra il piano e i
          valori di riferimento nutrizionali generali. Nessun dato viene usato per generare il
          piano.
        </p>

        <div className="flex flex-col gap-4">
          <div>
            <label className="mb-2 block text-sm font-medium text-zinc-700 dark:text-zinc-300">
              Sesso
            </label>
            <div className="flex gap-2">
              {(["M", "F"] as const).map((opt) => (
                <button
                  type="button"
                  key={opt}
                  onClick={() => setSesso(sesso === opt ? "" : opt)}
                  className={`rounded-full border px-4 py-1.5 text-sm ${
                    sesso === opt
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
              value={eta}
              onChange={(e) => setEta(e.target.value)}
              className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-sm focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
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
                value={pesoKg}
                onChange={(e) => setPesoKg(e.target.value)}
                className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-sm focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
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
                value={altezzaCm}
                onChange={(e) => setAltezzaCm(e.target.value)}
                className="w-full rounded-lg border border-zinc-300 bg-transparent px-4 py-2.5 text-sm focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
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
                  className="flex cursor-pointer items-center gap-3 rounded-lg border border-zinc-200 px-4 py-2.5 text-sm hover:bg-zinc-50 dark:border-zinc-800 dark:hover:bg-zinc-900"
                >
                  <input
                    type="radio"
                    name="livello_attivita"
                    checked={livelloAttivita === opt.value}
                    onChange={() => setLivelloAttivita(opt.value)}
                    className="h-4 w-4"
                  />
                  {opt.label}
                </label>
              ))}
            </div>
          </div>
        </div>
      </div>

      {errore && <p className="text-sm text-red-600 dark:text-red-400">{errore}</p>}
      {salvato && (
        <p className="text-sm text-green-700 dark:text-green-400">Impostazioni salvate.</p>
      )}

      <button
        onClick={handleSalva}
        disabled={salvando}
        className="self-start rounded-full bg-black px-6 py-2.5 text-sm font-medium text-white disabled:opacity-50 dark:bg-white dark:text-black"
      >
        {salvando ? "Salvo..." : "Salva impostazioni"}
      </button>
    </div>
  );
}
