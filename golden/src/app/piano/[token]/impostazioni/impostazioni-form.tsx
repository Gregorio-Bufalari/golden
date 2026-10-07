"use client";

import { useState } from "react";
import { updateProfilo } from "./actions";
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

export type ProfileData = {
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
  obiettivi_nutrizionali: {
    calorie_min?: number | null;
    calorie_max?: number | null;
    proteine_min_g?: number | null;
    carboidrati_max_g?: number | null;
    grassi_max_g?: number | null;
  } | null;
};

function toggleInArray(list: string[], value: string): string[] {
  return list.includes(value) ? list.filter((v) => v !== value) : [...list, value];
}

export function ImpostazioniForm({
  token,
  profile,
  onSalvato,
  onAnnulla,
}: {
  token: string;
  profile: ProfileData;
  onSalvato: (profilo: ProfileData) => void;
  onAnnulla: () => void;
}) {
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
  const [calorieMin, setCalorieMin] = useState(
    profile.obiettivi_nutrizionali?.calorie_min ? String(profile.obiettivi_nutrizionali.calorie_min) : "",
  );
  const [calorieMax, setCalorieMax] = useState(
    profile.obiettivi_nutrizionali?.calorie_max ? String(profile.obiettivi_nutrizionali.calorie_max) : "",
  );
  const [proteineMinG, setProteineMinG] = useState(
    profile.obiettivi_nutrizionali?.proteine_min_g ? String(profile.obiettivi_nutrizionali.proteine_min_g) : "",
  );
  const [carboidratiMaxG, setCarboidratiMaxG] = useState(
    profile.obiettivi_nutrizionali?.carboidrati_max_g
      ? String(profile.obiettivi_nutrizionali.carboidrati_max_g)
      : "",
  );
  const [grassiMaxG, setGrassiMaxG] = useState(
    profile.obiettivi_nutrizionali?.grassi_max_g ? String(profile.obiettivi_nutrizionali.grassi_max_g) : "",
  );

  const [salvando, setSalvando] = useState(false);
  const [errore, setErrore] = useState<string | null>(null);

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

    const householdSizeNum = householdSize ? Number(householdSize) : null;
    const budgetNum = budgetSettimanale ? Number(budgetSettimanale) : null;
    const etaNum = eta ? Number(eta) : null;
    const pesoNum = pesoKg ? Number(pesoKg) : null;
    const altezzaNum = altezzaCm ? Number(altezzaCm) : null;
    const obiettiviNutrizionali = {
      calorie_min: calorieMin ? Number(calorieMin) : null,
      calorie_max: calorieMax ? Number(calorieMax) : null,
      proteine_min_g: proteineMinG ? Number(proteineMinG) : null,
      carboidrati_max_g: carboidratiMaxG ? Number(carboidratiMaxG) : null,
      grassi_max_g: grassiMaxG ? Number(grassiMaxG) : null,
    };

    const result = await updateProfilo(token, {
      nome,
      restrizioni,
      household_size: householdSizeNum,
      obiettivo,
      preferenze: { cucina, graditi, non_graditi: nonGraditi },
      tempo_max_cucina: tempoMaxCucina,
      budget_settimanale: budgetNum,
      supermercato,
      sesso: sesso || null,
      eta: etaNum,
      peso_kg: pesoNum,
      altezza_cm: altezzaNum,
      livello_attivita: livelloAttivita || null,
      obiettivi_nutrizionali: obiettiviNutrizionali,
    });

    if ("error" in result) {
      setErrore(result.error);
      setSalvando(false);
    } else {
      onSalvato({
        nome,
        restrizioni,
        household_size: householdSizeNum,
        obiettivo: obiettivo || null,
        preferenze: { cucina, graditi, non_graditi: nonGraditi },
        tempo_max_cucina: tempoMaxCucina,
        budget_settimanale: budgetNum,
        supermercato: supermercato || null,
        sesso: sesso || null,
        eta: etaNum,
        peso_kg: pesoNum,
        altezza_cm: altezzaNum,
        livello_attivita: livelloAttivita || null,
        obiettivi_nutrizionali: obiettiviNutrizionali,
      });
    }
  }

  return (
    <div className="flex flex-col gap-6 pb-10 text-left">
      <div>
        <label className={labelClass}>Nome</label>
        <input type="text" value={nome} onChange={(e) => setNome(e.target.value)} className={inputClass} />
      </div>

      <div>
        <label className={labelClass}>Restrizioni alimentari</label>
        <div className="flex flex-col gap-1.5">
          {RESTRIZIONI_OPTIONS.map((opt) => (
            <label key={opt} className={optionRowClass}>
              <input
                type="checkbox"
                checked={restrizioni.includes(opt)}
                onChange={() => toggleRestrizione(opt)}
                className={checkboxClass}
              />
              {opt}
            </label>
          ))}
        </div>
      </div>

      <div>
        <label className={labelClass}>Numero di persone</label>
        <input
          type="number"
          min={1}
          value={householdSize}
          onChange={(e) => setHouseholdSize(e.target.value)}
          className={inputClass}
        />
      </div>

      <div>
        <label className={labelClass}>Obiettivo</label>
        <div className="flex flex-col gap-1.5">
          {OBIETTIVO_OPTIONS.map((opt) => (
            <label key={opt} className={optionRowClass}>
              <input
                type="radio"
                name="obiettivo"
                checked={obiettivo === opt}
                onChange={() => setObiettivo(opt)}
                className={checkboxClass}
              />
              {opt}
            </label>
          ))}
        </div>
      </div>

      <div>
        <label className={labelClass}>Cucina preferita</label>
        <div className="flex flex-wrap gap-2">
          {CUCINA_OPTIONS.map((opt) => (
            <Pill key={opt} label={opt} selected={cucina.includes(opt)} onClick={() => setCucina((p) => toggleInArray(p, opt))} />
          ))}
        </div>
      </div>

      <div>
        <label className={labelClass}>Alimenti che ti piacciono</label>
        <input type="text" value={graditi} onChange={(e) => setGraditi(e.target.value)} className={inputClass} />
      </div>

      <div>
        <label className={labelClass}>Alimenti che non ti piacciono</label>
        <input type="text" value={nonGraditi} onChange={(e) => setNonGraditi(e.target.value)} className={inputClass} />
      </div>

      <div>
        <label className={labelClass}>Tempo massimo per cucinare</label>
        <div className="flex flex-col gap-1.5">
          {TEMPO_OPTIONS.map((opt) => (
            <label key={opt.value} className={optionRowClass}>
              <input
                type="radio"
                name="tempo"
                checked={tempoMaxCucina === opt.value}
                onChange={() => setTempoMaxCucina(opt.value)}
                className={checkboxClass}
              />
              {opt.label}
            </label>
          ))}
        </div>
      </div>

      <div>
        <label className={labelClass}>Budget settimanale</label>
        <div className="relative">
          <span className="absolute left-4 top-1/2 -translate-y-1/2 text-ink/50">€</span>
          <input
            type="number"
            min={0}
            value={budgetSettimanale}
            onChange={(e) => setBudgetSettimanale(e.target.value)}
            className={`${inputClass} pl-8`}
          />
        </div>
      </div>

      <div>
        <label className={labelClass}>Supermercato</label>
        <p className="mb-2 text-xs text-ink/55">
          Opzionale. Ci serve solo per stimare meglio i prezzi, mai per favorirlo.
        </p>
        <SupermercatoSelector value={supermercato || null} onChange={setSupermercato} />
      </div>

      <div className="border-t border-ink/10 pt-6">
        <h3 className="mb-1 text-sm font-semibold text-ink">Dati biometrici</h3>
        <p className="mb-4 text-xs text-ink/55">
          Opzionali. Servono solo per mostrarti, nella sezione Menu, un confronto indicativo tra il piano e i
          valori di riferimento nutrizionali generali. Nessun dato viene usato per generare il piano.
        </p>

        <div className="flex flex-col gap-4">
          <div>
            <label className={labelClass}>Sesso</label>
            <div className="flex gap-2">
              {(["M", "F"] as const).map((opt) => (
                <Pill
                  key={opt}
                  label={opt === "M" ? "Maschio" : "Femmina"}
                  selected={sesso === opt}
                  onClick={() => setSesso(sesso === opt ? "" : opt)}
                />
              ))}
            </div>
          </div>

          <div>
            <label className={labelClass}>Età</label>
            <input type="number" min={1} value={eta} onChange={(e) => setEta(e.target.value)} className={inputClass} />
          </div>

          <div className="flex gap-3">
            <div className="flex-1">
              <label className={labelClass}>Peso (kg)</label>
              <input
                type="number"
                min={1}
                step="0.1"
                value={pesoKg}
                onChange={(e) => setPesoKg(e.target.value)}
                className={inputClass}
              />
            </div>
            <div className="flex-1">
              <label className={labelClass}>Altezza (cm)</label>
              <input
                type="number"
                min={1}
                step="0.5"
                value={altezzaCm}
                onChange={(e) => setAltezzaCm(e.target.value)}
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
                    checked={livelloAttivita === opt.value}
                    onChange={() => setLivelloAttivita(opt.value)}
                    className={checkboxClass}
                  />
                  {opt.label}
                </label>
              ))}
            </div>
          </div>
        </div>
      </div>

      <div className="border-t border-ink/10 pt-6">
        <h3 className="mb-1 text-sm font-semibold text-ink">Obiettivi nutrizionali per pasto</h3>
        <p className="mb-4 text-xs text-ink/55">
          Opzionali. Un vincolo in più per il motore, con la stessa priorità dell&apos;obiettivo generale: viene
          rispettato quando possibile, ma mai a scapito delle restrizioni alimentari o del budget.
        </p>

        <div className="flex flex-col gap-4">
          <div className="flex gap-3">
            <div className="flex-1">
              <label className={labelClass}>Calorie minime (kcal)</label>
              <input
                type="number"
                min={0}
                value={calorieMin}
                onChange={(e) => setCalorieMin(e.target.value)}
                className={inputClass}
              />
            </div>
            <div className="flex-1">
              <label className={labelClass}>Calorie massime (kcal)</label>
              <input
                type="number"
                min={0}
                value={calorieMax}
                onChange={(e) => setCalorieMax(e.target.value)}
                className={inputClass}
              />
            </div>
          </div>

          <div>
            <label className={labelClass}>Proteine minime (g)</label>
            <input
              type="number"
              min={0}
              value={proteineMinG}
              onChange={(e) => setProteineMinG(e.target.value)}
              className={inputClass}
            />
          </div>

          <div className="flex gap-3">
            <div className="flex-1">
              <label className={labelClass}>Carboidrati massimi (g)</label>
              <input
                type="number"
                min={0}
                value={carboidratiMaxG}
                onChange={(e) => setCarboidratiMaxG(e.target.value)}
                className={inputClass}
              />
            </div>
            <div className="flex-1">
              <label className={labelClass}>Grassi massimi (g)</label>
              <input
                type="number"
                min={0}
                value={grassiMaxG}
                onChange={(e) => setGrassiMaxG(e.target.value)}
                className={inputClass}
              />
            </div>
          </div>
        </div>
      </div>

      {errore && <p className="text-sm text-clay">{errore}</p>}

      <div className="flex items-center gap-2">
        <button
          onClick={handleSalva}
          disabled={salvando}
          className="flex min-h-11 items-center justify-center gap-2 self-start rounded-full bg-accent px-6 text-sm font-semibold text-accent-fill-text disabled:opacity-50"
        >
          {salvando && <Spinner className="h-4 w-4" />}
          {salvando ? "Salvo..." : "Salva impostazioni"}
        </button>
        <button
          type="button"
          onClick={onAnnulla}
          disabled={salvando}
          className="flex min-h-11 items-center justify-center self-start rounded-full px-6 text-sm font-semibold text-ink disabled:opacity-50"
        >
          Annulla
        </button>
      </div>
    </div>
  );
}
