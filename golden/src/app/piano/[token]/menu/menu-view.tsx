"use client";

import { useMemo, useState } from "react";
import { ModalitaToggle } from "../modalita-toggle";
import { setModalita } from "../actions";
import { Spinner } from "@/components/spinner";
import {
  calcolaRiferimentoLARN,
  confrontaConLARN,
  sommaNutrizioneSettimanale,
  DISCLAIMER_LARN,
  type DatiBiometrici,
  type ConfrontoNutriente,
} from "@/lib/larn";

type Ingrediente = {
  nome: string;
  quantita: number;
  unita: "g" | "kg" | "ml" | "l" | "pz" | "confezione";
  reparto: string;
};

type Nutrizione = {
  calorie: number;
  proteine_g: number;
  carboidrati_g: number;
  grassi_g: number;
  fibre_g: number;
};

type Pasto = {
  tipo: "pranzo" | "cena";
  nome: string;
  ingredienti: Ingrediente[];
  tempo_preparazione_min: number;
  nutrizione: Nutrizione;
  preparazione?: string[];
  verificare?: boolean;
  ingredienti_a_rischio?: string[];
};

type Giorno = {
  giorno: string;
  pasti: Pasto[];
};

function formatQuantita(quantita: number, unita: Ingrediente["unita"]): string {
  if (unita === "g" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} kg`;
  }
  if (unita === "ml" && quantita >= 1000) {
    return `${(quantita / 1000).toFixed(quantita % 1000 === 0 ? 0 : 1)} l`;
  }
  const arrotondata = Math.round(quantita * 10) / 10;
  return `${arrotondata} ${unita}`;
}

export function MenuView({
  token,
  initialModalita,
  initialGiorni,
  initialSettimana,
  budgetSettimanale,
  budgetStimatoIniziale,
  datiBiometrici,
}: {
  token: string;
  initialModalita: "routine" | "scoperta";
  initialGiorni: Giorno[] | null;
  initialSettimana: string;
  budgetSettimanale: number | null;
  budgetStimatoIniziale: number | null;
  datiBiometrici: DatiBiometrici | null;
}) {
  const [modalita, setModalitaState] = useState(initialModalita);
  const [cambiandoModalita, setCambiandoModalita] = useState(false);
  const [giorni, setGiorni] = useState<Giorno[] | null>(initialGiorni);
  const [settimana, setSettimana] = useState(initialSettimana);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [budgetSuperato, setBudgetSuperato] = useState(
    Boolean(budgetSettimanale && budgetStimatoIniziale && budgetStimatoIniziale > budgetSettimanale),
  );
  const [budgetStimato, setBudgetStimato] = useState<number | null>(budgetStimatoIniziale);

  const [messaggio, setMessaggio] = useState("");
  const [modificando, setModificando] = useState(false);
  const [erroreModifica, setErroreModifica] = useState<string | null>(null);
  const [rifiutoModifica, setRifiutoModifica] = useState<string | null>(null);
  const [nutrienteInCorso, setNutrienteInCorso] = useState<string | null>(null);
  const [pastoInCorso, setPastoInCorso] = useState<string | null>(null);
  const azioneInCorso = Boolean(nutrienteInCorso) || Boolean(pastoInCorso) || modificando;

  const [pastoEspanso, setPastoEspanso] = useState<string | null>(null);

  const confrontoLARN = useMemo(() => {
    if (!giorni || !datiBiometrici) return null;

    const totali = sommaNutrizioneSettimanale(giorni);
    const riferimento = calcolaRiferimentoLARN(datiBiometrici);
    return confrontaConLARN(totali, riferimento);
  }, [giorni, datiBiometrici]);

  async function handleGenerate() {
    setLoading(true);
    setError(null);

    try {
      const res = await fetch("/api/piano/generate", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ token }),
      });
      const data = await res.json();

      if (!res.ok) {
        setError(data.error || "Qualcosa è andato storto.");
        return;
      }

      setGiorni(data.giorni);
      setSettimana(data.settimana);
      setBudgetSuperato(Boolean(data.budget_superato));
      setBudgetStimato(data.grocery_list?.totale_stimato ?? null);
    } catch {
      setError("Qualcosa è andato storto. Riprova.");
    } finally {
      setLoading(false);
    }
  }

  async function handleSwitchModalita(nuova: "routine" | "scoperta") {
    if (nuova === modalita || cambiandoModalita) return;
    setCambiandoModalita(true);
    const precedente = modalita;
    setModalitaState(nuova);

    const result = await setModalita(token, nuova);
    if ("error" in result) {
      setModalitaState(precedente);
      setCambiandoModalita(false);
      return;
    }

    await handleGenerate();
    setCambiandoModalita(false);
  }

  async function applicaModifica(testo: string): Promise<boolean> {
    setErroreModifica(null);
    setRifiutoModifica(null);

    try {
      const res = await fetch("/api/piano/modifica", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ token, messaggio: testo }),
      });
      const data = await res.json();

      if (!res.ok) {
        setErroreModifica(data.error || "Qualcosa è andato storto.");
        return false;
      }

      if (data.modifica_applicata === false) {
        setRifiutoModifica(data.motivo_rifiuto);
        return false;
      }

      setGiorni(data.giorni);
      setBudgetSuperato(Boolean(data.budget_superato));
      setBudgetStimato(data.grocery_list?.totale_stimato ?? null);
      return true;
    } catch {
      setErroreModifica("Qualcosa è andato storto. Riprova.");
      return false;
    }
  }

  async function handleModifica() {
    if (!messaggio.trim() || azioneInCorso) return;
    setModificando(true);
    const ok = await applicaModifica(messaggio);
    if (ok) setMessaggio("");
    setModificando(false);
  }

  async function handleAzioneNutriente(n: ConfrontoNutriente, direzione: "Aumenta" | "Riduci") {
    if (azioneInCorso) return;
    setNutrienteInCorso(n.chiave);
    // Un target concreto rispetto al valore ATTUALE (non "avvicinati al
    // riferimento"): quel testo si bloccava non appena il nutriente era già
    // vicino al riferimento (fascia "media"), impedendo di continuare a
    // spingerlo oltre in entrambe le direzioni se l'utente lo desiderava.
    const segno = direzione === "Aumenta" ? 1 : -1;
    const passo = Math.max(Math.round(n.totale * 0.15), Math.round(n.riferimento * 0.1), 1);
    const nuovoTarget = Math.max(0, Math.round(n.totale) + segno * passo);
    const verboAzione = direzione === "Aumenta" ? "Aumentalo" : "Riducilo";
    await applicaModifica(
      `Il totale di ${n.nomeModifica} in questo piano è circa ${Math.round(n.totale)}${n.unita} questa settimana ` +
        `(il riferimento indicativo LARN è ${Math.round(n.riferimento)}${n.unita}, solo per contesto). ` +
        `${verboAzione} in modo concreto, puntando a circa ${nuovoTarget}${n.unita} questa settimana — ` +
        "se serve cambia quali pasti lo contengono, non solo le porzioni — mantenendo le restrizioni e senza " +
        "stravolgere il resto del piano più del necessario.",
    );
    setNutrienteInCorso(null);
  }

  // Riusa lo stesso motore di modifica in linguaggio naturale: il nuovo
  // piatto passa sempre da validaGiorni (controllo glutine) e dal
  // ricalcolo di lista della spesa e Frigo, esattamente come ogni altra
  // modifica — nessun percorso separato.
  async function handlePastoDiverso(giorno: Giorno, pasto: Pasto, chiave: string) {
    if (azioneInCorso) return;
    setPastoInCorso(chiave);
    await applicaModifica(
      `Proponi un piatto diverso per ${giorno.giorno} ${pasto.tipo}, stesse restrizioni e preferenze.`,
    );
    setPastoInCorso(null);
  }

  return (
    <div className="mt-6 w-full max-w-2xl">
      <div className="mb-4 flex justify-center">
        <ModalitaToggle
          modalita={modalita}
          onSwitch={handleSwitchModalita}
          disabled={cambiandoModalita || loading}
        />
      </div>

      {!giorni && (
        <div className="flex flex-col items-center gap-3">
          <button
            onClick={handleGenerate}
            disabled={loading || cambiandoModalita}
            className="flex items-center gap-2 rounded-full bg-black px-6 py-2.5 text-sm font-medium text-white disabled:opacity-50 dark:bg-white dark:text-black"
          >
            {loading && <Spinner className="h-4 w-4" />}
            {loading ? "Genero il piano..." : "Genera il piano della settimana"}
          </button>
          {loading && (
            <p className="text-xs text-zinc-400">Può richiedere qualche secondo...</p>
          )}
          {error && <p className="text-sm text-red-600 dark:text-red-400">{error}</p>}
        </div>
      )}

      {giorni && (
        <div className="flex flex-col gap-6 text-left">
          <div className="flex items-center justify-between">
            <p className="text-xs text-zinc-400">
              Settimana del {settimana}
              {loading && " · genero il nuovo piano, qualche secondo..."}
            </p>
            {modalita === "scoperta" && (
              <button
                onClick={handleGenerate}
                disabled={loading || cambiandoModalita}
                className="flex items-center gap-1.5 rounded-full border border-zinc-300 px-4 py-1.5 text-xs font-medium text-zinc-700 disabled:opacity-50 dark:border-zinc-700 dark:text-zinc-300"
              >
                {loading && <Spinner className="h-3.5 w-3.5" />}
                {loading ? "Genero..." : "Altri suggerimenti"}
              </button>
            )}
          </div>
          {error && <p className="text-sm text-red-600 dark:text-red-400">{error}</p>}

          {budgetSuperato && budgetSettimanale && budgetStimato && (
            <div className="rounded-xl border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-800 dark:border-amber-900 dark:bg-amber-950 dark:text-amber-300">
              Il piano supera il budget: stimato €{budgetStimato.toFixed(2)} contro €{budgetSettimanale}.
            </div>
          )}

          <div className="flex flex-col gap-6">
            {giorni.map((giorno) => (
              <div
                key={giorno.giorno}
                className="rounded-xl border border-zinc-200 p-5 dark:border-zinc-800"
              >
                <h3 className="mb-3 text-lg font-semibold text-zinc-950 dark:text-zinc-50">
                  {giorno.giorno}
                </h3>
                <div className="flex flex-col gap-4">
                  {giorno.pasti.map((pasto, i) => {
                    const chiave = `${giorno.giorno}-${i}`;
                    const espanso = pastoEspanso === chiave;
                    const haPreparazione = Boolean(pasto.preparazione?.length);
                    return (
                      <div key={i}>
                        <button
                          type="button"
                          onClick={() => haPreparazione && setPastoEspanso(espanso ? null : chiave)}
                          className={`flex w-full flex-wrap items-center gap-2 text-left ${
                            haPreparazione ? "cursor-pointer" : "cursor-default"
                          }`}
                        >
                          <span className="text-xs font-medium uppercase text-zinc-500 dark:text-zinc-500">
                            {pasto.tipo}
                          </span>
                          <span className="font-medium text-zinc-900 underline decoration-dotted underline-offset-4 dark:text-zinc-100">
                            {pasto.nome}
                          </span>
                          <span className="text-xs text-zinc-400">
                            ({pasto.tempo_preparazione_min} min)
                          </span>
                          {haPreparazione && (
                            <span className="text-xs text-zinc-400">
                              {espanso ? "▲ nascondi preparazione" : "▼ vedi preparazione"}
                            </span>
                          )}
                        </button>
                        <p className="mt-1 text-sm text-zinc-600 dark:text-zinc-400">
                          {pasto.ingredienti
                            .map((ing) => `${ing.nome} (${formatQuantita(ing.quantita, ing.unita)})`)
                            .join(", ")}
                        </p>
                        {pasto.nutrizione && (
                          <p className="mt-1 text-xs text-zinc-400">
                            {pasto.nutrizione.calorie} kcal · {pasto.nutrizione.proteine_g}g proteine ·{" "}
                            {pasto.nutrizione.carboidrati_g}g carboidrati · {pasto.nutrizione.grassi_g}g grassi ·{" "}
                            {pasto.nutrizione.fibre_g}g fibre
                          </p>
                        )}
                        <div className="mt-1.5 flex items-center gap-2">
                          <button
                            onClick={() => handlePastoDiverso(giorno, pasto, chiave)}
                            disabled={azioneInCorso}
                            className="rounded-full border border-zinc-300 px-2 py-0.5 text-[11px] font-medium text-zinc-500 disabled:opacity-40 dark:border-zinc-700 dark:text-zinc-400"
                          >
                            Proponine un altro
                          </button>
                          {pastoInCorso === chiave && <Spinner className="h-3.5 w-3.5 text-zinc-400" />}
                        </div>
                        {espanso && haPreparazione && (
                          <ol className="mt-2 flex list-decimal flex-col gap-1 pl-5 text-sm text-zinc-600 dark:text-zinc-400">
                            {pasto.preparazione?.map((passo, j) => (
                              <li key={j}>{passo}</li>
                            ))}
                          </ol>
                        )}
                        {pasto.verificare && (
                          <div className="mt-2 rounded-lg border border-red-200 bg-red-50 px-3 py-2 text-xs text-red-700 dark:border-red-900 dark:bg-red-950 dark:text-red-400">
                            ⚠️ Verifica necessaria: possibili tracce di glutine in{" "}
                            {pasto.ingredienti_a_rischio?.join(", ")}. Controlla le etichette
                            prima di procedere.
                          </div>
                        )}
                      </div>
                    );
                  })}
                </div>
              </div>
            ))}
          </div>

          {!confrontoLARN && (
            <p className="text-xs text-zinc-400">
              Inserisci sesso, età, peso, altezza e livello di attività nelle{" "}
              <a href={`/piano/${token}/impostazioni`} className="underline">
                Impostazioni
              </a>{" "}
              per vedere un confronto indicativo tra il piano e i valori di riferimento
              nutrizionali.
            </p>
          )}

          {confrontoLARN && (
            <div className="rounded-xl border border-zinc-200 p-5 dark:border-zinc-800">
              <h3 className="mb-3 text-sm font-semibold text-zinc-800 dark:text-zinc-200">
                Confronto nutrizionale settimanale
              </h3>
              <div className="flex flex-wrap gap-2">
                {confrontoLARN.map((n) => {
                  const inCorso = nutrienteInCorso === n.chiave;
                  const coloreFascia =
                    n.fascia === "media"
                      ? "text-green-700 dark:text-green-400"
                      : "text-amber-700 dark:text-amber-400";
                  return (
                    <div
                      key={n.chiave}
                      className="flex items-center gap-1.5 rounded-full border border-zinc-200 py-1.5 pl-3 pr-1.5 text-xs dark:border-zinc-800"
                    >
                      <span className="text-zinc-500 dark:text-zinc-400">{n.etichetta}</span>
                      <span className={`font-medium ${coloreFascia}`}>{n.fascia}</span>
                      <span className="text-zinc-400">
                        ({Math.round(n.totale)}/{Math.round(n.riferimento)}
                        {n.unita})
                      </span>
                      <button
                        onClick={() => handleAzioneNutriente(n, "Riduci")}
                        disabled={azioneInCorso}
                        aria-label={`Riduci ${n.etichetta.toLowerCase()}`}
                        className="flex h-5 w-5 items-center justify-center rounded-full border border-zinc-300 disabled:opacity-40 dark:border-zinc-700"
                      >
                        −
                      </button>
                      <button
                        onClick={() => handleAzioneNutriente(n, "Aumenta")}
                        disabled={azioneInCorso}
                        aria-label={`Aumenta ${n.etichetta.toLowerCase()}`}
                        className="flex h-5 w-5 items-center justify-center rounded-full border border-zinc-300 disabled:opacity-40 dark:border-zinc-700"
                      >
                        +
                      </button>
                      {inCorso && <Spinner className="h-3.5 w-3.5 text-zinc-400" />}
                    </div>
                  );
                })}
              </div>
              <p className="mt-3 text-xs text-zinc-400">
                Tra parentesi: questa settimana / riferimento. {DISCLAIMER_LARN}
              </p>
            </div>
          )}

          <div className="rounded-xl border border-zinc-200 p-5 dark:border-zinc-800">
            <h3 className="mb-2 text-sm font-semibold text-zinc-800 dark:text-zinc-200">
              Modifica il piano
            </h3>
            <p className="mb-3 text-xs text-zinc-500 dark:text-zinc-400">
              Es. &quot;giovedì mangio fuori&quot;, &quot;ho già comprato il pollo&quot;,
              &quot;spendi meno questa settimana&quot;.
            </p>
            <textarea
              value={messaggio}
              onChange={(e) => setMessaggio(e.target.value)}
              rows={2}
              placeholder="Scrivi qui la modifica..."
              className="w-full rounded-lg border border-zinc-300 bg-transparent px-3 py-2 text-sm placeholder:text-zinc-400 focus:border-black focus:outline-none dark:border-zinc-700 dark:focus:border-white"
            />
            {erroreModifica && (
              <p className="mt-2 text-sm text-red-600 dark:text-red-400">{erroreModifica}</p>
            )}
            {rifiutoModifica && (
              <div className="mt-2 rounded-lg border border-amber-200 bg-amber-50 px-3 py-2 text-sm text-amber-800 dark:border-amber-900 dark:bg-amber-950 dark:text-amber-300">
                {rifiutoModifica}
              </div>
            )}
            <button
              onClick={handleModifica}
              disabled={!messaggio.trim() || azioneInCorso}
              className="mt-3 flex items-center gap-2 rounded-full bg-black px-5 py-2 text-sm font-medium text-white disabled:opacity-40 dark:bg-white dark:text-black"
            >
              {modificando && <Spinner className="h-4 w-4" />}
              {modificando ? "Applico la modifica..." : "Applica modifica"}
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
