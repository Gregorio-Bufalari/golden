"use client";

import { useMemo, useState } from "react";
import { ModalitaToggle } from "../modalita-toggle";
import { PageHeader } from "../page-header";
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
  prezzo_stimato_eur: number;
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

function ChevronIcon({ aperto }: { aperto: boolean }) {
  return (
    <svg
      width="14"
      height="14"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="2"
      strokeLinecap="round"
      strokeLinejoin="round"
      className={`shrink-0 transition-transform ${aperto ? "rotate-180" : ""}`}
    >
      <path d="M6 9l6 6 6-6" />
    </svg>
  );
}

function RefreshIcon() {
  return (
    <svg width="19" height="19" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <path d="M4 4v5h5" />
      <path d="M4.5 9A8 8 0 1 1 6 16" />
    </svg>
  );
}

export function MenuView({
  token,
  nome,
  initialModalita,
  initialGiorni,
  initialSettimana,
  budgetSettimanale,
  budgetStimatoIniziale,
  datiBiometrici,
}: {
  token: string;
  nome: string;
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
    <div className="flex flex-1 flex-col">
      <PageHeader
        token={token}
        logo
        title="Menu"
        subtitle={giorni ? `Ciao ${nome} · Settimana del ${settimana}${loading ? " · genero il nuovo piano..." : ""}` : `Ciao ${nome}`}
      />

      <div className="mx-auto flex w-full max-w-2xl flex-1 flex-col gap-5 px-5 pb-10">
        <ModalitaToggle
          modalita={modalita}
          onSwitch={handleSwitchModalita}
          disabled={cambiandoModalita || loading}
        />

        {!giorni && (
          <div className="flex flex-col items-center gap-3 py-10">
            <button
              onClick={handleGenerate}
              disabled={loading || cambiandoModalita}
              className="flex items-center gap-2 rounded-full bg-accent px-6 py-3 text-sm font-semibold text-accent-fill-text disabled:opacity-50"
            >
              {loading && <Spinner className="h-4 w-4" />}
              {loading ? "Genero il piano..." : "Genera il piano della settimana"}
            </button>
            {loading && <p className="text-xs text-ink/50">Può richiedere qualche secondo...</p>}
            {error && <p className="text-sm text-clay">{error}</p>}
          </div>
        )}

        {giorni && (
          <div className="flex flex-col gap-6 text-left">
            {modalita === "scoperta" && (
              <button
                onClick={handleGenerate}
                disabled={loading || cambiandoModalita}
                className="flex items-center gap-1.5 self-start rounded-full bg-panel px-4 py-2 text-xs font-semibold text-ink disabled:opacity-50"
              >
                {loading && <Spinner className="h-3.5 w-3.5" />}
                {loading ? "Genero..." : "Altri suggerimenti"}
              </button>
            )}
            {error && <p className="text-sm text-clay">{error}</p>}

            {budgetSuperato && budgetSettimanale && budgetStimato && (
              <div className="bg-honey-soft px-4 py-3 text-sm text-ink">
                Il piano supera il budget: stimato €{budgetStimato.toFixed(2)} contro €{budgetSettimanale}.
              </div>
            )}

            <div className="flex flex-col gap-7">
              {giorni.map((giorno) => (
                <div key={giorno.giorno}>
                  <h3 className="mb-2.5 text-[15px] font-semibold text-ink">{giorno.giorno}</h3>
                  <div className="flex flex-col gap-3">
                    {giorno.pasti.map((pasto, i) => {
                      const chiave = `${giorno.giorno}-${i}`;
                      const espanso = pastoEspanso === chiave;
                      const haPreparazione = Boolean(pasto.preparazione?.length);
                      const caricandoPasto = pastoInCorso === chiave;
                      return (
                        <div key={i} className="flex flex-col gap-2">
                          <div className="flex items-stretch gap-2">
                            <button
                              type="button"
                              onClick={() => haPreparazione && setPastoEspanso(espanso ? null : chiave)}
                              disabled={!haPreparazione}
                              className="flex-1 rounded-[14px] bg-panel px-5 py-[18px] text-left disabled:cursor-default"
                            >
                              <div className="text-[13px] font-medium text-ink/60">
                                {pasto.tipo === "pranzo" ? "Pranzo" : "Cena"} · {pasto.tempo_preparazione_min} min
                              </div>
                              <div className="mt-1 text-[21px] font-bold leading-tight tracking-tight text-ink">
                                {pasto.nome}
                              </div>
                              <p className="mt-1.5 text-[13px] text-ink/65">
                                {pasto.ingredienti.map((ing) => ing.nome).join(", ")}
                              </p>
                              <div className="mt-3.5 flex flex-wrap items-center gap-x-3 gap-y-1 font-mono text-[13px] text-ink/75">
                                <span>{pasto.nutrizione.calorie} kcal</span>
                                <span>{pasto.nutrizione.proteine_g}g proteine</span>
                                <span>{pasto.nutrizione.carboidrati_g}g carboidrati</span>
                                <span>{pasto.nutrizione.grassi_g}g grassi</span>
                                <span>{pasto.nutrizione.fibre_g}g fibre</span>
                              </div>
                              {haPreparazione && (
                                <span className="mt-2 flex items-center gap-1 font-sans text-xs font-medium text-accent">
                                  Preparazione
                                  <ChevronIcon aperto={espanso} />
                                </span>
                              )}
                            </button>
                            <button
                              onClick={() => handlePastoDiverso(giorno, pasto, chiave)}
                              disabled={azioneInCorso}
                              aria-label={`Proponine un altro: ${pasto.tipo}`}
                              className="flex w-11 shrink-0 items-center justify-center rounded-xl text-accent disabled:opacity-40"
                            >
                              {caricandoPasto ? <Spinner className="h-4 w-4" /> : <RefreshIcon />}
                            </button>
                          </div>

                          {espanso && haPreparazione && (
                            <div className="flex items-stretch gap-2">
                              <ol className="flex flex-1 list-decimal flex-col gap-1 rounded-[14px] bg-panel px-5 py-4 pl-9 text-sm text-ink/80">
                                {pasto.preparazione?.map((passo, j) => (
                                  <li key={j}>{passo}</li>
                                ))}
                              </ol>
                              <div className="w-11 shrink-0" aria-hidden="true" />
                            </div>
                          )}

                          {pasto.verificare && (
                            <div className="flex gap-3 bg-clay-soft px-4 py-3">
                              <div className="w-1 shrink-0 bg-clay" />
                              <p className="text-[13px] leading-relaxed text-ink">
                                <span className="font-semibold text-clay">Verifica necessaria.</span>{" "}
                                Possibili tracce di glutine in {pasto.ingredienti_a_rischio?.join(", ")}.
                                Controlla le etichette prima di procedere.
                              </p>
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
              <p className="text-xs text-ink/50">
                Inserisci sesso, età, peso, altezza e livello di attività nel{" "}
                <a href={`/piano/${token}/impostazioni`} className="text-accent underline">
                  Profilo
                </a>{" "}
                per vedere un confronto indicativo tra il piano e i valori di riferimento nutrizionali.
              </p>
            )}

            {confrontoLARN && (
              <div className="rounded-[14px] bg-panel p-5">
                <h3 className="mb-3.5 text-sm font-semibold text-ink">Confronto nutrizionale settimanale</h3>
                <div className="flex flex-col gap-2.5">
                  {confrontoLARN.map((n) => {
                    const inCorso = nutrienteInCorso === n.chiave;
                    const colore = n.fascia === "media" ? "text-accent" : "text-honey";
                    const puntino = n.fascia === "media" ? "bg-accent" : "bg-honey";
                    return (
                      <div key={n.chiave} className="flex items-center gap-2.5">
                        <span className={`h-2 w-2 shrink-0 rounded-full ${puntino}`} />
                        <span className="text-[13.5px] text-ink">{n.etichetta}</span>
                        <span className={`text-[13.5px] font-medium ${colore}`}>{n.fascia}</span>
                        <span className="font-mono text-[13px] text-ink/60">
                          {Math.round(n.totale)}/{Math.round(n.riferimento)}
                          {n.unita}
                        </span>
                        <span className="ml-auto flex items-center gap-1">
                          <button
                            onClick={() => handleAzioneNutriente(n, "Riduci")}
                            disabled={azioneInCorso}
                            aria-label={`Riduci ${n.etichetta.toLowerCase()}`}
                            className="flex h-11 w-11 items-center justify-center text-ink disabled:opacity-40"
                          >
                            <span className="flex h-7 w-7 items-center justify-center rounded-full border border-ink/20 text-sm">
                              −
                            </span>
                          </button>
                          <button
                            onClick={() => handleAzioneNutriente(n, "Aumenta")}
                            disabled={azioneInCorso}
                            aria-label={`Aumenta ${n.etichetta.toLowerCase()}`}
                            className="flex h-11 w-11 items-center justify-center text-ink disabled:opacity-40"
                          >
                            <span className="flex h-7 w-7 items-center justify-center rounded-full border border-ink/20 text-sm">
                              +
                            </span>
                          </button>
                          {inCorso && <Spinner className="h-3.5 w-3.5 text-ink/50" />}
                        </span>
                      </div>
                    );
                  })}
                </div>
                <p className="mt-4 text-xs text-ink/50">
                  Tra parentesi: questa settimana / riferimento. {DISCLAIMER_LARN}
                </p>
              </div>
            )}

            <div className="rounded-[14px] bg-panel p-5">
              <h3 className="mb-1.5 text-sm font-semibold text-ink">Modifica il piano</h3>
              <p className="mb-3 text-xs text-ink/55">
                Es. &quot;giovedì mangio fuori&quot;, &quot;ho già comprato il pollo&quot;, &quot;spendi meno
                questa settimana&quot;.
              </p>
              <textarea
                value={messaggio}
                onChange={(e) => setMessaggio(e.target.value)}
                rows={2}
                placeholder="Scrivi qui la modifica..."
                className="w-full rounded-xl bg-paper px-3.5 py-3 text-sm text-ink placeholder:text-ink/40 focus:outline-none focus:ring-2 focus:ring-accent/40"
              />
              {erroreModifica && <p className="mt-2 text-sm text-clay">{erroreModifica}</p>}
              {rifiutoModifica && (
                <div className="mt-2 bg-honey-soft px-3.5 py-2.5 text-sm text-ink">{rifiutoModifica}</div>
              )}
              <button
                onClick={handleModifica}
                disabled={!messaggio.trim() || azioneInCorso}
                className="mt-3.5 flex min-h-11 items-center gap-2 rounded-full bg-accent px-5 py-2.5 text-sm font-semibold text-accent-fill-text disabled:opacity-40"
              >
                {modificando && <Spinner className="h-4 w-4" />}
                {modificando ? "Applico la modifica..." : "Applica modifica"}
              </button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
