"use client";

import { useMemo, useState } from "react";
import { ModalitaToggle } from "../modalita-toggle";
import { PageHeader } from "../page-header";
import { setModalita, scambiaPasti } from "../actions";
import { aggiungiPreferito, rimuoviPreferito } from "../preferiti-actions";
import { etichettaGiorno } from "@/lib/settimana";
import { conflittiDopoScambio, suggerisciGiornoAlternativo } from "@/lib/varieta-giorno";
import { formattaQuantita } from "@/lib/quantita";
import { Spinner } from "@/components/spinner";
import { HeartIcon } from "@/components/heart-icon";
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
  ingredienti_non_adatti?: string[];
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

function SwapIcon() {
  return (
    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <path d="M7 7h12l-3.5-3.5" />
      <path d="M17 17H5l3.5 3.5" />
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
  preferitiIniziali,
  householdSize,
}: {
  token: string;
  nome: string;
  initialModalita: "routine" | "scoperta";
  initialGiorni: Giorno[] | null;
  initialSettimana: string;
  budgetSettimanale: number | null;
  budgetStimatoIniziale: number | null;
  datiBiometrici: DatiBiometrici | null;
  preferitiIniziali: string[];
  householdSize: number | null;
}) {
  const [modalita, setModalitaState] = useState(initialModalita);
  const [cambiandoModalita, setCambiandoModalita] = useState(false);
  const [giorni, setGiorni] = useState<Giorno[] | null>(initialGiorni);
  const [preferiti, setPreferiti] = useState<Set<string>>(
    () => new Set(preferitiIniziali.map((n) => n.trim().toLowerCase())),
  );
  const [settimana, setSettimana] = useState(initialSettimana);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [budgetSuperato, setBudgetSuperato] = useState(
    Boolean(budgetSettimanale && budgetStimatoIniziale && budgetStimatoIniziale > budgetSettimanale),
  );
  const [budgetStimato, setBudgetStimato] = useState<number | null>(budgetStimatoIniziale);
  // Stesso fallback usato per generare il piano (vedi buildContestoProfilo
  // in claude.ts: "Numero di persone per cui cucinare: ... || 1"), così gli
  // ingredienti del piano restano coerenti con la quantità "a porzione"
  // mostrata sotto "Preparazione".
  const persone = householdSize || 1;

  const [messaggio, setMessaggio] = useState("");
  const [modificando, setModificando] = useState(false);
  const [erroreModifica, setErroreModifica] = useState<string | null>(null);
  const [rifiutoModifica, setRifiutoModifica] = useState<string | null>(null);
  const [nutrienteInCorso, setNutrienteInCorso] = useState<string | null>(null);
  const [pastoInCorso, setPastoInCorso] = useState<string | null>(null);
  const [scambioInCorso, setScambioInCorso] = useState(false);

  const [pastoEspanso, setPastoEspanso] = useState<string | null>(null);
  type SelezionePasto = { giorno: string; indice: number; nome: string };
  // Primo pasto scelto per lo scambio, poi il secondo (proposto) in attesa
  // di conferma esplicita prima di applicare davvero lo scambio.
  const [pastoSelezionato, setPastoSelezionato] = useState<SelezionePasto | null>(null);
  const [pastoProposto, setPastoProposto] = useState<SelezionePasto | null>(null);
  const [erroreScambio, setErroreScambio] = useState<string | null>(null);
  // Esito del controllo varietà leggero (nessuna AI) eseguito sui dati già
  // caricati, prima di mostrare la conferma dello scambio: se lo scambio
  // proposto farebbe finire lo stesso ingrediente principale sia a pranzo
  // che a cena in uno dei due giorni, lo segnala invece di proporre subito
  // la conferma normale.
  const [conflittoScambio, setConflittoScambio] = useState<{
    messaggio: string;
    alternativa: string | null;
  } | null>(null);
  const scambioInAttesaConferma = Boolean(pastoSelezionato) && Boolean(pastoProposto);

  const azioneInCorso =
    Boolean(nutrienteInCorso) || Boolean(pastoInCorso) || modificando || scambioInCorso || scambioInAttesaConferma;

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

  // Scambio tra due pasti di giorni diversi: puro riordino dei dati già nel
  // piano (stessi 14 pasti, stesso totale ingredienti), nessuna chiamata AI.
  // Tre tocchi: il primo arma la "modalità scambio" (card evidenziata,
  // banner con istruzioni); un secondo tocco sullo stesso giorno sposta
  // semplicemente la selezione; un tocco su un giorno diverso propone lo
  // scambio e mostra una conferma esplicita prima di applicarlo davvero.
  function valutaProposta(giornoA: string, indiceA: number, giornoB: string, indiceB: number) {
    if (!giorni) return;
    const conflitti = conflittiDopoScambio(giorni, giornoA, indiceA, giornoB, indiceB);
    if (conflitti.length === 0) {
      setConflittoScambio(null);
      return;
    }
    const alternativa = suggerisciGiornoAlternativo(giorni, giornoA, indiceA, giornoB, indiceB);
    const messaggio =
      conflitti
        .map((c) => `${c.giorno} avrebbe ${c.ingredienti.join(", ")} sia a pranzo che a cena`)
        .join("; ") + ".";
    setConflittoScambio({ messaggio, alternativa });
  }

  function handleTapScambia(giorno: Giorno, indice: number, pasto: Pasto) {
    if (azioneInCorso) return;
    setErroreScambio(null);

    if (!pastoSelezionato) {
      setPastoSelezionato({ giorno: giorno.giorno, indice, nome: pasto.nome });
      return;
    }

    if (pastoSelezionato.giorno === giorno.giorno) {
      setPastoSelezionato(
        pastoSelezionato.indice === indice ? null : { giorno: giorno.giorno, indice, nome: pasto.nome },
      );
      return;
    }

    setPastoProposto({ giorno: giorno.giorno, indice, nome: pasto.nome });
    valutaProposta(pastoSelezionato.giorno, pastoSelezionato.indice, giorno.giorno, indice);
  }

  // Passa alla proposta alternativa suggerita (stesso tipo di pasto, es.
  // pranzo con pranzo): il controllo varietà si rivaluta sul nuovo giorno.
  function handleProvaAlternativa(nomeGiorno: string) {
    if (!giorni || !pastoProposto) return;
    const record = giorni.find((g) => g.giorno === nomeGiorno);
    const pasto = record?.pasti[pastoProposto.indice];
    if (!record || !pasto) return;

    setPastoProposto({ giorno: nomeGiorno, indice: pastoProposto.indice, nome: pasto.nome });
    if (pastoSelezionato) {
      valutaProposta(pastoSelezionato.giorno, pastoSelezionato.indice, nomeGiorno, pastoProposto.indice);
    }
  }

  function handleAnnullaScambio() {
    setPastoSelezionato(null);
    setPastoProposto(null);
    setErroreScambio(null);
    setConflittoScambio(null);
  }

  async function handleConfermaScambio() {
    if (!pastoSelezionato || !pastoProposto) return;
    setScambioInCorso(true);
    const result = await scambiaPasti(
      token,
      pastoSelezionato.giorno,
      pastoSelezionato.indice,
      pastoProposto.giorno,
      pastoProposto.indice,
    );
    if ("error" in result) {
      setErroreScambio(result.error);
    } else {
      setGiorni(result.giorni as Giorno[]);
    }
    setPastoSelezionato(null);
    setPastoProposto(null);
    setConflittoScambio(null);
    setScambioInCorso(false);
  }

  // Preferiti (versione semplice): salva solo un'istantanea del piatto per
  // consultarla nella vista Preferiti, nessuna influenza sul motore di
  // generazione. Aggiornamento ottimistico, come lo stato "acquistato"
  // nella lista della spesa.
  async function handleToggleFavorito(pasto: Pasto) {
    const chiave = pasto.nome.trim().toLowerCase();
    const eraPreferito = preferiti.has(chiave);

    setPreferiti((prev) => {
      const next = new Set(prev);
      if (eraPreferito) next.delete(chiave);
      else next.add(chiave);
      return next;
    });

    const result = eraPreferito
      ? await rimuoviPreferito(token, pasto.nome)
      : await aggiungiPreferito(token, pasto);

    if ("error" in result) {
      setPreferiti((prev) => {
        const next = new Set(prev);
        if (eraPreferito) next.add(chiave);
        else next.delete(chiave);
        return next;
      });
    }
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

            {pastoSelezionato && !pastoProposto && (
              <div className="flex items-center justify-between gap-3 bg-panel px-4 py-3 text-sm text-ink">
                <span>
                  <strong className="font-semibold text-accent">Modalità scambio attiva.</strong> Tocca un pasto
                  in un altro giorno per scambiarlo con{" "}
                  <strong className="font-semibold">{pastoSelezionato.nome}</strong>.
                </span>
                <button
                  type="button"
                  onClick={handleAnnullaScambio}
                  className="shrink-0 text-xs font-semibold text-accent"
                >
                  Annulla
                </button>
              </div>
            )}

            {pastoSelezionato && pastoProposto && conflittoScambio && (
              <div className="flex flex-col gap-3 bg-honey-soft px-4 py-4 text-sm text-ink">
                <p>
                  <strong className="font-semibold text-honey">Attenzione:</strong> questo scambio ridurrebbe la
                  varietà del piano. {conflittoScambio.messaggio}
                </p>
                <div className="flex flex-wrap gap-2">
                  {conflittoScambio.alternativa && (
                    <button
                      type="button"
                      onClick={() => handleProvaAlternativa(conflittoScambio.alternativa as string)}
                      className="flex min-h-11 items-center justify-center rounded-full bg-accent px-5 text-sm font-semibold text-accent-fill-text"
                    >
                      Prova {conflittoScambio.alternativa} invece
                    </button>
                  )}
                  <button
                    type="button"
                    onClick={handleAnnullaScambio}
                    className="flex min-h-11 items-center justify-center rounded-full px-5 text-sm font-semibold text-ink"
                  >
                    Annulla
                  </button>
                  <button
                    type="button"
                    onClick={handleConfermaScambio}
                    disabled={scambioInCorso}
                    className="flex min-h-11 items-center justify-center text-xs font-medium text-ink/60 underline disabled:opacity-50"
                  >
                    {scambioInCorso ? "Scambio..." : "Scambia comunque"}
                  </button>
                </div>
              </div>
            )}

            {pastoSelezionato && pastoProposto && !conflittoScambio && (
              <div className="flex flex-col gap-3 bg-panel px-4 py-4 text-sm text-ink">
                <p>
                  Scambiare <strong className="font-semibold">{pastoSelezionato.nome}</strong> (
                  {pastoSelezionato.giorno}) con <strong className="font-semibold">{pastoProposto.nome}</strong> (
                  {pastoProposto.giorno})?
                </p>
                <div className="flex gap-2">
                  <button
                    type="button"
                    onClick={handleConfermaScambio}
                    disabled={scambioInCorso}
                    className="flex min-h-11 items-center justify-center gap-2 rounded-full bg-accent px-5 text-sm font-semibold text-accent-fill-text disabled:opacity-50"
                  >
                    {scambioInCorso && <Spinner className="h-4 w-4" />}
                    {scambioInCorso ? "Scambio..." : "Scambia"}
                  </button>
                  <button
                    type="button"
                    onClick={handleAnnullaScambio}
                    disabled={scambioInCorso}
                    className="flex min-h-11 items-center justify-center rounded-full px-5 text-sm font-semibold text-ink disabled:opacity-50"
                  >
                    Annulla
                  </button>
                </div>
              </div>
            )}
            {erroreScambio && <p className="text-sm text-clay">{erroreScambio}</p>}

            <div className="flex flex-col gap-7">
              {giorni.map((giorno) => (
                <div key={giorno.giorno}>
                  <h3 className="mb-2.5 text-[15px] font-semibold text-ink">
                    {etichettaGiorno(giorno.giorno, settimana)}
                  </h3>
                  <div className="flex flex-col gap-3">
                    {giorno.pasti.map((pasto, i) => {
                      const chiave = `${giorno.giorno}-${i}`;
                      const espanso = pastoEspanso === chiave;
                      const haPreparazione = Boolean(pasto.preparazione?.length);
                      const caricandoPasto = pastoInCorso === chiave;
                      const selezionatoPerScambio =
                        (pastoSelezionato?.giorno === giorno.giorno && pastoSelezionato?.indice === i) ||
                        (pastoProposto?.giorno === giorno.giorno && pastoProposto?.indice === i);
                      const preferito = preferiti.has(pasto.nome.trim().toLowerCase());
                      return (
                        <div key={i} className="flex flex-col gap-2">
                          <div className="flex items-stretch gap-2">
                            <div className="relative flex-1">
                              <button
                                type="button"
                                onClick={() => haPreparazione && setPastoEspanso(espanso ? null : chiave)}
                                disabled={!haPreparazione}
                                className={`block w-full rounded-[14px] bg-panel px-5 py-[18px] pr-14 text-left disabled:cursor-default ${
                                  selezionatoPerScambio ? "ring-2 ring-accent" : ""
                                }`}
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
                                type="button"
                                onClick={() => handleToggleFavorito(pasto)}
                                aria-label={
                                  preferito ? `Rimuovi ${pasto.nome} dai preferiti` : `Aggiungi ${pasto.nome} ai preferiti`
                                }
                                aria-pressed={preferito}
                                className={`absolute right-3 top-3 flex h-9 w-9 items-center justify-center ${
                                  preferito ? "text-accent" : "text-ink/35"
                                }`}
                              >
                                <HeartIcon pieno={preferito} />
                              </button>
                            </div>
                            <div className="flex w-11 shrink-0 flex-col gap-2">
                              <button
                                onClick={() => handlePastoDiverso(giorno, pasto, chiave)}
                                disabled={azioneInCorso}
                                aria-label={`Proponine un altro: ${pasto.tipo}`}
                                className="flex flex-1 items-center justify-center rounded-xl text-accent disabled:opacity-40"
                              >
                                {caricandoPasto ? <Spinner className="h-4 w-4" /> : <RefreshIcon />}
                              </button>
                              <button
                                onClick={() => handleTapScambia(giorno, i, pasto)}
                                disabled={azioneInCorso}
                                aria-label={`Scambia ${pasto.tipo} di ${giorno.giorno} con un altro giorno`}
                                aria-pressed={selezionatoPerScambio}
                                className={`flex flex-1 items-center justify-center rounded-xl disabled:opacity-40 ${
                                  selezionatoPerScambio ? "bg-accent text-accent-fill-text" : "text-accent"
                                }`}
                              >
                                <SwapIcon />
                              </button>
                            </div>
                          </div>

                          {espanso && haPreparazione && (
                            <div className="flex items-stretch gap-2">
                              <div className="flex-1 rounded-[14px] bg-panel px-5 py-4">
                                <h4 className="mb-2 text-sm font-semibold text-ink">
                                  Ingredienti {persone > 1 ? `· per ${persone} persone` : ""}
                                </h4>
                                <ul className="flex flex-col gap-1 text-sm text-ink/80">
                                  {pasto.ingredienti.map((ing, k) => (
                                    <li key={k} className="flex items-start justify-between gap-3">
                                      <span>{ing.nome}</span>
                                      <span className="shrink-0 whitespace-nowrap font-mono text-xs text-ink/60">
                                        {formattaQuantita(ing.quantita, ing.unita)}
                                        {persone > 1 &&
                                          ` · ${formattaQuantita(ing.quantita / persone, ing.unita)} a porzione`}
                                      </span>
                                    </li>
                                  ))}
                                </ul>

                                <h4 className="mb-1 mt-4 text-sm font-semibold text-ink">Preparazione</h4>
                                <ol className="flex list-decimal flex-col gap-1 pl-5 text-sm text-ink/80">
                                  {pasto.preparazione?.map((passo, j) => (
                                    <li key={j}>{passo}</li>
                                  ))}
                                </ol>
                              </div>
                              <div className="w-11 shrink-0" aria-hidden="true" />
                            </div>
                          )}

                          {pasto.verificare && pasto.ingredienti_non_adatti?.length ? (
                            <div className="flex gap-3 bg-clay-soft px-4 py-3">
                              <div className="w-1 shrink-0 bg-clay" />
                              <p className="text-[13px] leading-relaxed text-ink">
                                <span className="font-semibold text-clay">Non adatto.</span>{" "}
                                {pasto.ingredienti_non_adatti.join(", ")} contiene glutine: non sono riuscito a
                                sostituirlo automaticamente. Modifica questo pasto prima di procedere.
                              </p>
                            </div>
                          ) : (
                            pasto.verificare && (
                              <div className="flex gap-3 bg-honey-soft px-4 py-3">
                                <div className="w-1 shrink-0 bg-honey" />
                                <p className="text-[13px] leading-relaxed text-ink">
                                  <span className="font-semibold text-honey">Da verificare.</span>{" "}
                                  Il glutine in {pasto.ingredienti_a_rischio?.join(", ")} dipende dalla marca o dalla
                                  formulazione. Controlla l&apos;etichetta prima di procedere.
                                </p>
                              </div>
                            )
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
