"use client";

import { useState } from "react";
import { submitCheckin } from "./checkin-actions";
import { Spinner } from "@/components/spinner";
import { FeedbackPopup } from "@/components/feedback-popup";
import { SupermercatoSelector } from "@/components/supermercato-selector";

const CATEGORIE_SPRECO = ["Verdura", "Proteine", "Latticini", "Pane/pasta", "Altro"];

type CorrispondenzaScontrino = {
  nome_lista: string;
  prezzo_stimato_eur: number;
  trovato_sullo_scontrino: boolean;
  nome_scontrino: string | null;
  prezzo_scontrino_eur: number | null;
};

type RisultatoScontrino = {
  leggibile: boolean;
  totale_scontrino_eur: number | null;
  corrispondenze: CorrispondenzaScontrino[];
  extra_non_in_lista: { nome: string; prezzo_eur: number }[];
};

// Ridimensiona e ricomprime la foto prima di inviarla: una foto scattata
// con una fotocamera moderna può superare facilmente i limiti di corpo
// richiesta, e immagini più piccole costano anche meno token di visione.
function comprimiImmagine(file: File, maxLato = 1500, qualita = 0.8): Promise<{ base64: string; mediaType: "image/jpeg" }> {
  return new Promise((resolve, reject) => {
    const img = new window.Image();
    const url = URL.createObjectURL(file);

    img.onload = () => {
      URL.revokeObjectURL(url);
      let { width, height } = img;
      if (width > maxLato || height > maxLato) {
        const scala = maxLato / Math.max(width, height);
        width = Math.round(width * scala);
        height = Math.round(height * scala);
      }

      const canvas = document.createElement("canvas");
      canvas.width = width;
      canvas.height = height;
      const ctx = canvas.getContext("2d");
      if (!ctx) {
        reject(new Error("Canvas non disponibile"));
        return;
      }
      ctx.drawImage(img, 0, 0, width, height);

      const dataUrl = canvas.toDataURL("image/jpeg", qualita);
      resolve({ base64: dataUrl.split(",")[1], mediaType: "image/jpeg" });
    };
    img.onerror = () => {
      URL.revokeObjectURL(url);
      reject(new Error("Immagine non valida"));
    };
    img.src = url;
  });
}

function SiNoButton({
  label,
  selected,
  onClick,
}: {
  label: string;
  selected: boolean;
  onClick: () => void;
}) {
  return (
    <button
      onClick={onClick}
      className={`min-h-11 flex-1 rounded-[10px] text-sm font-semibold ${
        selected ? "bg-accent text-accent-fill-text" : "border border-ink/25 text-ink"
      }`}
    >
      {label}
    </button>
  );
}

function PillOption({
  label,
  selected,
  onClick,
}: {
  label: string;
  selected: boolean;
  onClick: () => void;
}) {
  return (
    <button
      onClick={onClick}
      className={`min-h-11 rounded-full px-3.5 text-[13px] font-semibold ${
        selected ? "bg-paper text-accent" : "border border-ink/20 text-ink"
      }`}
    >
      {label}
    </button>
  );
}

export function CheckinForm({ token }: { token: string }) {
  const [seguitoPiano, setSeguitoPiano] = useState<boolean | null>(null);
  const [spreco, setSpreco] = useState<boolean | null>(null);
  const [categoriaSpreco, setCategoriaSpreco] = useState<string | null>(null);
  const [spesaReale, setSpesaReale] = useState("");
  const [retailer, setRetailer] = useState<string | null>(null);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [inviato, setInviato] = useState(false);

  const [scontrinoElaborando, setScontrinoElaborando] = useState(false);
  const [erroreScontrino, setErroreScontrino] = useState<string | null>(null);
  const [confrontoScontrino, setConfrontoScontrino] = useState<RisultatoScontrino | null>(null);

  const puoInviare =
    seguitoPiano !== null && spreco !== null && (!spreco || categoriaSpreco !== null);

  async function handleSubmit() {
    if (seguitoPiano === null || spreco === null) return;

    setSubmitting(true);
    setError(null);

    const result = await submitCheckin(token, {
      seguito_piano: seguitoPiano,
      spreco,
      categoria_spreco: spreco ? categoriaSpreco : null,
      spesa_reale: spesaReale ? Number(spesaReale) : null,
      retailer_usato: retailer,
    });

    if ("error" in result) {
      setError(result.error);
    } else {
      setInviato(true);
    }
    setSubmitting(false);
  }

  // Confronto scontrino (versione semplice): la foto viene elaborata e
  // confrontata al volo, mai salvata — solo il risultato (e il totale, per
  // precompilare "quanto hai speso") resta nello stato di questo form.
  async function handleScontrino(e: React.ChangeEvent<HTMLInputElement>) {
    const file = e.target.files?.[0];
    e.target.value = "";
    if (!file) return;

    setScontrinoElaborando(true);
    setErroreScontrino(null);
    setConfrontoScontrino(null);

    try {
      const { base64, mediaType } = await comprimiImmagine(file);
      const res = await fetch("/api/scontrino/estrai", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ token, immagine_base64: base64, media_type: mediaType }),
      });
      const data = await res.json();

      if (!res.ok) {
        setErroreScontrino(data.error || "Qualcosa è andato storto.");
        return;
      }
      if (!data.leggibile) {
        setErroreScontrino("Non sono riuscito a leggere lo scontrino. Riprova con una foto più chiara.");
        return;
      }

      setConfrontoScontrino(data);
      if (data.totale_scontrino_eur != null) {
        setSpesaReale(String(data.totale_scontrino_eur));
      }
    } catch {
      setErroreScontrino("Qualcosa è andato storto. Riprova.");
    } finally {
      setScontrinoElaborando(false);
    }
  }

  if (inviato) {
    return (
      <div className="flex flex-col gap-3">
        <div className="bg-panel px-5 py-6 text-center text-sm font-medium text-ink">Grazie! Check-in salvato.</div>
        <FeedbackPopup
          token={token}
          contesto="checkin"
          domanda="Il check-in ti aiuta a tenere traccia della settimana?"
        />
      </div>
    );
  }

  return (
    <div className="flex flex-col gap-3 text-left print:hidden">
      <div className="bg-panel rounded-[14px] px-5 py-[18px]">
        <p className="text-base font-semibold text-ink">Hai seguito il piano?</p>
        <div className="mt-3.5 flex gap-2.5">
          <SiNoButton label="Sì" selected={seguitoPiano === true} onClick={() => setSeguitoPiano(true)} />
          <SiNoButton label="No" selected={seguitoPiano === false} onClick={() => setSeguitoPiano(false)} />
        </div>
      </div>

      <div className="bg-panel rounded-[14px] px-5 py-[18px]">
        <p className="text-base font-semibold text-ink">Hai sprecato qualcosa?</p>
        <div className="mt-3.5 flex gap-2.5">
          <SiNoButton
            label="No"
            selected={spreco === false}
            onClick={() => {
              setSpreco(false);
              setCategoriaSpreco(null);
            }}
          />
          <SiNoButton label="Sì" selected={spreco === true} onClick={() => setSpreco(true)} />
        </div>

        {spreco && (
          <>
            <p className="mb-2 mt-3.5 text-[13px] text-ink/65">Cosa, principalmente?</p>
            <div className="flex flex-wrap gap-2">
              {CATEGORIE_SPRECO.map((cat) => (
                <PillOption
                  key={cat}
                  label={cat}
                  selected={categoriaSpreco === cat}
                  onClick={() => setCategoriaSpreco(cat)}
                />
              ))}
            </div>
          </>
        )}
      </div>

      <div className="bg-panel rounded-[14px] px-5 py-[18px]">
        <p className="text-base font-semibold text-ink">Quanto hai speso davvero, e dove?</p>
        <p className="mt-0.5 text-xs text-ink/55">
          Opzionale. Ci aiuta a migliorare le stime dei prezzi nel tempo: il supermercato scelto qui
          è quello usato davvero questa settimana, anche se diverso da quello di riferimento in Profilo.
        </p>
        <div className="mt-3.5 flex items-center gap-2">
          <span className="text-ink/60">€</span>
          <input
            type="number"
            min={0}
            value={spesaReale}
            onChange={(e) => setSpesaReale(e.target.value)}
            placeholder="0"
            className="min-h-11 w-24 rounded-[10px] bg-paper px-3.5 font-mono text-sm text-ink focus:outline-none focus:ring-2 focus:ring-accent/40"
          />
        </div>
        <div className="mt-3">
          <SupermercatoSelector value={retailer} onChange={setRetailer} />
        </div>

        <div className="mt-4 border-t border-ink/10 pt-3.5">
          <label className="flex min-h-11 w-fit cursor-pointer items-center gap-2 rounded-full bg-paper px-4 text-xs font-semibold text-accent">
            {scontrinoElaborando && <Spinner className="h-3.5 w-3.5" />}
            {scontrinoElaborando ? "Leggo lo scontrino..." : "Fotografa lo scontrino"}
            <input
              type="file"
              accept="image/*"
              capture="environment"
              onChange={handleScontrino}
              disabled={scontrinoElaborando}
              className="hidden"
            />
          </label>
          <p className="mt-1.5 text-[11px] text-ink/50">
            Confrontiamo prodotti e prezzi con la lista della spesa di questa settimana — niente viene salvato,
            solo il totale precompila il campo sopra.
          </p>

          {erroreScontrino && <p className="mt-2 text-xs text-clay">{erroreScontrino}</p>}

          {confrontoScontrino && (
            <div className="mt-3 flex flex-col gap-1.5">
              {confrontoScontrino.corrispondenze.map((c) => (
                <div key={c.nome_lista} className="flex items-center justify-between gap-2 text-xs">
                  <span className="text-ink">{c.nome_lista}</span>
                  {c.trovato_sullo_scontrino ? (
                    <span className="font-mono text-ink/70">
                      €{c.prezzo_scontrino_eur?.toFixed(2)}
                      <span className="ml-1 text-ink/40">(stima €{c.prezzo_stimato_eur.toFixed(2)})</span>
                    </span>
                  ) : (
                    <span className="text-honey">non trovato</span>
                  )}
                </div>
              ))}
              {confrontoScontrino.extra_non_in_lista.length > 0 && (
                <div className="mt-1.5 border-t border-ink/10 pt-1.5 text-xs text-ink/60">
                  Extra non in lista:{" "}
                  {confrontoScontrino.extra_non_in_lista
                    .map((p) => `${p.nome} (€${p.prezzo_eur.toFixed(2)})`)
                    .join(", ")}
                </div>
              )}
            </div>
          )}
        </div>
      </div>

      {error && <p className="text-sm text-clay">{error}</p>}

      <button
        onClick={handleSubmit}
        disabled={!puoInviare || submitting}
        className="flex min-h-11 items-center justify-center gap-2 rounded-xl bg-accent py-3.5 text-[15px] font-semibold text-accent-fill-text disabled:opacity-40"
      >
        {submitting && <Spinner className="h-4 w-4" />}
        {submitting ? "Invio..." : "Conferma"}
      </button>
    </div>
  );
}
