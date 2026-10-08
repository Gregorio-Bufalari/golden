"use client";

import { useEffect, useState } from "react";
import { salvaPushSubscription } from "../push-actions";

// Converte la chiave pubblica VAPID (base64url) nel formato richiesto da
// PushManager.subscribe — boilerplate standard per le notifiche push web,
// non specifico di questa app.
function urlBase64ToUint8Array(base64: string): Uint8Array<ArrayBuffer> {
  const padding = "=".repeat((4 - (base64.length % 4)) % 4);
  const base64Safe = (base64 + padding).replace(/-/g, "+").replace(/_/g, "/");
  const raw = atob(base64Safe);
  const array = new Uint8Array(new ArrayBuffer(raw.length));
  for (let i = 0; i < raw.length; i++) array[i] = raw.charCodeAt(i);
  return array;
}

function supportoDisponibile(): boolean {
  return (
    typeof window !== "undefined" &&
    "serviceWorker" in navigator &&
    "PushManager" in window &&
    "Notification" in window
  );
}

/**
 * Propone di attivare le notifiche push SOLO quando c'è già qualcosa di
 * utile da notificare (il chiamante la monta solo in quel caso — vedi
 * frigo/page.tsx) e solo se il permesso non è già stato chiesto prima
 * (`Notification.permission === "default"`): mai un prompt proattivo al
 * primo avvio dell'app, mai ripetuto se l'utente ha già detto sì o no.
 */
export function NotifichePush({ token }: { token: string }) {
  const [stato, setStato] = useState<"nascosto" | "proponi" | "attivando" | "attivo" | "errore">("nascosto");

  useEffect(() => {
    if (!supportoDisponibile()) return;
    if (Notification.permission === "denied") return;

    if (Notification.permission === "granted") {
      // Il browser ricorda già il consenso su questo dispositivo (es. dato
      // in una sessione precedente): nessun bisogno di richiederlo di
      // nuovo, basta assicurarsi che la sottoscrizione sia salvata.
      void attiva();
      return;
    }

    // eslint-disable-next-line react-hooks/set-state-in-effect -- lettura one-shot di Notification.permission dopo il mount, non un mirror di props/state
    setStato("proponi");
    // eslint-disable-next-line react-hooks/exhaustive-deps -- va eseguito solo al mount, non ad ogni render
  }, []);

  async function attiva() {
    setStato("attivando");
    try {
      const registration = await navigator.serviceWorker.register("/sw.js");
      await navigator.serviceWorker.ready;

      const vapidPublicKey = process.env.NEXT_PUBLIC_VAPID_PUBLIC_KEY;
      if (!vapidPublicKey) {
        setStato("errore");
        return;
      }

      const subscription = await registration.pushManager.subscribe({
        userVisibleOnly: true,
        applicationServerKey: urlBase64ToUint8Array(vapidPublicKey),
      });

      const result = await salvaPushSubscription(token, subscription.toJSON() as { endpoint: string; keys: { p256dh: string; auth: string } });
      setStato("error" in result ? "errore" : "attivo");
    } catch {
      setStato("errore");
    }
  }

  async function handleAttiva() {
    const permesso = await Notification.requestPermission();
    if (permesso !== "granted") {
      setStato("nascosto");
      return;
    }
    await attiva();
  }

  if (stato === "nascosto" || stato === "errore") return null;

  if (stato === "attivo") {
    return (
      <div className="bg-panel px-4 py-3 text-[13px] text-ink">
        Notifiche attive: ti avviseremo prima che qualcosa scada, anche senza aprire l&apos;app.
      </div>
    );
  }

  return (
    <div className="flex items-center justify-between gap-3 bg-panel px-4 py-3">
      <p className="text-[13px] text-ink">
        Vuoi essere avvertito prima che qualcosa scada, anche senza aprire l&apos;app?
      </p>
      <span className="flex shrink-0 items-center gap-2">
        <button
          type="button"
          onClick={() => setStato("nascosto")}
          disabled={stato === "attivando"}
          className="text-xs font-semibold text-ink/55 disabled:opacity-50"
        >
          Non ora
        </button>
        <button
          type="button"
          onClick={handleAttiva}
          disabled={stato === "attivando"}
          className="rounded-full bg-accent px-4 py-2 text-xs font-semibold text-accent-fill-text disabled:opacity-50"
        >
          {stato === "attivando" ? "Attivo..." : "Attiva notifiche"}
        </button>
      </span>
    </div>
  );
}
