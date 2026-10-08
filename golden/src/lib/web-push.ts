import "server-only";
import webpush from "web-push";
import type { SupabaseClient } from "@supabase/supabase-js";

// Richiede VAPID_PUBLIC_KEY/VAPID_PRIVATE_KEY in env (vedi .env.example):
// coppia di chiavi generata una tantum con `npx web-push generate-vapid-keys`,
// nessun account esterno necessario. VAPID_SUBJECT è il contatto mostrato ai
// servizi push in caso di abuso, non raggiunge mai l'utente finale.
let configurato = false;
function assicuraConfigurazione() {
  if (configurato) return;
  const publicKey = process.env.VAPID_PUBLIC_KEY;
  const privateKey = process.env.VAPID_PRIVATE_KEY;
  if (!publicKey || !privateKey) {
    throw new Error("VAPID_PUBLIC_KEY/VAPID_PRIVATE_KEY non configurate.");
  }
  webpush.setVapidDetails(process.env.VAPID_SUBJECT || "mailto:notifiche@groci.app", publicKey, privateKey);
  configurato = true;
}

export type SottoscrizionePush = {
  id: string;
  endpoint: string;
  p256dh: string;
  auth_key: string;
};

/**
 * Invia una notifica push a una sottoscrizione. Se il servizio push
 * risponde che l'endpoint non è più valido (404/410 — l'utente ha
 * disinstallato l'app, revocato il permesso, o il browser ha scaduto la
 * sottoscrizione), la rimuove da subito invece di ritentare all'infinito
 * ad ogni controllo successivo.
 */
export async function inviaNotificaPush(
  supabase: SupabaseClient,
  sottoscrizione: SottoscrizionePush,
  payload: { titolo: string; corpo: string; url: string },
): Promise<void> {
  assicuraConfigurazione();

  try {
    await webpush.sendNotification(
      {
        endpoint: sottoscrizione.endpoint,
        keys: { p256dh: sottoscrizione.p256dh, auth: sottoscrizione.auth_key },
      },
      JSON.stringify(payload),
    );
  } catch (err) {
    const statusCode = (err as { statusCode?: number }).statusCode;
    if (statusCode === 404 || statusCode === 410) {
      await supabase.from("push_subscriptions").delete().eq("id", sottoscrizione.id);
    } else {
      console.error("inviaNotificaPush error:", err);
    }
  }
}
