import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { ingredientiInScadenzaDomani, testoNotificaScadenza } from "@/lib/notifiche-scadenza";
import { inviaNotificaPush } from "@/lib/web-push";

export const maxDuration = 60;

/**
 * Invocato una volta al giorno da Vercel Cron (vedi vercel.json): per ogni
 * profilo con almeno una notifica push attiva, controlla cosa scade
 * domani nel Frigo e, se c'è qualcosa, invia la notifica a tutti i suoi
 * dispositivi sottoscritti. Non tocca chi non ha mai attivato le
 * notifiche: per loro resta solo il banner in-app (vedi frigo/page.tsx).
 */
export async function GET(request: Request) {
  const cronSecret = process.env.CRON_SECRET;
  if (cronSecret) {
    const auth = request.headers.get("authorization");
    if (auth !== `Bearer ${cronSecret}`) {
      return NextResponse.json({ error: "Non autorizzato." }, { status: 401 });
    }
  }

  const supabase = createAdminClient();

  const { data: sottoscrizioni } = await supabase
    .from("push_subscriptions")
    .select("id, profile_id, endpoint, p256dh, auth_key, profiles(link_token)");

  if (!sottoscrizioni || sottoscrizioni.length === 0) {
    return NextResponse.json({ profili_notificati: 0, notifiche_inviate: 0 });
  }

  const sottoscrizioniPerProfilo = new Map<string, typeof sottoscrizioni>();
  for (const s of sottoscrizioni) {
    const gruppo = sottoscrizioniPerProfilo.get(s.profile_id) || [];
    gruppo.push(s);
    sottoscrizioniPerProfilo.set(s.profile_id, gruppo);
  }

  let profiliNotificati = 0;
  let notificheInviate = 0;

  for (const [profileId, sottoscrizioniProfilo] of sottoscrizioniPerProfilo) {
    const { data: rimanenze } = await supabase
      .from("rimanenze")
      .select("ingrediente, settimana")
      .eq("profile_id", profileId);

    const inScadenzaDomani = ingredientiInScadenzaDomani(rimanenze || []);
    if (inScadenzaDomani.length === 0) continue;

    const linkToken = (sottoscrizioniProfilo[0].profiles as unknown as { link_token: string } | null)?.link_token;
    if (!linkToken) continue;

    const { titolo, corpo } = testoNotificaScadenza(inScadenzaDomani);
    const payload = { titolo, corpo, url: `/piano/${linkToken}/frigo` };

    profiliNotificati += 1;
    for (const s of sottoscrizioniProfilo) {
      await inviaNotificaPush(supabase, { id: s.id, endpoint: s.endpoint, p256dh: s.p256dh, auth_key: s.auth_key }, payload);
      notificheInviate += 1;
    }
  }

  return NextResponse.json({ profili_notificati: profiliNotificati, notifiche_inviate: notificheInviate });
}
