import { NextResponse } from "next/server";
import { createAdminClient } from "@/lib/supabase/admin";
import { confrontaScontrino, type ArticoloListaSpesa } from "@/lib/claude";
import type { GroceryList } from "@/lib/grocery";

export const maxDuration = 60;

const MEDIA_TYPE_VALIDI = ["image/jpeg", "image/png", "image/webp"] as const;
type MediaTypeValido = (typeof MEDIA_TYPE_VALIDI)[number];

// Limite generoso ma non illimitato sulla stringa base64 (~6MB di immagine
// reale): il client comprime già la foto prima di inviarla (vedi
// checkin-form.tsx), questo è solo un tetto di sicurezza lato server.
const BASE64_MAX_LENGTH = 8_000_000;

export async function POST(request: Request) {
  const { token, immagine_base64, media_type } = await request.json();

  if (!token || typeof token !== "string") {
    return NextResponse.json({ error: "Token mancante." }, { status: 400 });
  }
  if (!immagine_base64 || typeof immagine_base64 !== "string") {
    return NextResponse.json({ error: "Immagine mancante." }, { status: 400 });
  }
  if (immagine_base64.length > BASE64_MAX_LENGTH) {
    return NextResponse.json({ error: "Immagine troppo grande." }, { status: 400 });
  }
  if (!MEDIA_TYPE_VALIDI.includes(media_type)) {
    return NextResponse.json({ error: "Formato immagine non supportato." }, { status: 400 });
  }

  const supabase = createAdminClient();

  const { data: profile } = await supabase.from("profiles").select("id").eq("link_token", token).single();
  if (!profile) {
    return NextResponse.json({ error: "Profilo non trovato." }, { status: 404 });
  }

  const { data: pianoAttuale } = await supabase
    .from("weekly_plans")
    .select("grocery_list")
    .eq("profile_id", profile.id)
    .order("settimana", { ascending: false })
    .order("created_at", { ascending: false })
    .limit(1)
    .maybeSingle();

  const groceryList = pianoAttuale?.grocery_list as GroceryList | null;
  if (!groceryList) {
    return NextResponse.json(
      { error: "Nessuna lista della spesa con cui confrontare lo scontrino: genera un piano prima." },
      { status: 400 },
    );
  }

  const listaSpesa: ArticoloListaSpesa[] = groceryList.reparti.flatMap((r) =>
    r.items.map((i) => ({ nome: i.nome, prezzo_stimato_eur: i.prezzo_stimato })),
  );

  try {
    const risultato = await confrontaScontrino(immagine_base64, media_type as MediaTypeValido, listaSpesa);

    if (!risultato.leggibile) {
      return NextResponse.json({
        leggibile: false,
        totale_scontrino_eur: null,
        corrispondenze: [],
        extra_non_in_lista: [],
      });
    }

    return NextResponse.json({
      leggibile: true,
      totale_scontrino_eur: risultato.totale_scontrino_eur,
      corrispondenze: risultato.corrispondenze,
      extra_non_in_lista: risultato.extra_non_in_lista,
    });
  } catch (err) {
    console.error("confrontaScontrino error:", err);
    return NextResponse.json(
      { error: "Non sono riuscito a leggere lo scontrino. Riprova con una foto più chiara." },
      { status: 502 },
    );
  }
}
