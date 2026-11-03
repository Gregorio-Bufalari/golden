import { ImageResponse } from "next/og";
import { appIconElement, appIconFonts } from "@/lib/app-icon";

// URL stabile (non il convention file `icon.tsx`, che genera un path con
// hash) — referenziata direttamente in manifest.ts come icona 192x192 per
// l'installazione PWA su home screen.
export async function GET() {
  const size = 192;
  return new ImageResponse(appIconElement(size), { width: size, height: size, fonts: await appIconFonts() });
}
