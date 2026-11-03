import { ImageResponse } from "next/og";
import { appIconElement, appIconFonts } from "@/lib/app-icon";

// URL stabile, vedi commento in ../icon-192/route.tsx — icona 512x512 per
// l'installazione PWA (manifest.ts).
export async function GET() {
  const size = 512;
  return new ImageResponse(appIconElement(size), { width: size, height: size, fonts: await appIconFonts() });
}
