import { ImageResponse } from "next/og";
import { appIconElement, appIconFonts } from "@/lib/app-icon";

export const size = { width: 180, height: 180 };
export const contentType = "image/png";

export default async function AppleIcon() {
  return new ImageResponse(appIconElement(size.width), { ...size, fonts: await appIconFonts() });
}
