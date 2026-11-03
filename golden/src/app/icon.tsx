import { ImageResponse } from "next/og";
import { appIconElement, appIconFonts } from "@/lib/app-icon";

export const size = { width: 32, height: 32 };
export const contentType = "image/png";

export default async function Icon() {
  return new ImageResponse(appIconElement(size.width), { ...size, fonts: await appIconFonts() });
}
