import { readFile } from "node:fs/promises";
import { join } from "node:path";

// Icona app: tile arrotondata (squircle, raggio ~22% della dimensione) con
// sfondo icon-bg e il wordmark "groci" centrato in Fredoka 500, colore dark.
// Letto una sola volta a livello di modulo (non dipende dalla request) —
// vedi "Predictable values" nei doc di ImageResponse.
const fredokaMediumPath = join(process.cwd(), "src/assets/fonts/Fredoka-Medium.ttf");
let fredokaMediumPromise: Promise<Buffer> | null = null;

export function fredokaMedium(): Promise<Buffer> {
  if (!fredokaMediumPromise) fredokaMediumPromise = readFile(fredokaMediumPath);
  return fredokaMediumPromise;
}

const ICON_BG = "#d9e1f1";
const DARK = "#192338";

/**
 * JSX dell'icona app per satori/ImageResponse, per una tile di `size` px.
 * Proporzioni dal design system: font-size ~43% della tile, letter-spacing
 * ~-9.7% del font-size, raggio ~22% della tile. La "g" di Fredoka ha una
 * gambetta discendente che sposta otticamente il testo in basso: compensata
 * con una translateY verso l'alto di ~11% del font-size.
 */
export function appIconElement(size: number) {
  const fontSize = Math.round(size * 0.43);
  const letterSpacing = Math.round(fontSize * -0.097 * 10) / 10;
  const translateY = Math.round(fontSize * -0.11 * 10) / 10;
  const radius = Math.round(size * 0.22);

  return (
    <div
      style={{
        width: "100%",
        height: "100%",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        background: ICON_BG,
        borderRadius: radius,
      }}
    >
      <span
        style={{
          display: "block",
          fontFamily: "Fredoka",
          fontWeight: 500,
          fontSize,
          lineHeight: 1,
          color: DARK,
          letterSpacing,
          transform: `translateY(${translateY}px)`,
        }}
      >
        groci
      </span>
    </div>
  );
}

export async function appIconFonts() {
  return [{ name: "Fredoka", data: await fredokaMedium(), weight: 500 as const, style: "normal" as const }];
}
