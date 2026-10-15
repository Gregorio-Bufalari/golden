// Logo testuale "Groci" in Caveat Brush — unico punto che usa questo
// font, leggermente ruotato per un tocco scritto a mano. Colore ink
// ovunque (anche nella versione grande di Home/Onboarding: mai terracotta).
export function Logo({ size = 24 }: { size?: number }) {
  return (
    <span
      className="font-caveat-brush inline-block text-ink"
      style={{ fontSize: size, lineHeight: 1, transform: "rotate(-3deg)" }}
    >
      Groci
    </span>
  );
}
