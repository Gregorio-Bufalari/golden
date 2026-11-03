// Wordmark "groci" — sempre minuscolo (mai "Groci"/"GROCI"), Fredoka 500,
// colore dark (navy). Letter-spacing molto stretto (~-9.7% del font-size,
// vedi design system) per via delle forme tondeggianti di Fredoka.
export function Logo({ size = 24 }: { size?: number }) {
  return (
    <span
      className="font-fredoka inline-block text-dark"
      style={{ fontSize: size, lineHeight: 1, letterSpacing: `${size * -0.097}px`, fontWeight: 500 }}
    >
      groci
    </span>
  );
}
