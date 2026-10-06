export function HeartIcon({ pieno }: { pieno: boolean }) {
  return (
    <svg
      width="20"
      height="20"
      viewBox="0 0 24 24"
      fill={pieno ? "currentColor" : "none"}
      stroke="currentColor"
      strokeWidth="1.8"
      strokeLinecap="round"
      strokeLinejoin="round"
    >
      <path d="M12 20.5s-7.5-4.5-9.5-9A5 5 0 0 1 12 6a5 5 0 0 1 9.5 5.5c-2 4.5-9.5 9-9.5 9Z" />
    </svg>
  );
}
