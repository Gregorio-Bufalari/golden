import { defineConfig } from "vitest/config";
import path from "node:path";

const here = import.meta.dirname;

// Test "smoke": chiamano davvero l'API Anthropic (costano token e tempo).
// Si eseguono SOLO manualmente con `npm run test:smoke`, mai in automatico.
export default defineConfig({
  resolve: {
    alias: {
      "@": path.resolve(here, "./src"),
      "server-only": path.resolve(here, "./src/test/stubs/server-only.ts"),
    },
  },
  test: {
    environment: "node",
    include: ["src/**/*.smoke.test.ts"],
    testTimeout: 60_000,
  },
});
