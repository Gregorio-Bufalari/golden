import { defineConfig } from "vitest/config";
import path from "node:path";

const here = import.meta.dirname;

export default defineConfig({
  resolve: {
    alias: {
      "@": path.resolve(here, "./src"),
      "server-only": path.resolve(here, "./src/test/stubs/server-only.ts"),
    },
  },
  test: {
    environment: "node",
    include: ["src/**/*.test.ts"],
    exclude: ["src/**/*.smoke.test.ts", "node_modules/**"],
  },
});
