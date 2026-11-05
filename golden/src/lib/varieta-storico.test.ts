import { describe, it, expect } from "vitest";
import { nomiPastiRecenti } from "./varieta-storico";
import type { SupabaseClient } from "@supabase/supabase-js";

// Client Supabase finto che imita la catena usata da nomiPastiRecenti:
// .from().select().eq().order().order().limit(), risolta con .then().
function fakeSupabase(righe: { meal_plan: { giorni: unknown[] } }[]) {
  const builder = {
    select: () => builder,
    eq: () => builder,
    order: () => builder,
    limit: () => builder,
    then: (resolve: (v: { data: typeof righe }) => void) => resolve({ data: righe }),
  };
  return { from: () => builder } as unknown as SupabaseClient;
}

function giorno(pasti: { nome: string; ingredienti: unknown[] }[]) {
  return { pasti };
}

describe("nomiPastiRecenti", () => {
  it("raccoglie i nomi distinti dei pasti dalle settimane restituite", async () => {
    const supabase = fakeSupabase([
      {
        meal_plan: {
          giorni: [
            giorno([{ nome: "Pasta al pomodoro", ingredienti: [{}] }, { nome: "Pollo al forno", ingredienti: [{}] }]),
          ],
        },
      },
      {
        meal_plan: {
          giorni: [giorno([{ nome: "Riso al salmone", ingredienti: [{}] }])],
        },
      },
    ]);

    const nomi = await nomiPastiRecenti(supabase, "profilo-1");

    expect(nomi).toEqual(["Pasta al pomodoro", "Pollo al forno", "Riso al salmone"]);
  });

  it("deduplica un piatto che ricompare in più settimane", async () => {
    const supabase = fakeSupabase([
      { meal_plan: { giorni: [giorno([{ nome: "Pasta al pomodoro", ingredienti: [{}] }])] } },
      { meal_plan: { giorni: [giorno([{ nome: "Pasta al pomodoro", ingredienti: [{}] }])] } },
    ]);

    const nomi = await nomiPastiRecenti(supabase, "profilo-1");

    expect(nomi).toEqual(["Pasta al pomodoro"]);
  });

  it('esclude i pasti "mangio fuori" (ingredienti vuoti): non sono ricette vere', async () => {
    const supabase = fakeSupabase([
      {
        meal_plan: {
          giorni: [
            giorno([
              { nome: "Fuori casa — nessuna preparazione necessaria", ingredienti: [] },
              { nome: "Pollo al forno", ingredienti: [{}] },
            ]),
          ],
        },
      },
    ]);

    const nomi = await nomiPastiRecenti(supabase, "profilo-1");

    expect(nomi).toEqual(["Pollo al forno"]);
  });

  it("restituisce un array vuoto se il profilo non ha ancora piani salvati", async () => {
    const supabase = fakeSupabase([]);
    expect(await nomiPastiRecenti(supabase, "profilo-1")).toEqual([]);
  });
});
