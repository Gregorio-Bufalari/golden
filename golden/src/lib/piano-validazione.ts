import "server-only";
import { regeneratePasto, type Pasto, type Giorno, type ProfiloPerPiano } from "./claude";
import { ingredientiARischio } from "./glutine-check";

const MAX_RIGENERAZIONI = 2;

export type PastoValidato = Pasto & {
  verificare?: boolean;
  ingredienti_a_rischio?: string[];
};

export type GiornoValidato = {
  giorno: string;
  pasti: PastoValidato[];
};

export async function validaGiorni(
  profilo: ProfiloPerPiano,
  giorni: Giorno[],
): Promise<GiornoValidato[]> {
  const richiedeControlloGlutine = profilo.restrizioni?.includes("Glutine (celiachia)");
  const giorniValidati: GiornoValidato[] = [];

  for (const giorno of giorni) {
    const pastiValidati: PastoValidato[] = [];

    for (const pasto of giorno.pasti) {
      let pastoCorrente: PastoValidato = pasto;

      if (richiedeControlloGlutine) {
        let rischi = ingredientiARischio(pastoCorrente.ingredienti.map((i) => i.nome));
        let tentativi = 0;

        while (rischi.length > 0 && tentativi < MAX_RIGENERAZIONI) {
          tentativi += 1;
          try {
            pastoCorrente = await regeneratePasto(profilo, giorno.giorno, pastoCorrente, rischi);
            rischi = ingredientiARischio(pastoCorrente.ingredienti.map((i) => i.nome));
          } catch (err) {
            console.error("regeneratePasto error:", err);
            break;
          }
        }

        if (rischi.length > 0) {
          pastoCorrente = { ...pastoCorrente, verificare: true, ingredienti_a_rischio: rischi };
        }
      }

      pastiValidati.push(pastoCorrente);
    }

    giorniValidati.push({ giorno: giorno.giorno, pasti: pastiValidati });
  }

  return giorniValidati;
}
