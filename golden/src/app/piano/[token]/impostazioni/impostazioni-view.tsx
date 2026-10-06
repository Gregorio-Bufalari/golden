"use client";

import { useState } from "react";
import { ImpostazioniForm, type ProfileData } from "./impostazioni-form";
import { RiepilogoProfilo } from "./riepilogo-profilo";

export function ImpostazioniView({ token, profile }: { token: string; profile: ProfileData }) {
  const [modalita, setModalita] = useState<"riepilogo" | "modifica">("riepilogo");
  const [datiProfilo, setDatiProfilo] = useState(profile);

  if (modalita === "modifica") {
    return (
      <ImpostazioniForm
        token={token}
        profile={datiProfilo}
        onAnnulla={() => setModalita("riepilogo")}
        onSalvato={(nuovoProfilo) => {
          setDatiProfilo(nuovoProfilo);
          setModalita("riepilogo");
        }}
      />
    );
  }

  return <RiepilogoProfilo profile={datiProfilo} onModifica={() => setModalita("modifica")} />;
}
