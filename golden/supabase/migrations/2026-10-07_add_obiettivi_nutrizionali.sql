-- Obiettivi nutrizionali per singolo pasto (Profilo): calorie min/max,
-- proteine minime, carboidrati/grassi massimi per pasto. Usati dal motore
-- come vincolo aggiuntivo, stessa priorità dell'obiettivo generale, sempre
-- sotto restrizioni alimentari e validazione di sicurezza — vedi
-- buildContestoProfilo in src/lib/claude.ts. Diversi dal confronto LARN
-- (automatico, dai dati biometrici): qui l'utente imposta un target
-- esplicito, usato per generare il piano.
-- Esegui nel SQL Editor di Supabase sul database esistente.

alter table profiles add column if not exists obiettivi_nutrizionali jsonb not null default '{}'::jsonb;
