-- Aggiunge campi biometrici opzionali al profilo, usati solo per il
-- confronto nutrizionale LARN nella sezione Menu (vedi src/lib/larn.ts).
-- Esegui nel SQL Editor di Supabase sul database esistente.

alter table profiles add column if not exists sesso text check (sesso in ('M', 'F'));
alter table profiles add column if not exists eta integer;
alter table profiles add column if not exists peso_kg numeric(5, 2);
alter table profiles add column if not exists altezza_cm numeric(5, 1);
alter table profiles add column if not exists livello_attivita text check (livello_attivita in ('sedentario', 'moderato', 'attivo'));
