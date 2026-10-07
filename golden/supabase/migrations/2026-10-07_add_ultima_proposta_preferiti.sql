-- Rotazione dei Preferiti in modalità Scoperta (vedi
-- src/lib/preferiti-scoperta.ts): traccia quando un Preferito è stato
-- incluso l'ultima volta in un piano, così a rotazione nessuno si ripete
-- finché ce n'è un altro in attesa.
-- Esegui nel SQL Editor di Supabase sul database esistente.

alter table preferiti add column if not exists ultima_proposta timestamptz;
