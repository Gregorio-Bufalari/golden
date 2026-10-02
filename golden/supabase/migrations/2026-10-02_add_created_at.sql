-- Aggiunge created_at a profiles, weekly_plans, checkins.
-- Serve a misurare il North Star metric (doc di prodotto, sezione 14):
-- % di settimane in cui l'utente richiede/riusa il piano senza essere
-- sollecitato. Esegui nel SQL Editor di Supabase sul database esistente
-- (schema.sql è già aggiornato per i progetti nuovi).

alter table profiles add column if not exists created_at timestamptz not null default now();
alter table weekly_plans add column if not exists created_at timestamptz not null default now();
alter table checkins add column if not exists created_at timestamptz not null default now();
