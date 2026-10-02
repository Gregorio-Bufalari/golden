-- Salva, per ogni weekly_plan, quali consumi dalla dispensa ha comportato
-- (oltre a grocery_list.rimasto, già presente). Serve per poter "annullare e
-- rifare" correttamente l'effetto sulla dispensa quando un piano viene
-- modificato più volte nella stessa settimana (vedi src/lib/dispensa.ts).
-- Esegui nel SQL Editor di Supabase sul database esistente.

alter table weekly_plans add column if not exists consumi_dispensa jsonb;
