-- Distingue, per ogni Preferito salvato, se il valore nutrizionale viene
-- dal dato ufficiale CREA/INRAN o da una stima AI (vedi
-- src/lib/nutrizione-crea.ts). Null per i preferiti salvati prima di
-- questa colonna: trattati come stima AI in UI, mai dato per scontato che
-- fossero CREA/INRAN.
-- Esegui nel SQL Editor di Supabase sul database esistente.

alter table preferiti add column if not exists fonte_nutrizione text
  check (fonte_nutrizione in ('crea_inran', 'stima_ai'));
