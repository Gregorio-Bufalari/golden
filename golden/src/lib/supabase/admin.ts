import "server-only";
import { createClient } from "@supabase/supabase-js";

// Usa la service role key: bypassa RLS. Non importare mai questo file
// da un Client Component o esporre SUPABASE_SERVICE_ROLE_KEY al browser.
export function createAdminClient() {
  return createClient(
    process.env.NEXT_PUBLIC_SUPABASE_URL!,
    process.env.SUPABASE_SERVICE_ROLE_KEY!,
    {
      auth: {
        autoRefreshToken: false,
        persistSession: false,
      },
    },
  );
}
