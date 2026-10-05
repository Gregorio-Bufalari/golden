import { notFound } from "next/navigation";
import { createAdminClient } from "@/lib/supabase/admin";
import { BottomNav } from "./bottom-nav";

export default async function PianoLayout({
  children,
  params,
}: {
  children: React.ReactNode;
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  const supabase = createAdminClient();

  const { data: profile } = await supabase
    .from("profiles")
    .select("id")
    .eq("link_token", token)
    .single();

  if (!profile) {
    notFound();
  }

  return (
    <div className="flex min-h-full w-full flex-1 flex-col bg-paper">
      <div className="flex flex-1 flex-col overflow-y-auto">{children}</div>
      <BottomNav token={token} />
    </div>
  );
}
