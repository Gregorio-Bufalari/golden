import { redirect } from "next/navigation";

export default async function PianoPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;
  redirect(`/piano/${token}/menu`);
}
