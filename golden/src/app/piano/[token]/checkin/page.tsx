import { CheckinForm } from "../checkin-form";
import { PageHeader } from "../page-header";

export default async function CheckinPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;

  return (
    <div className="flex flex-1 flex-col">
      <PageHeader token={token} title="Check-in" subtitle="Com'è andata questa settimana? Due minuti, non un inventario." />

      <div className="mx-auto w-full max-w-2xl flex-1 px-5 pb-10">
        <CheckinForm token={token} />
      </div>
    </div>
  );
}
