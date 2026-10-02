import { CheckinForm } from "../checkin-form";

export default async function CheckinPage({
  params,
}: {
  params: Promise<{ token: string }>;
}) {
  const { token } = await params;

  return (
    <div className="flex flex-1 flex-col items-center px-6 py-10">
      <h1 className="text-2xl font-semibold text-zinc-950 dark:text-zinc-50">
        Check-in della settimana
      </h1>
      <p className="mt-2 max-w-md text-center text-sm text-zinc-500 dark:text-zinc-400">
        Due minuti, non un inventario — aiuta a migliorare i prossimi piani.
      </p>

      <div className="mt-6 w-full max-w-md">
        <CheckinForm token={token} />
      </div>
    </div>
  );
}
