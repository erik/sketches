import { redirect } from "remix";
import type { LoaderFunction } from "remix";

import { getSession, commitSession } from "~/sessions.server";
import * as strava from "~/util/strava";

export const loader: LoaderFunction = async ({
  request
}) => {
  const code = new URL(request.url).searchParams.get('code');
  // TODO: handle failure
  const token = await strava.exchangeCodeForToken(code);

  const session = await getSession(
    request.headers.get("Cookie")
  );

  session.set("token", JSON.stringify(token));
  const cookie = await commitSession(session);

  return redirect("/", {
    headers: {"Set-Cookie": cookie}
  });
};
