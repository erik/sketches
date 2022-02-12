import { redirect } from "remix";
import type { LoaderFunction } from "remix";

import * as store from "~/storage.server";
import { getSession, commitSession } from "~/sessions.server";
import * as strava from "~/util/strava";

export const loader: LoaderFunction = async ({
  request
}) => {
  const params = new URL(request.url).searchParams;
  const code = params.get('code');
  const grantedScopes = params.get('scope').split(',');

  const {
    athleteId,
    token,
  } = await strava.exchangeCodeForToken(code, grantedScopes);

  await store.setStravaCredentials(athleteId, token);

  const session = await getSession(request.headers.get("Cookie"));
  session.set("athlete_id", athleteId);

  return redirect("/", {
    headers: {
      "Set-Cookie": await commitSession(session)
    }
  });
};
