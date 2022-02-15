import { redirect, useLoaderData } from "remix";

import { getSession, commitSession, getAthleteOrLogin } from "~/sessions.server";
import * as strava from "~/util/strava";

export async function loader({ request }) {
  const session = await getSession(request.headers.get("Cookie"));

  if (session.has("athlete_id")) {
    throw redirect("/configure", 301);
  }

  return {
    authUrl: strava.getLoginURL(`${request.url}auth/exchange_token`)
  };
}

export default function Index(request) {
  const { authUrl } = useLoaderData();

  return (
    <div>
      <h1>Strava Gear Tagger</h1>

      <a href={authUrl}>
        Login With Strava
      </a>
    </div>
  );
}
