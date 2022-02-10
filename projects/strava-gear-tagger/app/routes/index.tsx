import { redirect, useLoaderData } from "remix";

import { getSession, commitSession } from "~/sessions.server";
import * as strava from "~/util/strava";

function loginLink() {
  return strava.getLoginURL(
    'http://localhost:8787/auth/exchange_token'
  );
}

export async function loader({ request }) {
  const session = await getSession(
    request.headers.get("Cookie")
  );

  console.log('got session:', session.data)

  if (session.has("token")) {
    return redirect("configure");
  }

  return { session };
}

export default function Index(request) {
  const { session } = useLoaderData();

  return (
    <div style={{ fontFamily: "system-ui, sans-serif", lineHeight: "1.4" }}>
      <h1>Strava Gear Tagger</h1>

      <a href={loginLink()}>
        Login With Strava
      </a>

      <pre>
        { JSON.stringify(session) }
      </pre>
    </div>
  );
}
