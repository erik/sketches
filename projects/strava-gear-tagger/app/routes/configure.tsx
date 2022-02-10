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

  if (!session.has("token")) {
    return redirect("/");
  }

  const token = JSON.parse(session.get('token'));
  const gear = await strava.getGear(token);

  return {
    gear,
    session,
  };
}

export async function action({ request }) {
  const body = await request.formData();

  console.log('got', body)

  return redirect('/configure');
}

function mapGearSelections(activityTypes, gear) {
  const mapping = activityTypes.map(t => (
    <div key={t}>
      <label
        htmlFor={`grid-${t}`}>
        {t}
      </label>
      <div>
        <select
          name={t}
          id={`grid-${t}`}>
          <option value="">---</option>
          { gear.map(it => <option key={it.id} value={it.id}>{it.name}</option>) }
        </select>
      </div>
    </div>
  ));

  return <div>{mapping}</div>;
}

export default function Configure() {
  const { gear } = useLoaderData();

  return (
    <div style={{ fontFamily: "system-ui, sans-serif", lineHeight: "1.4" }}>
      <h1>Configuration</h1>

      <form method="post">
        <h2>Rides</h2>
        {mapGearSelections(strava.BIKE_ACTIVITY_TYPES, gear.bikes)}

        <h2>Runs</h2>
        {mapGearSelections(strava.SHOE_ACTIVITY_TYPES, gear.shoes)}

        <button type="submit">
          Configure
        </button>
      </form>

      <h2>Debug</h2>
      <details>
        <pre>{JSON.stringify(gear, null, 2)}</pre>
      </details>
    </div>
  );
}
