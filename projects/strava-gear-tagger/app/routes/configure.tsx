import {
  redirect,
  useActionData,
  useLoaderData,
  useTransition,
} from "remix";

import { getSession, commitSession } from "~/sessions.server";
import { getGearMapping, setGearMapping } from "~/storage.server";
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

  const tok = JSON.parse(session.get('token'));

  return {
    gear: await strava.getGear(session),
    gearMapping: await getGearMapping(tok.athleteId),
    session,
  };
}

export async function action({ request }) {
  const session = await getSession(
    request.headers.get("Cookie")
  );

  const tok = JSON.parse(session.get('token'));

  const form = await request.formData();
  const activityMap = {};
  for (const [k, v] of form) {
    v !== '' && (activityMap[k] = v);
  }

  console.log('got', activityMap);
  await setGearMapping(tok.athleteId, activityMap);

  return redirect('/configure');
}

function mapGearSelections(activityTypes, gear, currentMapping) {
  const mapping = activityTypes.map(t => (
    <div key={t}>
      <label
        htmlFor={`mapping-${t}`}>
        {t}
      </label>
      <div>
        <select
          name={t}
          defaultValue={currentMapping[t] || ""}
          id={`mapping-${t}`}>
          <option value="">---</option>
          { gear.map(it => <option key={it.id} value={it.id}>{it.name}</option>) }
        </select>
      </div>
    </div>
  ));

  return <div>{mapping}</div>;
}

export default function Configure() {
  const { gear, gearMapping } = useLoaderData();
  const transition = useTransition();
  const actionData = useActionData();

  // TODO: use Form (capital F)
  return (
    <div style={{ fontFamily: "system-ui, sans-serif", lineHeight: "1.4" }}>
      <h1>Configuration</h1>

      <pre>{JSON.stringify(gearMapping)}</pre>

      <form method="post">
        <h2>Rides</h2>
        {mapGearSelections(strava.BIKE_ACTIVITY_TYPES, gear.bikes, gearMapping)}

        <h2>Runs</h2>
        {mapGearSelections(strava.SHOE_ACTIVITY_TYPES, gear.shoes, gearMapping)}

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
