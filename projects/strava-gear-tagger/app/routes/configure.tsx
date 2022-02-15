import {
  redirect,
  useActionData,
  useLoaderData,
  useSubmit,
  useTransition,
  Form,
} from "remix";

import { getSession, commitSession, getAthleteOrLogin } from "~/sessions.server";
import * as store from "~/storage.server";
import * as strava from "~/util/strava";

export async function loader({ request }) {
  const session = await getSession(request.headers.get("Cookie"));
  const athleteId = getAthleteOrLogin(session);
  const token = await store.getStravaCredentials(athleteId);

  return {
    athleteId,
    availableGear: await strava.getGear(token),
    activityGearMapping: await store.getGearMapping(athleteId),
  };
}

export async function action({ request }) {
  const session = await getSession(request.headers.get("Cookie"));
  const athleteId = getAthleteOrLogin(session);

  const form = await request.formData();
  const activityMap = {};
  for (const [k, v] of form) {
    v !== '' && (activityMap[k] = v);
  }

  await store.setGearMapping(athleteId, activityMap);

  return {
    updatedAt: new Date(),
  }
}

function activityTypeGearInput(activityType, availableGear, currentMapping) {
  return (
    <div key={activityType}>
      <label htmlFor={`mapping-${activityType}`}>{activityType}</label>
      <div>
        <select
          name={activityType}
          defaultValue={currentMapping}
          id={`mapping-${activityType}`}>
          <option value="">---</option>
          { availableGear.map(it => <option key={it.id} value={it.id}>{it.name}</option>) }
        </select>
      </div>
    </div>
  );
}

export default function Configure() {
  const {
    availableGear,
    activityGearMapping,
  } = useLoaderData();

  const submit = useSubmit();
  const transition = useTransition();
  const { updatedAt = null } = useActionData() || {};


  function saveChange(event) {
    submit(event.currentTarget, { replace: true });
  }

  const bikeInputs = strava.BIKE_ACTIVITY_TYPES.map(t => {
    return activityTypeGearInput(t, availableGear.bikes, activityGearMapping[t]);
  });

  const shoeInputs = strava.SHOE_ACTIVITY_TYPES.map(t => {
    return activityTypeGearInput(t, availableGear.shoes, activityGearMapping[t]);
  });

  // TODO: toLocaleString() does not work as expected with SSR
  const saveIndicator = (transition.state === 'submitting')
        ? <span>Saving...</span>
        : (updatedAt !== null)
        ? <span>Saved at <em>{updatedAt.toLocaleString()}</em></span>
        : null
        ;

  return (
    <div>
      <h1>Configuration</h1>

      <Form method="post" onChange={saveChange}>
        <h2>Rides</h2>
        {bikeInputs}

        <h2>Runs</h2>
        {shoeInputs}
      </Form>

      <p>
        {saveIndicator}
      </p>
    </div>
  );
}
