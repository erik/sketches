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
    gearMapping: await store.getGearMapping(athleteId),
    availableGear: await strava.getGear(token),
  };
}

export async function action({ request }) {
  const session = await getSession(request.headers.get("Cookie"));
  const athleteId = getAthleteOrLogin(session);

  const form = await request.formData();

  const activityMapping = { };
  const modifierMapping = { trainer: null, commute: null };

  for (const [k, v] of form) {
    if (v === '') {
      continue;
    } else if (k in modifierMapping) {
      modifierMapping[k] = v;
    } else {
      activityMapping[k] = v;
    }
  }

  const gearMapping = {
    activityMapping,
    modifierMapping,
  };

  await store.setGearMapping(athleteId, gearMapping);

  return {
    gearMapping,
    updatedAt: new Date(),
  }
}

function gearInput(type, availableGear, currentMapping) {
  return (
    <div key={type}>
      <label htmlFor={`mapping-${type}`}>{type}</label>
      <div>
        <select
          name={type}
          defaultValue={currentMapping}
          id={`mapping-${type}`}>
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
    gearMapping,
  } = useLoaderData();

  const submit = useSubmit();
  const transition = useTransition();
  const { updatedAt = null } = useActionData() || {};


  function saveChange(event) {
    submit(event.currentTarget, { replace: true });
  }

  const bikeInputs = strava.BIKE_ACTIVITY_TYPES.map(t => {
    return gearInput(t, availableGear.bikes, gearMapping?.activityMapping[t]);
  });

  const bikeModifierInputs = strava.BIKE_ACTIVITY_MODIFIERS.map(m => {
    return gearInput(m, availableGear.bikes, gearMapping?.modifierMapping[m]);
  });

  const shoeInputs = strava.SHOE_ACTIVITY_TYPES.map(t => {
    return gearInput(t, availableGear.shoes, gearMapping?.activityMapping[t]);
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
        <h2>Bike Activities</h2>
        {bikeInputs}

        <h3>Bike Activity Modifiers</h3>
        <em>Takes precedence.</em>
        {bikeModifierInputs}

        <h2>Shoe Activities</h2>
        {shoeInputs}
      </Form>

      <p>
        {saveIndicator}
      </p>
    </div>
  );
}
