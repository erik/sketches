import type {
  ActionFunction,
  LoaderFunction,
} from "remix";
import { json } from "remix";

import { getSession, commitSession } from "~/sessions.server";
import * as store from "~/storage.server";
import * as strava from "~/util/strava";

// Confirm webhook subscription
export const loader: LoaderFunction = async ({
  request
}) => {
  const params = new URL(request.url).searchParams;
  const validated = strava.validateWebhookPayload(params);

  if (params.get('hub.mode') !== 'subscribe' || !validated) {
    console.log('Ignoring bogus Strava request', params);
    return json({ msg: 'Webhook not accepted' }, { status: 401 });
  }

  return json({
    'hub.challenge': params.get('hub.challenge')
  });
};

export const action: ActionFunction = async ({
  request
}) => {
  if (request.method !== "POST") {
    throw json("Method Not Allowed", { status: 405 });
  }

  console.log('Received webhook request', request);

  const payload = await request.json();
  return await handleWebhookPayload(payload);
};


// Strava doesn't (I think?) give us any way to verify the authenticity of the webhook payload.
// TODO: Maybe register the webhook with a ?strava-token=FOO qs param?
async function handleWebhookPayload(payload) {
  console.log("Received webhook payload", payload);

  if (payload['aspect_type'] !== 'create' || payload['object_type'] !== 'activity') {
    console.log('Ignoring unhandled webhook payload type', payload);
    throw json({msg: 'ignored.'}, { status: 200 });
  }

  const athleteId = payload['owner_id'];
  const token = await store.getStravaCredentials(athleteId);

  if (token == null) {
    console.error("No credentials available for athleteId=", athleteId);
    throw json({msg: 'ignored.'}, { status: 200 });
  }

  const gearMapping = await store.getGearMapping(athleteId);
  if (gearMapping == null) {
    console.error("No gear mapping available for athleteId=", athleteId);
    throw json({msg: 'ignored.'}, { status: 200 });
  }

  const activityId = payload['object_id'];
  const activity = await strava.getActivityDetails(token, activityId);

  if (activity.gear_id !== null) {
    console.log('Activity already has associated gear, skipping');
    throw json({msg: 'ignored.'}, { status: 200 });
  }

  const mappedGearId = gearMapping[activity.type];
  if (mappedGearId == null) {
    console.log('No gear mapped for type=', activity.type);
    throw json({msg: 'ignored.'}, { status: 200 });
  }

  console.log('Okay, updating activityId=', activityId, 'to gearId=', mappedGearId);
  await strava.updateActivityGear(token, activityId, mappedGearId);

  return json({msg: 'updated'});
}
