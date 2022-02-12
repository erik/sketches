import type {
  json,
  ActionFunction,
  LoaderFunction,
} from "remix";

import { getSession, commitSession } from "~/sessions.server";
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

  const payload = await request.json();
  return await handleWebhookPayload(payload);
};


async function handleWebhookPayload(payload) {
  console.log("Received webhook payload", payload);

  const athleteId = payload['owner_id'];
  // TODO: fetch credentials.
//   {
//     "aspect_type": "update",
//     "event_time": 1516126040,
//     "object_id": 1360128428,
//     "object_type": "activity",
//     "owner_id": 134815,
//     "subscription_id": 120475,
//     "updates": {
//         "title": "Messy"
//     }
// }
}
