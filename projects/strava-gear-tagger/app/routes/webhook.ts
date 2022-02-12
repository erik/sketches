import type { LoaderFunction } from "remix";

import { getSession, commitSession } from "~/sessions.server";
import * as strava from "~/util/strava";

// Confirm webhook subscription
export const loader: LoaderFunction = async ({
  request
}) => {
  const params = new URL(request.url).searchParams;
  const validated = strava.validateWebhookPayload(params);

  if (params.get('hub.mode') !== 'subscribe' || !validated) {
    // TODO: 4xx
    console.log('Ignoring bogus Strava request', params);
    return 'bah!';
  }

  // TODO: Your callback address must respond within two seconds to
  // the GET request from Strava’s subscription service. The response
  // should indicate status code 200 and should echo the hub.challenge
  // field in the response body as application/json content type: {
  // “hub.challenge”:”15f7d1a91c1f40f8a748fd134752feb3” }
  return params.get('hub.challenge');
};


// TODO: webhook post
