import { redirect } from "remix";
import { getSession, commitSession } from "~/sessions.server";

type AuthToken = {
    accessToken: string;
    refreshToken: string;
    expiresAt: Date;
};

// Defined in .env
const CLIENT_ID = globalThis.STRAVA_CLIENT_ID;
const CLIENT_SECRET = globalThis.STRAVA_CLIENT_SECRET;
const WEBHOOK_TOKEN = globalThis.STRAVA_WEBHOOK_TOKEN;

const REQUIRED_SCOPES = [
    'profile:read_all',
    'activity:read_all',
    'activity:write',
];

export const BIKE_ACTIVITY_TYPES = [
    'EBikeRide',
    'Handcycle',
    'Ride',
    'VirtualRide',
];

export const SHOE_ACTIVITY_TYPES = [
    'Hike',
    'Run',
    'VirtualRun',
    'Walk',
];

// TODO: Check if these can have shoes attached.
const UNKNOWN_ACTIVITY_TYPES = [
    'AlpineSki',
    'BackcountrySki',
    'Canoeing',
    'Crossfit',
    'Elliptical',
    'Golf',
    'IceSkate',
    'InlineSkate',
    'Kayaking',
    'Kitesurf',
    'NordicSki',
    'RockClimbing',
    'RollerSki',
    'Rowing',
    'Sail',
    'Skateboard',
    'Snowboard',
    'Snowshoe',
    'Soccer',
    'StairStepper',
    'StandUpPaddling',
    'Surfing',
    'Swim',
    'Velomobile',
    'WeightTraining',
    'Wheelchair',
    'Windsurf',
    'Workout',
    'Yoga',
]

export function getLoginURL(redirectURL: string): string {
    return 'https://www.strava.com/oauth/authorize' +
        `?client_id=${CLIENT_ID}` +
        '&response_type=code' +
        `&redirect_uri=${redirectURL}` +
        '&approval_prompt=force' +
        `&scope=${REQUIRED_SCOPES.join(",")}`;
}

export async function exchangeCodeForToken(code: string, grantedScopes: List<string>) {
    if (REQUIRED_SCOPES.some(s => !grantedScopes.includes(s))) {
        console.error('Missing required scope', grantedScopes);
        // TODO: Flash error
        throw redirect("/", 301);
    }

    const response = await fetch('https://www.strava.com/oauth/token', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({
            code,
            client_id: CLIENT_ID,
            client_secret: CLIENT_SECRET,
            grant_type: 'authorization_code',
        }),
    });

    if (response.status !== 200) {
        console.error('Auth failed!', response.statusText)
        // TODO: Flash error
        throw redirect("/", 301);
    }

    const {
        access_token,
        refresh_token,
        expires_at,
        athlete: { id },
    } = await response.json();

    return {
        athleteId: id,
        token: {
            accessToken: access_token,
            refreshToken: refresh_token,
            expiresAt: new Date(expires_at * 1000),
        }
    };
}

export async function refreshToken(tok: AuthToken): AuthToken {
    console.log('Token needs refresh', tok);

    const response = await fetch('https://www.strava.com/api/v3/oauth/token', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({
            refresh_token: tok.refreshToken,
            client_id: CLIENT_ID,
            client_secret: CLIENT_SECRET,
            grant_type: 'refresh_token',
        }),
    });

    if (response.status !== 200) {
        console.error('Token refresh failed!', response);
        // TODO: log user out here.
        throw ':(';
    }

    const {
        access_token,
        refresh_token,
        expires_at,
    } = await response.json();

    return {
        accessToken: access_token,
        refreshToken: refresh_token,
        expiresAt: new Date(expires_at * 1000),
        athleteId: tok.athleteId,
    };
}

export async function getGear(token) {
    const response = await fetch('https://www.strava.com/api/v3/athlete', {
        headers: {
            'Accept': 'application/json',
            'Authorization': `Bearer ${token.accessToken}`,
        }
    });

    if (response.status !== 200) {
        console.error('Failed to fetch gear:', response);
        throw redirect("/", 301);
    }

    const { bikes, shoes } = await response.json();
    return { bikes, shoes };
}

export async function registerWebhook(tok: AuthToken, callbackUrl: string) {
    const response = await fetch('https://www.strava.com/api/v3/push_subscriptions', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({
            client_id: CLIENT_ID,
            client_secret: CLIENT_SECRET,
            callback_url: callbackUrl,
            verify_token: WEBHOOK_TOKEN
        }),
    });

    if (response.status !== 200) {
        console.error('Failed to create webhook:', response);
        throw redirect("/", 301);
    }
}

export function validateWebhookPayload(urlParams: SearchParams): bool {
    return urlParams.get('hub.verify_token') === WEBHOOK_TOKEN;
}
