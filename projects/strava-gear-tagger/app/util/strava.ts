type AuthToken = {
    athlete_id: number;
    access_token: string;
    refresh_token: string;
    expires_at: Date;
};

// Defined in .env
const CLIENT_ID = globalThis.STRAVA_CLIENT_ID;
const CLIENT_SECRET = globalThis.STRAVA_CLIENT_SECRET;

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

export async function exchangeCodeForToken(code: string): AuthToken {
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
        throw 'TODO HANDLE ME'
    }

    const {
        access_token,
        refresh_token,
        expires_at,
        athlete: { id },
    } = await response.json();

    return {
        access_token,
        refresh_token,
        expires_at: new Date(expires_at * 1000),
        athlete_id: id,
    };
}

export async function refreshToken(tok: AuthToken): AuthToken {
    const response = await fetch('https://www.strava.com/api/v3/oauth/token', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({
            refresh_token: tok.refresh_token,
            client_id: CLIENT_ID,
            client_secret: CLIENT_SECRET,
            grant_type: 'refresh_token',
        }),
    });

    const {
        access_token,
        refresh_token,
        expires_at,
    } = await response.json();

    return {
        access_token,
        refresh_token,
        expires_at: new Date(expires_at * 1000),
        athlete_id: tok.athlete_id,
    };
}

export async function getGear(tok: AuthToken) {
    const response = await fetch('https://www.strava.com/api/v3/athlete', {
        headers: {
            'Accept': 'application/json',
            'Authorization': `Bearer ${tok.access_token}`,
        }
    });

    const { bikes, shoes } = await response.json();
    return { bikes, shoes };
}
