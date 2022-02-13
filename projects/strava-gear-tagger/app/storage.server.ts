import * as strava from "~/util/strava";


export async function getStravaCredentials(athleteId: string) {
    let token = await KV_STRAVA_OAUTH.get(`${athleteId}`);
    console.log('here is what', token)
    token = JSON.parse(token || '{}');

    if (!token.expiresAt || new Date(token.expiresAt) < new Date()) {
        console.log('Token appears expired:', token);
        token = strava.refreshToken(token);
        await setStravaCredentials(athleteId, token);
    }

    return token;
}

export async function setStravaCredentials(athleteId: string, token) {
    await KV_STRAVA_OAUTH.put(`${athleteId}`, JSON.stringify(token));
}

export async function getGearMapping(athleteId: string) {
    const map = await KV_GEAR_MAP.get(`${athleteId}`);
    return map ? JSON.parse(map) : {};
}

export async function setGearMapping(athleteId: string, gearMap) {
    await KV_GEAR_MAP.put(`${athleteId}`, JSON.stringify(gearMap));
}
