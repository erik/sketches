export async function getGearMapping(athleteId: string) {
    const map = await KV_GEAR_MAP.get(`${athleteId}`);
    return map ? JSON.parse(map) : null;
}

export async function setGearMapping(athleteId: string, gearMap) {
    await KV_GEAR_MAP.put(`${athleteId}`, JSON.stringify(gearMap));
}
