import {
    createCookie,
    createCookieSessionStorage,
    redirect,
} from "remix";

const { getSession, commitSession, destroySession } = createCookieSessionStorage({
    kv: KV_SESSION,
    cookie: {
        name: "__session",
        secrets: ["TODO CHANGE ME"],
        // secure: true,
        sameSite: "Lax",
        path: "/",
    },
});

export function getAthleteOrLogin(session): string {
    if (!session.has("athlete_id")) {
        throw redirect("/", 301);
    }

    return session.get('athlete_id');
}

export { getSession, commitSession, destroySession };
