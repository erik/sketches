import {
    createCookie,
    createCookieSessionStorage,
    redirect,
} from "remix";

import { secrets } from "~/env.server.ts";

const { getSession, commitSession, destroySession } = createCookieSessionStorage({
    cookie: {
        name: "__session",
        secrets: [
            secrets.COOKIE_SECRET,
        ],
        // secure: true,
        sameSite: "lax",
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
