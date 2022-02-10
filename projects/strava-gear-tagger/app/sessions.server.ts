import {
    createCookie,
    createCloudflareKVSessionStorage
} from "remix";

const { getSession, commitSession, destroySession } = createCloudflareKVSessionStorage({
    kv: KV_SESSION,
    cookie: {
        name: "__session",
        secrets: ["TODO CHANGE ME"],
        // secure: true,
        sameSite: "Lax",
        path: "/",
    },
});

export { getSession, commitSession, destroySession };
