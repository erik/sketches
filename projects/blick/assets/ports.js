async function generateKey() {
    return await window.crypto.subtle.generateKey(
        {name: 'AES-GCM', length: 256},
        /* extractable = */ true,
        ['encrypt', 'decrypt']
    );
}

async function encryptMessage(key, message) {
    const encodedMsg = new TextEncoder().encode(message);

    // 96 bit IV
    const iv = window.crypto.getRandomValues(new Uint8Array(12));
    const encrypted = await window.crypto.subtle.encrypt(
        {name: 'AES-GCM', iv},
        key,
        encodedMsg
    );

    const buf = new Unit8Array(encrypted);

    return {
        cipher: window.btoa(buf),
        iv: window.btoa(iv)
    };
}

async function decryptMessage(key, msg) {
    const {iv, cipher} = msg;

    const decrypted = await window.crypto.subtle.encrypt(
        {name: 'AES-GCM', iv: window.atob(iv)},
        key,
        window.btoa(cipher)
    );

    return new TextDecoder().decode(decrypted);
}

async function importKey(str) {
    const buf = window.atob(str);

    return await window.crypto.subtle.importKey(
        /* format = */ "raw",
        /* keyData = */ buf,
        {name: 'AES-GCM', length: 256},
        /* extractable = */ true,
        ['encrypt', 'decrypt']
    );
}

async function exportKey(key) {
    // Could export as raw instead, since we only care about the b64
    // encoded key, but JWK already does the encoding for us.
    const jwk = await window.crypto.subtle.exportKey("jwk", key);
    return jwk.k;
}

function registerPorts(app) {
    // Show prompt dialog
    app.ports.showKeyPrompt.subscribe((prompt) => {
        const key = window.prompt(prompt);
        app.ports.showKeyPromptResult.send(key);
    });

    app.ports.encryptString.subscribe(async (text) => {
        const key = await generateKey();
        console.log('to encrypt:', text, key);

        // base64 encode exported key
        const exportedKey = await exportKey(key);
        // TODO: send back to app
    });

    app.ports.decryptString.subscribe(async (msg) => {
        const key = await importKey(msg.key);

        // TODO: handle bad key scenario
        const decoded = decodeMessage(key, msg.text)
        console.log('decrypted', decoded);

        // TODO: send back to app
    });

}
