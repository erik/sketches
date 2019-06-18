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

    const buf = new Uint8Array(encrypted);

    return {
        cipher: arrayBufferToB64(buf),
        iv: arrayBufferToB64(iv)
    };
}

function b64ToArrayBuffer(b64) {
    const bytes = window.atob(b64);
    const buf = new Uint8Array(bytes.length);
    for (let i = 0; i < bytes.length; ++i) {
        buf[i] = bytes.charCodeAt(i);
    }

    return buf.buffer;
}

function arrayBufferToB64(buf) {
    const decoded = String.fromCharCode.apply(null, buf);
    return window.btoa(decoded);
}

async function decryptMessage(key, msg) {
    const msgBuf = b64ToArrayBuffer(msg.cipher);
    const iv = b64ToArrayBuffer(msg.iv);

    const decrypted = await window.crypto.subtle.decrypt(
        {name: 'AES-GCM', iv},
        key,
        msgBuf
    );

    return new TextDecoder().decode(decrypted);
}

async function importKey(str) {
    const buf = b64ToArrayBuffer(str);

    return await window.crypto.subtle.importKey(
        /* format = */ "raw",
        /* keyData = */ buf,
        {name: 'AES-GCM', length: 256},
        /* extractable = */ true,
        ['encrypt', 'decrypt']
    );
}

async function exportKey(key) {
    const buf = await window.crypto.subtle.exportKey(
        /* format = */ "raw",
        /* keyData = */ key,
    );
    const bufView = new Uint8Array(buf);

    return arrayBufferToB64(bufView);
}

function registerPorts(app) {
    // Show prompt dialog
    app.ports.showPrompt.subscribe((prompt) => {
        const key = window.prompt(prompt);
        app.ports.showPromptResult.send(key);
    });

    app.ports.encryptString.subscribe(async (text) => {
        const key = await generateKey();
        console.log('to encrypt:', text, key);

        // base64 encode exported key
        const exportedKey = await exportKey(key);
        const msg = await encryptMessage(key, text);

        console.log('encrypted "', text,'" to => ', msg);

        const response = {
            blob: JSON.stringify(msg),
            key: exportedKey
        };
        app.ports.encryptStringResult.send(response);
    });

    app.ports.decryptString.subscribe(async (msg) => {
        const key = await importKey(msg.key);
        const request = JSON.parse(msg.blob);

        console.log('got request => ', request);

        // TODO: handle bad key scenario
        const decoded = await decryptMessage(key, request);
        console.log('decrypted', decoded);

        app.ports.decryptStringResult.send(decoded);
    });

}
