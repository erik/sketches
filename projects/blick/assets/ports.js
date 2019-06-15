function generateKey() {
    return window.crypto.subtle.generateKey(
        {name: 'AES-GCM', length: 256},
        true,
        ['encrypt', 'decrypt']
    );
}

function registerPorts(app) {
    // Show prompt dialog
    app.ports.showKeyPrompt.subscribe((prompt) => {
        let key = window.prompt(prompt);
        app.ports.showKeyPromptResult.send(key);
    });


    app.ports.encryptString.subscribe(async (req) => {
        let key = await generateKey();
        console.log('to encrypt:', req, key);
    });
}
