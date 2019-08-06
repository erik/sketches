const ALG = {name: 'AES-GCM', length: 256};

// Generate a symmetric encryption key suitable for use with our
// algorithm.
async function generateKey() {
  return window.crypto.subtle.generateKey(
    ALG,
    /* extractable = */ true,
    ['encrypt', 'decrypt']
  );
}


async function encryptMessage(key, message) {
  const encoded = new TextEncoder().encode(message);

  // Use 96 bits for IV
  const iv_size = 96 / 8;
  const iv = window.crypto.getRandomValues(new Uint8Array(iv_size));

  const encrypted = await window.crypto.subtle.encrypt(
    {name: ALG.name, iv},
    key,
    encoded
  );

  const buf = new Uint8Array(encrypted);
  return `${arrayBufToB64(iv)}.${arrayBufToB64(buf)}`;
}

async function decryptMessage(key, msg) {
  const [iv, cipher] = msg
        .split('.', 2)
        .map(s => b64ToArrayBuf(s));

  const decrypted = await window.crypto.subtle.decrypt(
    {name: ALG.name, iv},
    key,
    cipher
  );

  return new TextDecoder().decode(decrypted);
}

async function importKey(str) {
  const buf = b64ToArrayBuf(str);

  return await window.crypto.subtle.importKey(
    /* format = */ "raw",
    /* keyData = */ buf,
    ALG,
    /* extractable = */ true,
    ['encrypt', 'decrypt']
  );
}

async function exportKey(key) {
  const buf = await window.crypto.subtle.exportKey(
    /* format = */ "raw",
    /* keyData = */ key,
  );

  return arrayBufToB64(new Uint8Array(buf));
}

function arrayBufToB64(buf) {
  const decoded = String.fromCharCode.apply(null, buf);
  return window.btoa(decoded);
}

function b64ToArrayBuf(b64) {
  const bytes = window.atob(b64);
  const buf = new Uint8Array(bytes.length);
  for (let i = 0; i < bytes.length; ++i) {
    buf[i] = bytes.charCodeAt(i);
  }

  return buf.buffer;
}


async function encrypt(input) {
  const key = await generateKey();

  return {
    key: await exportKey(key),
    cipher: await encryptMessage(key, input),
  };
}

async function decrypt(key, cipher) {
  const k = await importKey(key);
  return await decryptMessage(k, cipher);
}
