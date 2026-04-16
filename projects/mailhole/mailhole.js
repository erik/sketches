#!/usr/bin/env osascript -l JavaScript

ObjC.import("Cocoa");
ObjC.import("Vision");
ObjC.import("ImageIO");

const LOOKBACK_DAYS = 90;
const SUBJECT_LINE = "Your Daily Digest";

function run(argv) {
  if (argv.length !== 3) {
    console.log("usage: mailhole.js <account name> <mailbox> <regexp>");
    return;
  }

  const app = Application.currentApplication();
  app.includeStandardAdditions = true;

  const tempBase = app.systemAttribute("TMPDIR");
  const tempDir = `${tempBase}mailhole`;
  app.doShellScript(`mkdir -p '${tempDir}'`);

  const Mail = Application("Mail");

  const [accountName, mailboxName, matchText] = argv;
  const matchRegex = new RegExp(matchText, "i");

  const acctNames = Mail.accounts.name();
  if (!acctNames.includes(accountName)) {
    console.log(
      `error: account "${accountName}" not found. available: ${acctNames.join(", ")}`,
    );
    return;
  }
  const acct = Mail.accounts.byName(accountName);

  const mbox = acct.mailboxes.byName(mailboxName);
  try {
    mbox.name();
  } catch (e) {
    console.log(
      `error: mailbox "${mailboxName}" not found. available: ${acct.mailboxes.name().join(", ")}`,
    );
    return;
  }

  const cutoff = new Date(Date.now() - LOOKBACK_DAYS * 24 * 60 * 60 * 1000);
  const unread = mbox.messages.whose({
    _and: [
      { subject: { _contains: SUBJECT_LINE } },
      { dateReceived: { _greaterThan: cutoff } },
      { readStatus: false },
    ],
  })();

  const total = unread.length;
  console.log(`Found ${total} unread digests.`);

  if (total === 0) return;

  const matchedMessages = [];

  for (let idx = 0; idx < total; idx++) {
    const msg = unread[idx];
    const prefix = `[${idx + 1}/${total}]`;
    const date = msg.dateReceived().toISOString().slice(0, 10);

    const scans = msg.mailAttachments().filter(
      (it) =>
        // filter out adverts that get included in the emails
        !it.name().startsWith("content-") && !it.name().startsWith("mailer-"),
    );

    let isMatch = false;
    for (let si = 0; si < scans.length; si++) {
      const scan = scans[si];
      const p = `${tempDir}/att_${idx}_${si}`;
      Mail.save(scan, { in: Path(p) });

      try {
        const text = extractImageText(p);

        if (matchRegex.test(text)) {
          isMatch = true;
          console.log(`${prefix} ${date} - MATCHED -- ${text}`);

          // No need to keep OCRing after we've found a first match
          break;
        }
      } catch (e) {
        console.log(`${prefix} ${date} - OCR ERROR: ${e.message}`);
      }
    }

    if (isMatch) {
      msg.flaggedStatus = true;
      matchedMessages.push({
        subject: msg.subject(),
        messageId: msg.messageId(),
      });
    } else {
      console.log(`${prefix} ${date}`);
      msg.readStatus = true;
      msg.flaggedStatus = false;
    }
  }

  // Clean up temp directory
  const fm = $.NSFileManager.defaultManager;
  fm.removeItemAtPathError($(tempDir), $());

  if (matchedMessages.length > 0) {
    const count = matchedMessages.length;
    const noun = count === 1 ? "message" : "messages";
    const subjects = matchedMessages.map((m) => `• ${m.subject}`).join("\n");

    const response = app.displayAlert("You've got mail", {
      message: `${count} ${noun} with your name:\n\n${subjects}`,
      as: "critical",
      buttons: ["Dismiss", "Open in Mail"],
      defaultButton: "Open in Mail",
    });

    if (response.buttonReturned === "Open in Mail") {
      for (const m of matchedMessages) {
        app.openLocation(`message://%3c${encodeURIComponent(m.messageId)}%3e`);
      }
    }
  }
}

// MacOS built-in text recognition API
// https://developer.apple.com/documentation/vision/vnrecognizetextrequest
function extractImageText(path) {
  const url = $.NSURL.fileURLWithPath($(path));
  const src = $.CGImageSourceCreateWithURL(url, null);
  const cgImage = $.CGImageSourceCreateImageAtIndex(src, 0, null);

  const request = $.VNRecognizeTextRequest.alloc.init;
  request.recognitionLevel = $.VNRequestTextRecognitionLevelAccurate;

  const handler = $.VNImageRequestHandler.alloc.initWithCGImageOptions(
    cgImage,
    $(),
  );
  const requests = $.NSArray.arrayWithObject(request);
  handler.performRequestsError(requests, Ref());

  const results = request.results;
  const lines = [];
  for (let i = 0; i < results.count; i++) {
    const obs = results.objectAtIndex(i);
    lines.push(obs.topCandidates(1).objectAtIndex(0).string.js);
  }
  return lines.join(" ");
}
