# mailhole

A lil AppleScript tool to OCR USPS Informed Delivery emails and notify you when
something matches a regex.

```bash
./mailhole.jxa "iCloud" "SomeMailLabel/USPS" "your name"
```

## running it automatically

Use the plist template below and replace the placeholder values.

```bash
# install
launchctl load ~/Library/LaunchAgents/xxx.mailhole.plist

# uninstall
launchctl unload ~/Library/LaunchAgents/com.erik.mailhole.plist
lm ~/Library/LaunchAgents/com.erik.mailhole.plist
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>xxx.mailhole</string>
    <key>ProgramArguments</key>
    <array>
        <string> /PATH/TO/mailhole.jxa </string>
        <string> SOME_ACCOUNT </string>
        <string> SOME_MAILBOX </string>
        <string> SOME_MATCH_STRING </string>
    </array>

    <!-- Weekly on Monday at 10am -->
    <key>StartCalendarInterval</key>
    <dict>
        <key>Weekday</key>
        <integer>1</integer>
        <key>Hour</key>
        <integer>10</integer>
        <key>Minute</key>
        <integer>0</integer>
    </dict>
    <key>StandardOutPath</key>
    <string>/path/to/mailhole.log</string>
    <key>StandardErrorPath</key>
    <string>/path/to/mailhole.log</string>
</dict>
</plist>
```

## license

gl;hf
