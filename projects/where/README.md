# where

Very minimal service to keep people up to date on where you are.

![](https://imgur.com/wZGbmOC.png)

This project was put together quickly before I left for a bicycle tour
where I'd have intermittent cell service, but wanted to keep my family
up to date on my location and state of alive-ness.

## setup

1. Have redis-server running.
2. Set up [https://developers.google.com/identity/protocols/OAuth2](Google OAuth).
3. Use the client id and client secret in the config.

``` console
$ npm install
$ mv example.env .env
$ $EDITOR .env # populate with your own values
$ node server.js
```

Update your location by going to `/where`.

## license

AGPL-3.0
