# where

Very minimal service to keep people up to date on where you are.

![](https://imgur.com/wZGbmOC.png)

This project was put together quickly before I left for a bicycle tour
where I'd have intermittent cell service, but wanted to keep my family
up to date on my location and state of alive-ness.

## setup

``` console

$ redis-server
$ git clone https://github.com/erik/where.git && cd where
$ npm install
$ mv example.env .env
$ vim .env # populate with your own values
$ node server.js
```

## license

AGPL-3.0
