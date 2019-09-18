# oop

Simple personal file upload server.

I use this to upload files from my phone (or wherever) to my VPS, and
then serve the static content using nginx.

> NOTE: There is no authentication built in. Use your actual web
> server to configure Basic Auth or something.

```console
oop -port 5432 -host 127.0.0.1 \
    -uploaddir /var/www/oop \
    -url http://example.com/static/
```

## License

Available under AGPL-3.0 or later. See `LICENSE` for details.
