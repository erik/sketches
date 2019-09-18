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

## simple nginx setup

Using Nginx as a reverse proxy to serve `oop` with Basic
Authentication as well as to serve the uploaded static files.

1. Set up your credentials using `htpasswd`

   ```console
   sudo htpasswd -c /etc/nginx/oop.htpasswd whatever-user
   ```

2. Update Nginx configuration

   ```conf
   location /uploads/ {
       proxy_pass http://localhost:12345

       auth_basic "protected area";
       auth_basic_user_file /etc/nginx/oop.htpasswd;

       client_max_body_size 50M;
   }

   location /static/ {
       alias /var/www/uploads/;
   }
   ```

3. Run oop

   ```console
   oop -port 12345 -host 127.0.0.1 \
       -url http://example.com/static/ \
       -uploaddir /var/www/uploads/
   ```


## License

Available under AGPL-3.0 or later. See `LICENSE` for details.
