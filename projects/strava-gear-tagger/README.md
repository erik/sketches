# strava-gear-tagger

Set default bike or shoes on Strava activities depending on type.

e.g.

- Use my trainer for all virtual rides
- Use hiking boots by default for all hikes
- Use running shoes for all runs

Save those precious 5 seconds.

## Development

``` sh
# Dev server
npm run dev

# Miniflare (local CloudFlare Workers impl)
npm start
```

Server will be running on http://127.0.0.1:8787

## Deployment

First modify `wrangler.toml` so it doesn't point to my resources.

``` sh
# Deploy the worker
npm run deploy

# Create the secrets
wrangler secret put STRAVA_CLIENT_ID
wrangler secret put STRAVA_CLIENT_SECRET
wrangler secret put STRAVA_WEBHOOK_TOKEN
wrangler secret put STRAVA_COOKIE_SECRET

# Create the webhook
curl -X POST https://www.strava.com/api/v3/push_subscriptions \
     -F 'client_id=FOO' \
     -F 'client_secret=BAR' \
     -F 'callback_url=https://BAZ.workers.dev/webhook' \
     -F 'verify_token=QUUX'

# Ensure the webhook was registered
curl -G https://www.strava.com/api/v3/push_subscriptions \
     -d client_id=FOO \
     -d client_secret=BAR
```
