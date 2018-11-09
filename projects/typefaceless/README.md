# typefaceless

Google Web Fonts are super handy, but I don't like the idea of Google having
such a broad tracking vector.

So this extension just rewrites any requests to Google Web Fonts to obscure
the domain the font is being requested from as well as the browser.

Not a replacement for an adblocker.

## todo

- [x] Rewrite requests
- [ ] Add basic UI elements
  - [ ] Create / hook up icons
  - [ ] Toggle enabled / disabled
  - [ ] Basic request counting (per page, all time, most common fonts?)
- [x] Minimal configurable rewriting (for UA / host)
- [ ] Tutorial on install, what it does, what it doesn't
- [ ] Publish to AMO/Chrome Extensions
