# Medium robots.txt reference

Fetched on 2026-05-15 for the Medium image downloader.

## `https://medium.com/robots.txt`

Relevant rules for the project:

```txt
User-Agent: *
Disallow: /m/
Disallow: /me/
Disallow: /@me$
Disallow: /@me/
Disallow: /*/edit$
Disallow: /*/*/edit$
Disallow: /media/
Disallow: /p/*/share
Disallow: /r/
Disallow: /trending
Disallow: /search?q$
Disallow: /search?q=
Disallow: /*/search?q=
Disallow: /*/*source=
```

The downloader uses this to skip direct `https://medium.com/media/...` URLs.

## `https://miro.medium.com/robots.txt`

Current result when fetched on 2026-05-15:

```txt
HTTP 404
```

That means there is no local robots rule to quote for `https://miro.medium.com/v2/...` CDN image URLs. The downloader should not treat this missing robots file as a disallow rule.
