# tracker-tracker

A simple JS tool that lists all sub-requests that are caused by visiting a given
URL.

Useful to enumerate and track over time if the list changes.

## Usage

```shell
❯ node index.js https://reddit.com | grep -vE '(reddit.com|redditmedia.com|redditstatic.com|redd.it|gfycat.com|data:)'
https://api-js.datadome.co/js/
https://cdn.embed.ly/player-0.1.0.min.js
https://www.googletagmanager.com/gtm.js?id=GTM-5XVNS82&l=googleTagManager
https://id.rlcdn.com/472486.gif?gtmcb=2063110210
```
