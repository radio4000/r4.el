# r4.el: guide for working on it

r4.el is the Emacs client of [radio4000.com](https://radio4000.com): it
does what the web app (`../r4-svelte`) and the command line (`../r4`)
do, with their words, in Emacs lists, pages and menus.

- Canonical repository: `github` remote, `git@github.com:radio4000/r4.el.git`,
  branch `main`. Mirror: `origin`, `git@gitlab.com:ugrp/lisp/r4.el.git`.
  Push to both.
- CI (`.github/workflows/test.yml`): compile, test and lint on Emacs
  29.1, 30.1 and snapshot. `pages.yml` publishes the site
  (https://radio4000.github.io/r4.el/) from `readme.org`, `features.org`
  and `source.org`.
- Users install with `package-vc` (`:rev :newest`), so every pushed
  commit reaches them.

## Documents

- `readme.org`: for users, installation and usage, one place per feature.
- `features.org`: what the web and the command line do, and how to do
  it in Emacs. Update it with each feature.
- `source.org`: every file in reading order, one line on its role.
  Add new files there.
- This file: architecture and conventions, for developers.

## Architecture

Layers, from bottom to top (see `source.org` for each file):

1. Data: `r4-models` (structs built from API rows), `r4-api` (Supabase
   REST; `r4-api-get` and `r4-api-send` are asynchronous,
   `r4-api-request` blocks, use it only for user actions), `r4-auth`.
2. Media: `r4-files` (the folders of the `r4` command line, keep them
   compatible), `r4-download` (yt-dlp queue), `r4-mpv` (JSON IPC),
   `r4-player`, `r4-video` (videos in Emacs), `r4-broadcast` (listen),
   `r4-broadcasting` (go live).
3. Interface: `r4-columns` (column specs), `r4-tracks`, `r4-channels`,
   `r4-home`, `r4-channel-page`, `r4-player-window`, `r4-queue`,
   `r4-faces`, `r4-filter`, `r4-search`.
4. Entry: `r4-menus` (transient menus and the shared keys), `r4.el`
   (requires everything).

Mechanisms that cut across files:

- Hooks, not requires, connect the layers: `r4-player-changed-hook`
  (every second while playing, so keep handlers cheap),
  `r4-player-track-changed-hook`, `r4-download-changed-functions`.
- To avoid require cycles, a lower file exposes a function variable that
  a higher one sets: `r4-channels-following-function`,
  `r4-channels-synced-function`, `r4-tracks-visit-channel-function`,
  `r4-video-show-function`, `r4-video-area-function`.
- Lists update only the rows that changed (`r4-tracks-update-status`,
  `r4-channels-status-changed` with slugs). A full redraw of a long list
  costs a quarter of a second: never on a timer or on download events.
- Columns are `r4-column` structs in `defconst` specs, so a new version
  replaces them when it loads. Headers are the radio4000.com field
  names with spaces ("created at"), options use the field names
  (`created_at`).
- Menus: `r4-menus--define` adds the "Go to" and player groups to each
  menu. Transient formats descriptions in a temporary buffer, so read
  the state of a list inside `r4-menus--in-buffer`. Each r4 buffer binds
  `?` to its own menu, whose heading says what it acts on.
- Shared keys (`r4-player-map`, the parent of every r4 keymap): the
  player keys, on Meta (`M-n` `M-p` tracks, `M-f` `M-b` seek, `M-<up>`
  `M-<down>` volume) and `SPC` pause, and the main views (`^` up, `j`
  visit a channel, `E`, `B`, `H`). A list key must not hide them; the
  tests check it.
- Keys stay close to Emacs: never take a key Emacs gives a meaning in
  `special-mode`, `tabulated-list-mode` or Dired (`n` `p` `<` `>` `-`
  digits `g` `q` `h`), except `SPC` for pause. Lists follow Dired
  (`m` `u` `U` `t` `% m` marks, actions on the marked items else the one
  at point, `^` up, `j` jump, `w` copy, `v` view); uppercase acts on
  the whole list or is bigger; a prefix argument varies a command
  (`C-u V` pops the video out).
- Faces are in `r4-faces` and inherit standard faces, to follow themes.
- Videos: mpv draws with `--wid` in a child frame kept over an area of
  the player window. On Wayland, mpv ignores `--wid` unless started
  with `--gpu-context=x11egl`.
- Broadcasting writes the `broadcast` table and sends `state` messages
  to the Realtime topic `broadcast-state:CHANNEL_ID` with the Realtime
  HTTP API, like `src/lib/broadcast.js` of r4-svelte.

## Conventions

- Use the words of radio4000.com: its models (`slug`, `track_count`,
  `created_at`…) and its interface. Do not invent words.
- The same key does the same thing everywhere. Like magit: `?` opens
  the menu of the buffer, and the menu keys work without opening it.
- Local channels never offer radio4000.com features (web links…).
- Prefer Emacs defaults (`tabulated-list-mode`, `display-buffer`
  actions, Customize) to custom code. If a workaround seems needed,
  ask first.
- Keep files small, one area each. Keep commits atomic.
- Never add Claude as author or co-author of commits.
- Stay quiet: no message per item, summaries instead (see downloads).
  Never block Emacs on the network or the disk in timers or hooks.

## Development and tests

```sh
make compile   # each file in a fresh Emacs, warnings are errors
make test      # ERT, test/r4-test-*.el
make lint      # checkdoc
make site      # website in ./public
```

- Tests never touch the user's files: set `r4-history-file`,
  `r4-channels-cache-file` and `r4-auth-session-file` to nil and use a
  temporary `r4-download-directory` (see `r4-test--with-backend`, a fake
  Supabase backend).
- Never open visible windows: run Emacs with `--batch`. A GUI Emacs
  shows its first frame even with `(visibility . nil)`.
- In batch, `format-mode-line` returns "", and long cells of
  tabulated lists are hidden with display properties, not truncated:
  check the data, not the formatted text.
- Never act on radio4000.com for real (going live, editing) without the
  user's go-ahead: test with the fake backend.

## Upgrades

Users upgrade with `M-x package-vc-upgrade RET r4`. The old version
lingers in the running Emacs (buffer-local menus, `defvar` values), so
tell them to restart Emacs. Bump `Version:` in `r4.el` for releases:
`use-package :vc` without `:rev :newest` installs the last version bump.
