# shared-ghci

A GHCi session that you and an AI agent use together. GHCi runs in tmux, so
you can attach and type as usual, while the agent works with the same session
through an MCP server.

- **No lost output.** `tmux pipe-pane` copies everything GHCi prints to
  `.shared-ghci/ghci.log`, and the agent reads its results from that log
  rather than from the screen. Scrollback limits and wrapped lines can't cut
  off an error message.
- **Exact command boundaries, whatever the prompt.** After each command the
  server types an end-marker line, `System.IO.putStrLn
  "<<shared-ghci:\&17>>"`. GHCi runs it once the command finishes, so the
  printed `<<shared-ghci:17>>` marks the end of the command's output. Your
  prompt, `.ghci` files and `:set prompt` don't matter.
- **No clobbering.** The agent won't send a command while one of its own is
  still running, or while you have a partly typed line at the prompt.

## Requirements

- tmux (any recent version; checked: `$PATH`, `~/.nix-profile/bin`, Homebrew, `/usr/bin`)
- GHC and cabal (the server depends on `aeson`)

## Setup

```sh
# 1. Build and install the MCP server
cabal install --installdir="$HOME/.local/bin" --install-method=copy --overwrite-policy=always

# 2. Put the launcher on your PATH
ln -s "$PWD/bin/ghci-session" ~/.local/bin/ghci-session

# 3. Register the MCP server with Claude Code for all projects
claude mcp add --scope user shared-ghci -- "$HOME/.local/bin/shared-ghci-mcp"
```

Sources:

- `app/Main.hs`: the MCP server (JSON-RPC over stdio, using aeson)
- `src/SharedGhci/Session.hs`: driving tmux and reading the log
- `src/SharedGhci/Output.hs`: pure helpers (finding end markers, escape-code
  stripping, input preparation, shortening long output)

The server finds the session by searching upward from Claude Code's working
directory for `.shared-ghci/`. Set `SHARED_GHCI_DIR` to override.

## Usage

In a Haskell project root:

```sh
ghci-session up                         # start (cabal repl) and attach
ghci-session start -- cabal repl mylib  # pick a component; don't attach
ghci-session start -- stack ghci
ghci-session start -- ./repl.sh         # any wrapper script, e.g. GHCi in docker
ghci-session attach                     # attach later (detach: C-b d)
ghci-session log                        # page the full raw log
ghci-session restart | stop | status
```

The session uses its own tmux server (`tmux -L shared-ghci`), so it doesn't
interfere with your normal tmux sessions. That server keeps 100,000 lines of
scrollback. If GHCi exits, the pane stays open so you can read why.

`.shared-ghci/` contains a `.gitignore` that ignores the whole directory.

### Viewing the session in Neovim (or any terminal)

```vim
:vsplit | terminal ghci-session attach
```

The session's tmux server has no status bar and doesn't use the terminal's
alternate screen. tmux therefore scrolls the terminal it is attached to,
rather than repainting it in place. GHCi's output ends up in that terminal's
own scrollback: a Neovim `:terminal` buffer, iTerm, and so on. You can then
scroll and search it with normal Neovim motions. Neovim keeps 10,000 lines of
terminal scrollback by default; to keep more:

```vim
autocmd TermOpen * setlocal scrollback=100000
```

When tmux redraws the whole screen, for example after the window is resized,
the visible lines can appear twice in the scrollback. tmux's own scrollback
(`C-b [`) has everything too.

These settings take effect when the session's tmux server starts. If a
session from an older version is running, restart the server with
`ghci-session stop` and `tmux -L shared-ghci kill-server`, then start again.

### Wrapper scripts and Docker

The launcher runs whatever command you give it unchanged, so any wrapper
works: a script, `docker run ...`, `nix develop -c ...`. For a script that runs
GHCi in Docker:

- Run the container with `-it`. tmux gives the script a real terminal, and
  GHCi needs one inside the container too.
- Compiler errors show paths as the container sees them (e.g.
  `/app/src/Foo.hs`). If the project is mounted somewhere other than its host
  path, tell the agent how to map them, for example in `CLAUDE.md`.

## Agent tools

| Tool             | Purpose                                                         |
|------------------|-----------------------------------------------------------------|
| `ghci_reload`    | `:reload` and return the compiler errors and warnings           |
| `ghci_eval`      | Any GHCi input: expressions, `:t`, `:i`, `:browse`, multi-line code (wrapped in `:{ :}` automatically) |
| `ghci_wait`      | Keep waiting for a command that timed out, or for GHCi to start |
| `ghci_interrupt` | Send Ctrl-C                                                     |
| `ghci_restart`   | Restart GHCi with the original command (e.g. after `.cabal` changes) |
| `ghci_status`    | Show whether GHCi is ready, plus recent output (including yours) |

Output longer than 400 lines is shortened to the first 300 and last 60 lines.
The full text goes to `.shared-ghci/last-output.txt`.

You may want to add this to a project's `CLAUDE.md`:

> After editing Haskell sources, use the `ghci_reload` tool for compiler
> feedback instead of running `cabal build`. Use `ghci_eval` with `:t` and
> `:i` to check types.

## Caveats

- You'll see the end-marker line and its output in the terminal after each
  command the agent runs.
- A command that reads from stdin (e.g. `getLine`) receives the end-marker
  line as its input, and the agent's call then times out. `ghci_interrupt`
  recovers.
- The agent can only tell that you're mid-way through typing a line, not
  that you're running something. If you are, its command waits in GHCi's
  input until yours finishes.
- The prompt is removed from the agent's output by matching it against the
  prompt GHCi shows next. Runs of digits (line numbers, clocks) match any
  digits, but a prompt that changes in other ways between consecutive prompts
  can leave prompt text in the output.
- Escape sequences are removed from the output the agent sees, including any
  that your program prints on purpose.

## Tests

```sh
cabal test unit         # pure tests of SharedGhci.Output (instant)
cabal test integration  # real GHCi in tmux, driven over MCP (~10s)
```

The integration suite starts `ghci` on a small module in a temporary
directory using `bin/ghci-session`. It then runs the server and acts as an MCP
client, covering:

- evaluation and multi-line input
- a project `.ghci` with its own prompt, and changing the prompt mid-session
  (to multi-line and line-numbered prompts)
- starting through a wrapper script
- a 300+ line reload error, returned in full
- timeouts, `ghci_wait` and interrupts
- shortening long output
- refusing to send while someone has typed input at the prompt
- detecting that GHCi exited, and restarting it
- protocol errors

It needs tmux and `ghci` on `PATH`. If tmux can't be found, the suite prints
a message and is skipped. Set `SHARED_GHCI_TMUX` to point at tmux if it is
installed somewhere unusual.
