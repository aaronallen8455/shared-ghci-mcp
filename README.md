# shared-ghci

A GHCi session that you and an AI agent use together. GHCi runs in tmux, so
you can attach and type as usual, while the agent works with the same session
through an MCP server.

- **No lost output.** `tmux pipe-pane` copies everything GHCi prints to
  `.shared-ghci/ghci.log`, and the agent reads its results from that log
  rather than from the screen. Scrollback limits and wrapped lines can't cut
  off an error message.
- **Exact command boundaries.** GHCi's prompt is set to `[ghci#N]> `. A
  command's output ends at the first prompt whose number is higher than the
  one shown before the command was sent.
- **No interleaving.** The agent won't send input while GHCi is busy or while
  you have a partly typed line at the prompt.

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
- `src/SharedGhci/Output.hs`: pure helpers (prompt detection, escape-code
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

### Wrapper scripts and Docker

The agent needs GHCi's prompt set to `[ghci#N]> `. There are two ways this
happens:

- **`cabal`, `stack`, `ghci` and `ghc` commands:** the launcher adds
  `-ghci-script=.shared-ghci/init.ghci` to the command, so GHCi sets the
  prompt at startup.
- **Any other command** (a wrapper script, `docker run ...`, `nix develop -c
  ...`): the launcher runs it unchanged. When the server sees GHCi's
  `GHCi, version ...` banner in the log, it types the two `:set
  prompt-function` commands into GHCi, and does so again after every restart.
  You'll see those two lines appear in the session.

For a script that runs GHCi in Docker:

- Run the container with `-it`. tmux gives the script a real terminal, and
  GHCi needs one inside the container too.
- Don't start GHCi with `-v0`, which hides the banner the server waits for.
- Compiler errors show paths as the container sees them (e.g.
  `/app/src/Foo.hs`). If the project is mounted somewhere other than its host
  path, tell the agent how to map them, for example in `CLAUDE.md`.

To pass the startup script yourself instead, put `{ghci_script}` in the
command where its path should go, e.g.
`ghci-session start -- ./repl.sh --ghci-options=-ghci-script={ghci_script}`.

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

- A `.ghci` or other startup script that sets its own prompt is fine. If one
  runs after the prompt setup and overrides it, the server notices that GHCi
  started but showed no `[ghci#N]> ` prompt for 3 seconds, and types the setup
  again. If you change the prompt by hand later in a session, though, the
  agent can no longer tell where output ends. `ghci_restart` fixes it.
- Escape sequences are removed from the output the agent sees, including any
  that your program prints on purpose.
- Project paths containing spaces don't work with `cabal`, `stack` or `ghci`
  commands, because `-ghci-script` can't take a path with spaces. Wrapper
  scripts aren't affected.

## Tests

```sh
cabal test unit         # pure tests of SharedGhci.Output (instant)
cabal test integration  # real GHCi in tmux, driven over MCP (~10s)
```

The integration suite starts `ghci` on a small module in a temporary
directory using `bin/ghci-session`. It then runs the server and acts as an MCP
client, covering:

- evaluation and multi-line input
- starting through a wrapper script, with the prompt setup typed into GHCi
- a 300+ line reload error, returned in full
- timeouts, `ghci_wait` and interrupts
- shortening long output
- refusing to send while someone has typed input at the prompt
- detecting that GHCi exited, and restarting it
- protocol errors

It needs tmux and `ghci` on `PATH`. If tmux can't be found, the suite prints
a message and is skipped. Set `SHARED_GHCI_TMUX` to point at tmux if it is
installed somewhere unusual.
