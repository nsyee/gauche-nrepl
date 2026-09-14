# gauche-nrepl

[Gauche Scheme](https://practical-scheme.net/gauche/) implementation of an
[nREPL](https://nrepl.org/) server and client, covering the same protocol
surface as [ts-nrepl](https://github.com/nsyee/ts-nrepl).

No dependencies beyond Gauche itself: Bencode is implemented from scratch, the
transport uses `gauche.net`, and every session evaluates in its own anonymous
module.

## Requirements

Gauche >= 0.9.10 (`gosh` on `PATH`).

## Usage

```bash
bin/nrepl-server            # server on port 7888 (override with NREPL_PORT or argv)
bin/nrepl-client            # interactive client, `:quit` to exit
make test                   # gauche.test suites
```

```
nrepl> (define (add a b) (+ a b))
=> add
nrepl> (add 1 2)
=> 3
nrepl> (display "hi")
hi=> #<undef>
```

Programmatic client:

```scheme
(add-load-path "lib")
(use nrepl.client)

(define client (nrepl-connect 7888))
(define session (nrepl-clone client))
(define responses (nrepl-eval client "(define x 41) (+ x 1)" session))
;; responses is a list of hash tables; (response-ref r "value") => "42"
(nrepl-close client session)
(nrepl-disconnect client)
```

Embedding the server:

```scheme
(use nrepl.server)

(define (main args)
  (let1 server (start-nrepl-server 0)   ; 0 picks a free port
    (print (nrepl-server-port server))
    (nrepl-server-wait server)))        ; or (nrepl-server-stop! server)
```

Start the server from `main` (or otherwise after the script has finished
loading): connection threads evaluate `define` forms, and doing so while the
main thread is still inside `load` deadlocks in Gauche.

## Protocol

TCP transport, Bencode encoding, every message is a dictionary.

| op | request | response |
| --- | --- | --- |
| `clone` | `{id, op}` | `{id, new-session, status: ["done"]}` |
| `eval` | `{id, op, session, code}` | optional `{id, session, out}`, then `{id, session, value}` or `{id, session, err, status: ["eval-error"]}`, then `{id, session, status: ["done"]}` |
| `close` | `{id, op, session}` | `{id, session, status: ["session-closed", "done"]}` |
| `describe` | `{id, op}` | `{id, ops, versions, status: ["done"]}` |

Unknown ops answer `status: ["error", "unknown-op", "done"]`; evaluating in a
missing session answers `status: ["error", "unknown-session", "done"]`. Every
op terminates with a `done` status.

`code` may contain several forms; they are evaluated in order and `value` is
the `write` representation of the last one. Anything the code prints to the
current output port is returned in `out`.

## Architecture

1. **Parse (pure)** — `lib/nrepl/bencode.scm` decodes stream chunks
   incrementally; `nrepl-message?` filters decoded dictionaries carrying an `op`.
2. **Route (pure-ish)** — `lib/nrepl/handlers.scm` maps a request plus the
   server context to a list of responses. Only session bookkeeping mutates
   state; no socket access.
3. **Effect** — `lib/nrepl/server.scm` encodes the responses and writes them to
   the socket, one thread per connection.

Because a bencoded message can be split across TCP packets, the server and
client buffer incoming bytes and use `bencode-decode-all`, which returns the
complete values plus the remainder.

Each session is a fresh anonymous module inheriting `gauche`, so bindings
persist per session and are isolated between sessions. `exit` is shadowed
inside sessions so evaluated code cannot take the server down; uncaught
errors and errors raised in threads spawned by evaluated code only affect the
request or thread in which they occur.

## Layout

```
lib/nrepl/bencode.scm    Bencode encoder/decoder (incremental)
lib/nrepl/handlers.scm   clone / eval / close / describe + routing
lib/nrepl/server.scm     TCP server, framing, socket writes
lib/nrepl/client.scm     Synchronous client API + interactive REPL
bin/nrepl-server         CLI entry point for the server
bin/nrepl-client         CLI entry point for the client
test/                    gauche.test suites (bencode, handlers, end-to-end)
```

## License

[MIT](LICENSE)
