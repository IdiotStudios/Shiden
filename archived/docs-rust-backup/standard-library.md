# Shiden — Standard Library

Shiden ships with `fs` and `net` helper namespaces.

## Filesystem

Common helpers include `fs read`, `fs write`, `fs exists`, `fs mkdir`, `fs list_dir`.
These are wrappers over built-in runtime helpers with stable names.

```shiden
fn new main/
    let data = fs read("/tmp/a")/str
    fs write("/tmp/b", data)/
fn/
```

## Networking

Helpers include `net http get`, `net server new`, `net tcp connect`, `net websocket connect`.
Networking APIs are synchronous and return `str` or `i64` handles.

```shiden
fn new main/
    let body = net http get("https://example.com")/str
    println("{}", body)/
fn/
```
