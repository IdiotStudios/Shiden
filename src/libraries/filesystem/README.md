# filesystem library

## Usage:

This library provides convenience wrappers around built-in file operations and a JSON-edit helper.

Reading a file:
```shiden
let env = fs read("./.env/")/str
println("{}", env)/unit
```

Writing a file (overwrites):
```shiden
fs write("example.json", "{\"example\": true}")/unit
```

Editing a JSON file (sets top-level key):
```shiden
fs edit("example.json", "example", false)/unit
```

Advanced edits and merging strategies:

- Replace (default): `fs_edit(path, key, value)` — overwrites the key
- Merge (objects): `fs_edit(path, key, value, "merge")` — deep-merges objects
- Append (arrays): `fs_edit(path, key, value, "append")` — appends to an array (if `value` is an array it appends all elements)
- Unique (arrays): `fs_edit(path, key, value, "unique")` — appends items only if not already present

Other helpers:
```shiden
fs delete("example.json", "example")/unit      # remove top-level key
fs array-append("example.json", "arr", 3)/unit # append single value to array
fs merge("example.json", "{\"a\": {\"b\": 2}}")/unit # deep-merge JSON string into file
```

Note: `fs edit` and friends modify the file as JSON (uses serde_json). If the file is missing or invalid JSON it will be created/overwritten with a JSON object containing the changes.
