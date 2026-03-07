# Filesystem Library

## Basic File Operations

Read a file:
```shiden
let content = fs read("./.env")/str
println("{}", content)/unit
```

Write a file (overwrites):
```shiden
fs write("output.txt", "Hello, World!")/unit
```

Check if a file exists:
```shiden
if eq(fs exists("config.txt"), 1)
    println("File exists")/unit
fn/
```

Delete a file:
```shiden
fs delete_file("old_file.txt")/unit
```

Get file size in bytes:
```shiden
let size = fs file_size("data.bin")/i64
println("Size: {} bytes", size)/unit
```

## Directory Operations

Create a directory (recursively):
```shiden
fs mkdir("path/to/new/directory")/unit
```

List directory contents:
```shiden
let entries = fs list_dir("./")/str
println("Files: {}", entries)/unit
```

## JSON File Operations

Edit a JSON file (sets top-level key):
```shiden
fs edit("config.json", "theme", "dark")/unit
```

Edit with strategy:
- Replace (default): `fs edit(path, key, value)` — overwrites the key
- Merge: `fs edit_strategy(path, key, value, "merge")` — deep-merges objects
- Append: `fs edit_strategy(path, key, value, "append")` — appends to arrays
- Unique: `fs edit_strategy(path, key, value, "unique")` — appends only if not present

Example with merge strategy:
```shiden
fs edit_strategy("config.json", "database", "{\"host\": \"localhost\"}", "merge")/unit
```

Delete a top-level key from JSON file:
```shiden
fs delete("config.json", "deprecated_setting")/unit
```

Append value to array at key:
```shiden
fs array_append("data.json", "items", "new_item")/unit
```

Deep merge JSON string into file:
```shiden
fs merge("config.json", "{\"a\": {\"b\": 2}}")/unit
```

## Complete Example

```shiden
# Create config directory
fs mkdir("./config")/unit

# Initialize config file with JSON
fs write("config/app.json", "{\"version\": \"1.0.0\", \"debug\": false}")/unit

# Add settings using edit
fs edit("config/app.json", "port", "3000")/unit

# Merge additional config
fs merge("config/app.json", "{\"database\": {\"host\": \"localhost\", \"port\": 5432}}")/unit

# Read the final config
let config = fs read("config/app.json")/str
println("Config: {}", config)/unit
```