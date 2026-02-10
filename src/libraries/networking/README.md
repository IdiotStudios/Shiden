# Networking Library

## HTTP Client

```shiden
net http get(url/str)/str
net http post(url/str, body/str)/str
net http put(url/str, body/str)/str
net http delete(url/str)/str
```

Example:
```shiden
let response = net http get("https://api.example.com/data")/str
println("{}", response)/unit
```

## HTTP Server

```shiden
net server new(port/i64)/i64           # Create server
net server route(server/i64, path/str, method/str)/unit   # Register route
net server start(server/i64)/unit      # Start listening
net server stop(server/i64)/unit       # Stop server
```

Example:
```shiden
let server = net server new(3000)/i64
net server route(server, "/api/users", "GET")/unit
net server start(server)/unit
```

## TCP Sockets

```shiden
net tcp server new(port/i64)/i64       # Create TCP server
net tcp server accept(server/i64)/i64  # Accept connection
net tcp connect(host/str, port/i64)/i64   # Connect to server
net tcp send(socket/i64, data/str)/unit   # Send data
net tcp recv(socket/i64)/str           # Receive data
net tcp close(socket/i64)/unit         # Close socket
```

Example:
```shiden
let socket = net tcp connect("127.0.0.1", 8080)/i64
net tcp send(socket, "Hello")/unit
let response = net tcp recv(socket)/str
net tcp close(socket)/unit
```

## UDP Sockets

```shiden
net udp socket new(port/i64)/i64           # Create UDP socket
net udp send(socket/i64, addr/str, data/str)/unit   # Send packet
net udp recv(socket/i64)/str               # Receive packet
net udp close(socket/i64)/unit             # Close socket
```

Example:
```shiden
let socket = net udp socket new(5000)/i64
net udp send(socket, "192.168.1.100:5001", "Hello UDP")/unit
let data = net udp recv(socket)/str
net udp close(socket)/unit
```

## WebSocket

```shiden
net websocket connect(url/str)/i64     # Connect to WebSocket
net websocket send(ws/i64, data/str)/unit   # Send message
net websocket close(ws/i64)/unit       # Close connection
```

## JSON Operations

```shiden
net json parse(data/str)/str           # Parse JSON
net json stringify(obj/str)/str        # Convert to JSON
net json get(obj/str, key/str)/str     # Get value by key
net json set(obj/str, key/str, value/str)/str   # Set value
```

## URL Operations

```shiden
net url parse(url/str)/str             # Parse URL
net url encode(str/str)/str            # URL encode string
net url decode(str/str)/str            # URL decode string
```

## Web Scraping

```shiden
net scrape page(url/str)/str           # Fetch webpage HTML
net scrape extract_text(html/str)/str  # Extract plain text
net scrape extract_links(html/str)/str # Extract all links
net scrape select(html/str, selector/str)/str  # CSS selector
net scrape attr(html/str, selector/str, attr/str)/str  # Extract attribute
```

Example for search engine crawler:
```shiden
let html = net scrape page("https://example.com")/str
let links = net scrape extract_links(html)/str
let text = net scrape extract_text(html)/str
```

## File Serving

```shiden
net file serve_dir(path/str, port/i64)/unit   # Serve directory
```