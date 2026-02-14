# Networking Library

## Note:
- This library is partialy completed

### Incomplete features:

- TCP sockets (tcp_server_accept, tcp_send, tcp_recv, tcp_close just return placeholders)
- UDP sockets (udp_send, udp_recv, udp_close are empty stubs)
- WebSocket (all functions are stubs returning dummy values)
- HTTP server routing (routes are stored but not used; server only returns static response)
- Server stop functionality (does nothing)

### Completed features:

- HTTP client (GET, POST, PUT, DELETE)
- JSON operations (parse, stringify, get, set)
- URL operations (parse, encode, decode)
- Web scraping (page, extract_text, extract_links, select, attr)
- File serving
- Low-level assembly helpers 

## HTTP Client

```shiden
net http get(url)/str
net http post(url, body)/str
net http put(url, body)/str
net http delete(url)/str
```

Example:
```shiden
let response = net http get("https://api.example.com/data")/str
println("{}", response)/unit
```

## HTTP Server

```shiden
net server new(port)/i64           # Create server
net server route(server, path, method)/unit   # Register route
net server start(server)/unit      # Start listening
net server stop(server)/unit       # Stop server
```

Example:
```shiden
let server = net server new(3000)/i64
net server route(server, "/api/users", "GET")/unit
net server start(server)/unit
```

## TCP Sockets

```shiden
net tcp server new(port)/i64       # Create TCP server
net tcp server accept(server)/i64  # Accept connection
net tcp connect(host, port)/i64   # Connect to server
net tcp send(socket, data)/unit   # Send data
net tcp recv(socket)/str           # Receive data
net tcp close(socket)/unit         # Close socket
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
net udp socket new(port)/i64           # Create UDP socket
net udp send(socket, addr, data)/unit   # Send packet
net udp recv(socket)/str               # Receive packet
net udp close(socket)/unit             # Close socket
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
net websocket connect(url)/i64     # Connect to WebSocket
net websocket send(ws, data)/unit   # Send message
net websocket close(ws)/unit       # Close connection
```

## JSON Operations

```shiden
net json parse(data)/str           # Parse JSON
net json stringify(obj)/str        # Convert to JSON
net json get(obj, key)/str     # Get value by key
net json set(obj, key, value)/str   # Set value
```

## URL Operations

```shiden
net url parse(url)/str             # Parse URL
net url encode(str)/str            # URL encode string
net url decode(str)/str            # URL decode string
```

## Web Scraping

```shiden
net scrape page(url)/str           # Fetch webpage HTML
net scrape extract_text(html)/str  # Extract plain text
net scrape extract_links(html)/str # Extract all links
net scrape select(html, selector)/str  # CSS selector
net scrape attr(html, selector, attr)/str  # Extract attribute
```

Example for search engine crawler:
```shiden
let html = net scrape page("https://example.com")/str
let links = net scrape extract_links(html)/str
let text = net scrape extract_text(html)/str
```

## File Serving

```shiden
net file serve_dir(path, port)/unit   # Serve directory
```