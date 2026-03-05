# Shiden Networking Library

## Current Status

**HTTP Operations - IMPLEMENTED**
- HTTP GET requests (http:// only, no TLS yet)
- HTTP POST requests with custom body
- JSON GET and POST operations  
- HTTP status code tracking
- URL parsing and connection management

**TCP Operations - PARTIAL**
- Basic TCP connect functionality exists
- File descriptor management not yet implemented
- Low-level send/recv operations pending

**Not Yet Implemented**
- HTTPS/TLS support (requires TLS library or implementation)
- Advanced HTTP features (redirects, cookies, auth)
- TCP file descriptor registry for persistent connections
- Chunked transfer encoding
- HTTP/2 or HTTP/3

## Available Functions

### HTTP Operations

- `http_get(url: &str) -> Result<String, String>` - Make HTTP GET request, returns response body
- `http_post(url: &str, body: &str) -> Result<String, String>` - Make HTTP POST request
- `http_get_json(url: &str) -> Result<Value, String>` - Make HTTP GET, parse JSON response
- `http_post_json(url: &str, json: &Value) -> Result<Value, String>` - POST JSON data
- `http_last_status() -> i32` - Get HTTP status code of last request

### TCP Operations

- `tcp_connect(host: &str, port: u16) -> Result<i32, String>` - Connect to TCP server (not usable yet)
- `tcp_send(fd: i32, data: &[u8]) -> Result<usize, String>` - Send data (pending)
- `tcp_recv(fd: i32, buffer: &mut [u8]) -> Result<usize, String>` - Receive data (pending)
- `tcp_close(fd: i32) -> Result<(), String>` - Close connection (pending)

## Implementation Notes

**HTTP Implementation**: Uses Rust's `std::net::TcpStream` for raw socket connections. Constructs HTTP/1.1 requests manually and parses responses. This provides cross-platform functionality without external dependencies like reqwest.

**Limitations**:
- Only HTTP supported (no HTTPS/TLS yet)
- 10 second connection timeout, 30 second read timeout
- No redirect following
- No compression support
- Basic HTTP/1.1 protocol only

## Example Usage

See `examples/http/` for a working example