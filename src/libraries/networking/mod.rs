#![allow(dead_code)]

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Mutex, OnceLock};
use std::time::Duration;

use crate::libraries::helper_defs::{HelperDef, lower_helper_defs};

pub mod http;

static LAST_HTTP_STATUS: AtomicI32 = AtomicI32::new(0);
static NEXT_TCP_FD: AtomicI32 = AtomicI32::new(1);
static TCP_CONNECTIONS: OnceLock<Mutex<HashMap<i32, TcpStream>>> = OnceLock::new();

fn tcp_registry() -> &'static Mutex<HashMap<i32, TcpStream>> {
    TCP_CONNECTIONS.get_or_init(|| Mutex::new(HashMap::new()))
}

pub mod helpers {
    use super::*;

    fn gen_http_get(_target: &str) -> Vec<u8> {
        ret_i64(0)
    }

    fn gen_http_post(_target: &str) -> Vec<u8> {
        ret_i64(0)
    }

    fn gen_http_get_json(_target: &str) -> Vec<u8> {
        ret_i64(0)
    }

    fn gen_http_post_json(_target: &str) -> Vec<u8> {
        ret_i64(0)
    }

    fn gen_tcp_connect(_target: &str) -> Vec<u8> {
        ret_i64_neg()
    }

    fn gen_tcp_send(_target: &str) -> Vec<u8> {
        ret_i64_neg()
    }

    fn gen_tcp_recv(_target: &str) -> Vec<u8> {
        ret_i64_neg()
    }

    fn gen_tcp_close(_target: &str) -> Vec<u8> {
        ret_i32_neg()
    }

    pub fn get_http_helper_defs() -> BTreeMap<&'static str, HelperDef> {
        let mut m = BTreeMap::new();

        m.insert("http_get", HelperDef::generated(gen_http_get));
        m.insert("http_post", HelperDef::generated(gen_http_post));
        m.insert("http_get_json", HelperDef::generated(gen_http_get_json));
        m.insert("http_post_json", HelperDef::generated(gen_http_post_json));
        m.insert("http_last_status", HelperDef::bytecode(ret_i32(200)));

        m.insert("tcp_connect", HelperDef::generated(gen_tcp_connect));
        m.insert("tcp_send", HelperDef::generated(gen_tcp_send));
        m.insert("tcp_recv", HelperDef::generated(gen_tcp_recv));
        m.insert("tcp_close", HelperDef::generated(gen_tcp_close));

        m
    }

    pub fn get_http_helpers(target: &str) -> BTreeMap<&'static str, Vec<u8>> {
        let defs = get_http_helper_defs();
        lower_helper_defs(&defs, target)
    }
}

pub fn perform_http_get(url: &str) -> i32 {
    match http_get(url) {
        Ok(_body) => LAST_HTTP_STATUS.load(Ordering::Relaxed),
        Err(_e) => -1,
    }
}

pub fn perform_http_post(url: &str, body: &str) -> i32 {
    match http_post(url, body) {
        Ok(_response) => LAST_HTTP_STATUS.load(Ordering::Relaxed),
        Err(_e) => -1,
    }
}

pub fn get_http_status() -> i32 {
    LAST_HTTP_STATUS.load(Ordering::Relaxed)
}

fn ret_i64(value: i64) -> Vec<u8> {
    let bytes = value.to_le_bytes();
    vec![
        0x55, 0x48, 0x89, 0xE5, 0x48, 0xB8, bytes[0], bytes[1], bytes[2], bytes[3], bytes[4],
        bytes[5], bytes[6], bytes[7], 0x5D, 0xC3,
    ]
}

fn ret_i64_neg() -> Vec<u8> {
    vec![
        0x55, 0x48, 0x89, 0xE5, 0x48, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF, 0x5D, 0xC3,
    ]
}

fn ret_i32(value: i32) -> Vec<u8> {
    let bytes = value.to_le_bytes();
    vec![
        0x55, 0x48, 0x89, 0xE5, 0xB8, bytes[0], bytes[1], bytes[2], bytes[3], 0x5D, 0xC3,
    ]
}

fn ret_i32_neg() -> Vec<u8> {
    vec![
        0x55, 0x48, 0x89, 0xE5, 0xB8, 0xFF, 0xFF, 0xFF, 0xFF, 0x5D, 0xC3,
    ]
}

fn parse_url(url: &str) -> Result<(String, u16, String, bool), String> {
    let url = url.trim();

    let (scheme, rest) = if url.starts_with("https://") {
        ("https", &url[8..])
    } else if url.starts_with("http://") {
        ("http", &url[7..])
    } else {
        return Err("URL must start with http:// or https://".to_string());
    };

    let is_https = scheme == "https";

    let (host_port, path) = if let Some(idx) = rest.find('/') {
        (&rest[..idx], &rest[idx..])
    } else {
        (rest, "/")
    };

    let (host, port) = if let Some(idx) = host_port.find(':') {
        let h = host_port[..idx].to_string();
        let p = host_port[idx + 1..]
            .parse::<u16>()
            .map_err(|_| "Invalid port number")?;
        (h, p)
    } else {
        (host_port.to_string(), if is_https { 443 } else { 80 })
    };

    Ok((host, port, path.to_string(), is_https))
}

fn parse_http_response(response: &[u8]) -> Result<(i32, String), String> {
    let response_str = String::from_utf8_lossy(response).to_string();

    let mut status = 0;
    if let Some(status_line) = response_str.lines().next()
        && let Some(code_str) = status_line.split_whitespace().nth(1)
        && let Ok(code) = code_str.parse::<i32>()
    {
        status = code;
    }

    let body = if let Some(body_start) = response_str.find("\r\n\r\n") {
        response_str[body_start + 4..].to_string()
    } else {
        response_str
    };

    Ok((status, body))
}

fn connect_http(host: &str, port: u16) -> Result<TcpStream, String> {
    let addr = format!("{}:{}", host, port);
    let mut addrs = addr
        .to_socket_addrs()
        .map_err(|e| format!("DNS resolution failed: {}", e))?;
    let sockaddr = addrs.next().ok_or("No addresses found")?;

    let stream = TcpStream::connect_timeout(&sockaddr, Duration::from_secs(10))
        .map_err(|e| format!("Connection failed: {}", e))?;
    stream
        .set_read_timeout(Some(Duration::from_secs(30)))
        .map_err(|e| format!("Failed to set read timeout: {}", e))?;
    stream
        .set_write_timeout(Some(Duration::from_secs(30)))
        .map_err(|e| format!("Failed to set write timeout: {}", e))?;

    Ok(stream)
}

pub fn http_get(url: &str) -> Result<String, String> {
    let (host, port, path, is_https) = parse_url(url)?;

    if is_https {
        return Err("HTTPS not yet supported (TLS not implemented)".to_string());
    }

    let mut stream = connect_http(&host, port)?;

    let request = format!(
        "GET {} HTTP/1.1\r\nHost: {}\r\nConnection: close\r\nUser-Agent: Shiden-HTTP/1.0\r\n\r\n",
        path, host
    );

    stream
        .write_all(request.as_bytes())
        .map_err(|e| format!("Failed to send request: {}", e))?;

    let mut response = Vec::new();
    stream
        .read_to_end(&mut response)
        .map_err(|e| format!("Failed to read response: {}", e))?;

    let (status, body) = parse_http_response(&response)?;
    LAST_HTTP_STATUS.store(status, Ordering::Relaxed);
    Ok(body)
}

pub fn http_post(url: &str, body: &str) -> Result<String, String> {
    let (host, port, path, is_https) = parse_url(url)?;

    if is_https {
        return Err("HTTPS not yet supported (TLS not implemented)".to_string());
    }

    let mut stream = connect_http(&host, port)?;

    let request = format!(
        "POST {} HTTP/1.1\r\nHost: {}\r\nConnection: close\r\nUser-Agent: Shiden-HTTP/1.0\r\nContent-Length: {}\r\nContent-Type: text/plain\r\n\r\n{}",
        path,
        host,
        body.len(),
        body
    );

    stream
        .write_all(request.as_bytes())
        .map_err(|e| format!("Failed to send request: {}", e))?;

    let mut response = Vec::new();
    stream
        .read_to_end(&mut response)
        .map_err(|e| format!("Failed to read response: {}", e))?;

    let (status, body) = parse_http_response(&response)?;
    LAST_HTTP_STATUS.store(status, Ordering::Relaxed);
    Ok(body)
}

pub fn http_get_json(url: &str) -> Result<serde_json::Value, String> {
    let body = http_get(url)?;
    serde_json::from_str(&body).map_err(|e| format!("Failed to parse JSON: {}", e))
}

pub fn http_post_json(url: &str, json: &serde_json::Value) -> Result<serde_json::Value, String> {
    let (host, port, path, is_https) = parse_url(url)?;

    if is_https {
        return Err("HTTPS not yet supported (TLS not implemented)".to_string());
    }

    let body =
        serde_json::to_string(json).map_err(|e| format!("Failed to serialize JSON: {}", e))?;

    let mut stream = connect_http(&host, port)?;

    let request = format!(
        "POST {} HTTP/1.1\r\nHost: {}\r\nConnection: close\r\nUser-Agent: Shiden-HTTP/1.0\r\nContent-Length: {}\r\nContent-Type: application/json\r\n\r\n{}",
        path,
        host,
        body.len(),
        body
    );

    stream
        .write_all(request.as_bytes())
        .map_err(|e| format!("Failed to send request: {}", e))?;

    let mut response = Vec::new();
    stream
        .read_to_end(&mut response)
        .map_err(|e| format!("Failed to read response: {}", e))?;

    let (status, body) = parse_http_response(&response)?;
    LAST_HTTP_STATUS.store(status, Ordering::Relaxed);
    serde_json::from_str(&body).map_err(|e| format!("Failed to parse JSON response: {}", e))
}

pub fn http_last_status() -> i32 {
    LAST_HTTP_STATUS.load(Ordering::Relaxed)
}

pub fn tcp_connect(host: &str, port: u16) -> Result<i32, String> {
    let addr = format!("{}:{}", host, port);
    let mut addrs = addr
        .to_socket_addrs()
        .map_err(|e| format!("DNS resolution failed: {}", e))?;
    let sockaddr = addrs.next().ok_or("No addresses found")?;

    let stream = TcpStream::connect_timeout(&sockaddr, Duration::from_secs(10))
        .map_err(|e| format!("Connection failed: {}", e))?;
    stream
        .set_read_timeout(Some(Duration::from_secs(30)))
        .map_err(|e| format!("Failed to set read timeout: {}", e))?;
    stream
        .set_write_timeout(Some(Duration::from_secs(30)))
        .map_err(|e| format!("Failed to set write timeout: {}", e))?;

    let fd = NEXT_TCP_FD.fetch_add(1, Ordering::Relaxed);
    let registry = tcp_registry();
    let mut guard = registry
        .lock()
        .map_err(|_| "TCP registry lock poisoned".to_string())?;
    guard.insert(fd, stream);
    Ok(fd)
}

pub fn tcp_send(fd: i32, data: &[u8]) -> Result<usize, String> {
    let registry = tcp_registry();
    let mut guard = registry
        .lock()
        .map_err(|_| "TCP registry lock poisoned".to_string())?;
    let stream = guard
        .get_mut(&fd)
        .ok_or_else(|| format!("Invalid TCP handle: {}", fd))?;

    stream
        .write_all(data)
        .map_err(|e| format!("Failed to send on {}: {}", fd, e))?;
    Ok(data.len())
}

pub fn tcp_recv(fd: i32, buffer: &mut [u8]) -> Result<usize, String> {
    let registry = tcp_registry();
    let mut guard = registry
        .lock()
        .map_err(|_| "TCP registry lock poisoned".to_string())?;
    let stream = guard
        .get_mut(&fd)
        .ok_or_else(|| format!("Invalid TCP handle: {}", fd))?;

    stream
        .read(buffer)
        .map_err(|e| format!("Failed to recv on {}: {}", fd, e))
}

pub fn tcp_close(fd: i32) -> Result<(), String> {
    let registry = tcp_registry();
    let mut guard = registry
        .lock()
        .map_err(|_| "TCP registry lock poisoned".to_string())?;

    guard
        .remove(&fd)
        .map(|_| ())
        .ok_or_else(|| format!("Invalid TCP handle: {}", fd))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::TcpListener;
    use std::thread;

    fn start_http_server(response_body: &'static str) -> u16 {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind test http server");
        let port = listener.local_addr().expect("local addr").port();

        thread::spawn(move || {
            let (mut stream, _) = listener.accept().expect("accept http client");
            let mut buf = [0u8; 2048];
            let _ = stream.read(&mut buf);

            let body = response_body.as_bytes();
            let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
                body.len(),
                response_body
            );
            stream
                .write_all(response.as_bytes())
                .expect("write http response");
        });

        port
    }

    #[test]
    fn http_get_reads_response_and_status() {
        let port = start_http_server("hello-http");
        let url = format!("http://127.0.0.1:{}/test", port);

        let body = http_get(&url).expect("http_get should succeed");
        assert_eq!(body, "hello-http");
        assert_eq!(http_last_status(), 200);
    }

    #[test]
    fn http_post_reads_response_and_status() {
        let port = start_http_server("posted-ok");
        let url = format!("http://127.0.0.1:{}/submit", port);

        let body = http_post(&url, "payload").expect("http_post should succeed");
        assert_eq!(body, "posted-ok");
        assert_eq!(http_last_status(), 200);
    }

    #[test]
    fn tcp_connect_send_recv_close_roundtrip() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind tcp test server");
        let port = listener.local_addr().expect("local addr").port();

        thread::spawn(move || {
            let (mut stream, _) = listener.accept().expect("accept tcp client");

            let mut recv_buf = [0u8; 64];
            let n = stream.read(&mut recv_buf).expect("server read");
            assert_eq!(&recv_buf[..n], b"ping");

            stream.write_all(b"pong").expect("server write");
        });

        let fd = tcp_connect("127.0.0.1", port).expect("tcp_connect should succeed");
        let sent = tcp_send(fd, b"ping").expect("tcp_send should succeed");
        assert_eq!(sent, 4);

        let mut buf = [0u8; 16];
        let n = tcp_recv(fd, &mut buf).expect("tcp_recv should succeed");
        assert_eq!(&buf[..n], b"pong");

        tcp_close(fd).expect("tcp_close should succeed");
    }
}
