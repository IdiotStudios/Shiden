use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub mod helpers {
    use std::collections::BTreeMap;

    pub fn get_network_helpers() -> BTreeMap<&'static str, Vec<u8>> {
        let mut m = BTreeMap::new();

        m.insert(
            "net_server_new",
            vec![
                0x55, 0x48, 0x89, 0xE5, 0x48, 0x83, 0xEC, 0x20, 0x53, 0x41, 0x54, 0x49, 0x89, 0xFC,
                0xBF, 0x02, 0x00, 0x00, 0x00, 0xBE, 0x01, 0x00, 0x00, 0x00, 0x31, 0xD2, 0xB8, 0x29,
                0x00, 0x00, 0x00, 0x0F, 0x05, 0x48, 0x89, 0xC3, 0xC7, 0x45, 0xE0, 0x02, 0x00, 0x00,
                0x00, 0x45, 0x89, 0xE0, 0x66, 0x41, 0xC1, 0xC0, 0x08, 0x66, 0x44, 0x89, 0x45, 0xE2,
                0xC7, 0x45, 0xE4, 0x00, 0x00, 0x00, 0x00, 0x48, 0x89, 0xDF, 0x48, 0x8D, 0x75, 0xE0,
                0xBA, 0x10, 0x00, 0x00, 0x00, 0xB8, 0x31, 0x00, 0x00, 0x00, 0x0F, 0x05, 0x48, 0x89,
                0xDF, 0xBE, 0x0A, 0x00, 0x00, 0x00, 0xB8, 0x32, 0x00, 0x00, 0x00, 0x0F, 0x05, 0x48,
                0x89, 0xD8, 0x41, 0x5C, 0x5B, 0x48, 0x89, 0xEC, 0x5D, 0xC3,
            ],
        );

        m.insert(
            "net_server_accept",
            vec![
                0x55, 0x48, 0x89, 0xE5, 0x31, 0xF6, 0x31, 0xD2, 0xB8, 0x2B, 0x00, 0x00, 0x00, 0x0F,
                0x05, 0x5D, 0xC3,
            ],
        );

        m.insert(
            "net_send",
            vec![
                0x55, 0x48, 0x89, 0xE5, 0x48, 0x81, 0xEC, 0x00, 0x10, 0x00, 0x00, 0x53, 0x41, 0x54,
                0x49, 0x89, 0xFC, 0x48, 0x8D, 0x9D, 0x00, 0xF0, 0xFF, 0xFF, 0x31, 0xC9, 0x48, 0x85,
                0xF6, 0x74, 0x18, 0x48, 0x8B, 0x76, 0x08, 0x48, 0x85, 0xF6, 0x74, 0x0F, 0x48, 0x8B,
                0x06, 0x88, 0x04, 0x0B, 0x48, 0x8B, 0x76, 0x08, 0x48, 0xFF, 0xC1, 0xEB, 0xED, 0x4C,
                0x89, 0xE7, 0x48, 0x8D, 0xB5, 0x00, 0xF0, 0xFF, 0xFF, 0x48, 0x89, 0xCA, 0xB8, 0x01,
                0x00, 0x00, 0x00, 0x0F, 0x05, 0x41, 0x5C, 0x5B, 0x48, 0x89, 0xEC, 0x5D, 0xC3,
            ],
        );

        m.insert(
            "net_send_byte",
            vec![
                0x55, 0x48, 0x89, 0xE5, 0x48, 0x83, 0xEC, 0x08, 0x40, 0x88, 0x34, 0x24, 0xB8, 0x01,
                0x00, 0x00, 0x00, 0x48, 0x8D, 0x34, 0x24, 0xBA, 0x01, 0x00, 0x00, 0x00, 0x0F, 0x05,
                0xC9, 0xC3,
            ],
        );

        m.insert(
            "net_send_raw",
            vec![0xB8, 0x01, 0x00, 0x00, 0x00, 0x0F, 0x05, 0xC3],
        );

        m.insert(
            "net_recv",
            vec![
                0x55, 0x48, 0x89, 0xE5, 0x48, 0x81, 0xEC, 0x00, 0x10, 0x00, 0x00, 0x48, 0x8D, 0xB5,
                0x00, 0xF0, 0xFF, 0xFF, 0xBA, 0x00, 0x10, 0x00, 0x00, 0xB8, 0x00, 0x00, 0x00, 0x00,
                0x0F, 0x05, 0x48, 0x83, 0xF8, 0x00, 0x7C, 0x0C, 0x48, 0x8D, 0x9D, 0x00, 0xF0, 0xFF,
                0xFF, 0xC6, 0x04, 0x03, 0x00, 0x48, 0x8D, 0x85, 0x00, 0xF0, 0xFF, 0xFF, 0x48, 0x89,
                0xEC, 0x5D, 0xC3, 0x31, 0xC0, 0x48, 0x89, 0xEC, 0x5D, 0xC3,
            ],
        );

        m.insert(
            "net_close",
            vec![0xB8, 0x03, 0x00, 0x00, 0x00, 0x0F, 0x05, 0xC3],
        );

        m
    }
}

pub fn http_get(url: &str) -> Result<String, String> {
    reqwest::blocking::get(url)
        .map_err(|e| format!("HTTP GET failed for '{}': {}", url, e))?
        .text()
        .map_err(|e| format!("Failed to read response body: {}", e))
}

pub fn http_post(url: &str, body: &str) -> Result<String, String> {
    let client = reqwest::blocking::Client::new();
    client
        .post(url)
        .body(body.to_string())
        .send()
        .map_err(|e| format!("HTTP POST failed for '{}': {}", url, e))?
        .text()
        .map_err(|e| format!("Failed to read response body: {}", e))
}

pub fn http_put(url: &str, body: &str) -> Result<String, String> {
    let client = reqwest::blocking::Client::new();
    client
        .put(url)
        .body(body.to_string())
        .send()
        .map_err(|e| format!("HTTP PUT failed for '{}': {}", url, e))?
        .text()
        .map_err(|e| format!("Failed to read response body: {}", e))
}

pub fn http_delete(url: &str) -> Result<String, String> {
    let client = reqwest::blocking::Client::new();
    client
        .delete(url)
        .send()
        .map_err(|e| format!("HTTP DELETE failed for '{}': {}", url, e))?
        .text()
        .map_err(|e| format!("Failed to read response body: {}", e))
}

lazy_static::lazy_static! {
    static ref HTTP_SERVERS: Arc<Mutex<HashMap<i64, HttpServerState>>> = Arc::new(Mutex::new(HashMap::new()));
    static ref NEXT_SERVER_ID: Arc<Mutex<i64>> = Arc::new(Mutex::new(1));
}

struct HttpServerState {
    port: u16,
    routes: HashMap<String, HashMap<String, String>>,
    _running: bool,
}

pub fn http_server_new(port: i64) -> i64 {
    let mut servers = HTTP_SERVERS.lock().unwrap();
    let mut next_id = NEXT_SERVER_ID.lock().unwrap();

    let server_id = *next_id;
    *next_id += 1;

    servers.insert(
        server_id,
        HttpServerState {
            port: port as u16,
            routes: HashMap::new(),
            _running: false,
        },
    );

    server_id
}

pub fn http_server_route(server_id: i64, path: &str, method: &str) -> Result<(), String> {
    let mut servers = HTTP_SERVERS.lock().unwrap();
    let server = servers
        .get_mut(&server_id)
        .ok_or_else(|| format!("Server {} not found", server_id))?;

    server
        .routes
        .entry(path.to_string())
        .or_insert_with(HashMap::new)
        .insert(
            method.to_uppercase(),
            format!("Handler for {} {}", method, path),
        );

    Ok(())
}

pub fn http_server_start(server_id: i64) -> Result<(), String> {
    let servers = HTTP_SERVERS.lock().unwrap();
    let server = servers
        .get(&server_id)
        .ok_or_else(|| format!("Server {} not found", server_id))?;

    let addr = format!("0.0.0.0:{}", server.port);
    let http_server = tiny_http::Server::http(&addr)
        .map_err(|e| format!("Failed to start HTTP server on {}: {}", addr, e))?;

    for request in http_server.incoming_requests() {
        let response = tiny_http::Response::from_string("Hello from Shiden HTTP Server");
        let _ = request.respond(response);
    }

    Ok(())
}

pub fn http_server_stop(_server_id: i64) -> Result<(), String> {
    Ok(())
}

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};

pub fn tcp_server_new(port: i64) -> Result<i64, String> {
    let addr = format!("0.0.0.0:{}", port);
    let _listener = TcpListener::bind(&addr)
        .map_err(|e| format!("Failed to bind TCP server to {}: {}", addr, e))?;

    Ok(port)
}

pub fn tcp_server_accept(_server_id: i64) -> Result<i64, String> {
    Ok(0)
}

pub fn tcp_connect(host: &str, port: i64) -> Result<i64, String> {
    let addr = format!("{}:{}", host, port);
    let _stream =
        TcpStream::connect(&addr).map_err(|e| format!("Failed to connect to {}: {}", addr, e))?;

    Ok(1)
}

pub fn tcp_send(_socket: i64, data: &str) -> Result<(), String> {
    Ok(())
}

pub fn tcp_recv(_socket: i64) -> Result<String, String> {
    Ok(String::new())
}

pub fn tcp_close(_socket: i64) -> Result<(), String> {
    Ok(())
}

use std::net::UdpSocket;

pub fn udp_socket_new(port: i64) -> Result<i64, String> {
    let addr = format!("0.0.0.0:{}", port);
    let _socket = UdpSocket::bind(&addr)
        .map_err(|e| format!("Failed to bind UDP socket to {}: {}", addr, e))?;

    Ok(port)
}

pub fn udp_send(_socket: i64, addr: &str, data: &str) -> Result<(), String> {
    let _ = (addr, data);
    Ok(())
}

pub fn udp_recv(_socket: i64) -> Result<String, String> {
    Ok(String::new())
}

pub fn udp_close(_socket: i64) -> Result<(), String> {
    Ok(())
}

pub fn websocket_connect(url: &str) -> Result<i64, String> {
    let _ = url;
    Ok(1)
}

pub fn websocket_send(_ws: i64, data: &str) -> Result<(), String> {
    let _ = data;
    Ok(())
}

pub fn websocket_close(_ws: i64) -> Result<(), String> {
    Ok(())
}

pub fn json_parse(data: &str) -> Result<String, String> {
    serde_json::from_str::<serde_json::Value>(data)
        .map(|v| v.to_string())
        .map_err(|e| format!("Failed to parse JSON: {}", e))
}

pub fn json_stringify(obj: &str) -> Result<String, String> {
    serde_json::from_str::<serde_json::Value>(obj)
        .map(|v| serde_json::to_string_pretty(&v).unwrap_or_else(|_| obj.to_string()))
        .map_err(|e| format!("Failed to stringify JSON: {}", e))
}

pub fn json_get(obj: &str, key: &str) -> Result<String, String> {
    let value: serde_json::Value =
        serde_json::from_str(obj).map_err(|e| format!("Failed to parse JSON: {}", e))?;

    value
        .get(key)
        .map(|v| v.to_string())
        .ok_or_else(|| format!("Key '{}' not found in JSON object", key))
}

pub fn json_set(obj: &str, key: &str, value: &str) -> Result<String, String> {
    let mut json: serde_json::Value =
        serde_json::from_str(obj).map_err(|e| format!("Failed to parse JSON: {}", e))?;

    let parsed_value: serde_json::Value = serde_json::from_str(value)
        .unwrap_or_else(|_| serde_json::Value::String(value.to_string()));

    if let Some(obj_map) = json.as_object_mut() {
        obj_map.insert(key.to_string(), parsed_value);
    }

    Ok(json.to_string())
}

use url::{Url, form_urlencoded};

pub fn url_parse(url_str: &str) -> Result<String, String> {
    let parsed =
        Url::parse(url_str).map_err(|e| format!("Failed to parse URL '{}': {}", url_str, e))?;

    let info = serde_json::json!({
        "scheme": parsed.scheme(),
        "host": parsed.host_str().unwrap_or(""),
        "port": parsed.port(),
        "path": parsed.path(),
        "query": parsed.query().unwrap_or(""),
        "fragment": parsed.fragment().unwrap_or("")
    });

    Ok(info.to_string())
}

pub fn url_encode(text: &str) -> String {
    form_urlencoded::byte_serialize(text.as_bytes()).collect()
}

pub fn url_decode(text: &str) -> Result<String, String> {
    let decoded = form_urlencoded::parse(text.as_bytes())
        .map(|(key, val)| {
            if key.is_empty() {
                val.to_string()
            } else {
                format!("{}={}", key, val)
            }
        })
        .collect::<Vec<_>>()
        .join("&");

    Ok(decoded)
}

use scraper::{Html, Selector};

pub fn scrape_page(url: &str) -> Result<String, String> {
    http_get(url)
}

pub fn scrape_extract_text(html: &str) -> Result<String, String> {
    let document = Html::parse_document(html);
    let mut text = String::new();

    for node in document.tree.nodes() {
        if let Some(text_node) = node.value().as_text() {
            text.push_str(text_node);
            text.push(' ');
        }
    }

    Ok(text.trim().to_string())
}

pub fn scrape_extract_links(html: &str) -> Result<String, String> {
    let document = Html::parse_document(html);
    let selector = Selector::parse("a[href]").map_err(|e| format!("Invalid selector: {:?}", e))?;

    let links: Vec<String> = document
        .select(&selector)
        .filter_map(|element| element.value().attr("href"))
        .map(|s| s.to_string())
        .collect();

    Ok(links.join("\n"))
}

pub fn scrape_select(html: &str, css_selector: &str) -> Result<String, String> {
    let document = Html::parse_document(html);
    let selector = Selector::parse(css_selector)
        .map_err(|e| format!("Invalid CSS selector '{}': {:?}", css_selector, e))?;

    let elements: Vec<String> = document
        .select(&selector)
        .map(|element| element.html())
        .collect();

    Ok(elements.join("\n"))
}

pub fn scrape_attr(html: &str, css_selector: &str, attr: &str) -> Result<String, String> {
    let document = Html::parse_document(html);
    let selector = Selector::parse(css_selector)
        .map_err(|e| format!("Invalid CSS selector '{}': {:?}", css_selector, e))?;

    let attrs: Vec<String> = document
        .select(&selector)
        .filter_map(|element| element.value().attr(attr))
        .map(|s| s.to_string())
        .collect();

    Ok(attrs.join("\n"))
}

use std::path::PathBuf;

pub fn file_serve_dir(dir_path: &str, port: i64) -> Result<(), String> {
    let addr = format!("0.0.0.0:{}", port);
    let server = tiny_http::Server::http(&addr)
        .map_err(|e| format!("Failed to start file server on {}: {}", addr, e))?;

    let base_path = PathBuf::from(dir_path);

    eprintln!(
        "Serving files from {} on http://localhost:{}",
        dir_path, port
    );

    for request in server.incoming_requests() {
        let url_path = request.url();
        let file_path = base_path.join(url_path.trim_start_matches('/'));

        if file_path.exists() && file_path.is_file() {
            match std::fs::read(&file_path) {
                Ok(contents) => {
                    let response = tiny_http::Response::from_data(contents);
                    let _ = request.respond(response);
                }
                Err(_) => {
                    let response =
                        tiny_http::Response::from_string("404 Not Found").with_status_code(404);
                    let _ = request.respond(response);
                }
            }
        } else {
            let response = tiny_http::Response::from_string("404 Not Found").with_status_code(404);
            let _ = request.respond(response);
        }
    }

    Ok(())
}
