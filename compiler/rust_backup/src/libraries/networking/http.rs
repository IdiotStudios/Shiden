use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct HttpResponse {
    pub status: u16,
    pub status_text: String,
    pub headers: HashMap<String, String>,
    pub body: String,
}

impl HttpResponse {
    pub fn is_ok(&self) -> bool {
        self.status >= 200 && self.status < 300
    }
}

pub fn http_get(url: &str) -> Result<HttpResponse, String> {
    let client = reqwest::blocking::Client::new();
    let response = client
        .get(url)
        .send()
        .map_err(|e| format!("failed to perform GET request: {}", e))?;

    response_to_struct(response)
}

pub fn http_post(
    url: &str,
    body: Option<&str>,
    headers: Option<&HashMap<String, String>>,
) -> Result<HttpResponse, String> {
    let client = reqwest::blocking::Client::new();
    let mut req = client.post(url);

    if let Some(body_str) = body {
        req = req.body(body_str.to_string());
    }

    if let Some(header_map) = headers {
        for (key, value) in header_map {
            req = req.header(key, value);
        }
    }

    let response = req
        .send()
        .map_err(|e| format!("failed to perform POST request: {}", e))?;

    response_to_struct(response)
}

pub fn http_put(
    url: &str,
    body: Option<&str>,
    headers: Option<&HashMap<String, String>>,
) -> Result<HttpResponse, String> {
    let client = reqwest::blocking::Client::new();
    let mut req = client.put(url);

    if let Some(body_str) = body {
        req = req.body(body_str.to_string());
    }

    if let Some(header_map) = headers {
        for (key, value) in header_map {
            req = req.header(key, value);
        }
    }

    let response = req
        .send()
        .map_err(|e| format!("failed to perform PUT request: {}", e))?;

    response_to_struct(response)
}

pub fn http_patch(
    url: &str,
    body: Option<&str>,
    headers: Option<&HashMap<String, String>>,
) -> Result<HttpResponse, String> {
    let client = reqwest::blocking::Client::new();
    let mut req = client.patch(url);

    if let Some(body_str) = body {
        req = req.body(body_str.to_string());
    }

    if let Some(header_map) = headers {
        for (key, value) in header_map {
            req = req.header(key, value);
        }
    }

    let response = req
        .send()
        .map_err(|e| format!("failed to perform PATCH request: {}", e))?;

    response_to_struct(response)
}

pub fn http_delete(
    url: &str,
    headers: Option<&HashMap<String, String>>,
) -> Result<HttpResponse, String> {
    let client = reqwest::blocking::Client::new();
    let mut req = client.delete(url);

    if let Some(header_map) = headers {
        for (key, value) in header_map {
            req = req.header(key, value);
        }
    }

    let response = req
        .send()
        .map_err(|e| format!("failed to perform DELETE request: {}", e))?;

    response_to_struct(response)
}

fn response_to_struct(response: reqwest::blocking::Response) -> Result<HttpResponse, String> {
    let status = response.status();
    let status_code = status.as_u16();
    let status_text = status.canonical_reason().unwrap_or("Unknown").to_string();

    let mut headers = HashMap::new();
    for (key, value) in response.headers().iter() {
        let key_str = key.to_string();
        let value_str = value.to_str().unwrap_or("").to_string();
        headers.insert(key_str, value_str);
    }

    let body = response
        .text()
        .map_err(|e| format!("failed to read response body: {}", e))?;

    Ok(HttpResponse {
        status: status_code,
        status_text,
        headers,
        body,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::thread;

    fn start_test_server(response: &'static str) -> u16 {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind test server");
        let port = listener.local_addr().expect("get port").port();

        thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                let mut buf = [0u8; 1024];
                let _ = stream.read(&mut buf);
                let http_response = format!(
                    "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
                    response.len(),
                    response
                );
                let _ = stream.write_all(http_response.as_bytes());
            }
        });

        port
    }

    #[test]
    fn test_http_get() {
        let port = start_test_server("test response");
        let url = format!("http://127.0.0.1:{}/", port);

        let response = http_get(&url).expect("http_get should succeed");
        assert_eq!(response.status, 200);
        assert_eq!(response.body, "test response");
        assert!(response.is_ok());
    }

    #[test]
    fn test_http_response_is_ok() {
        let response = HttpResponse {
            status: 200,
            status_text: "OK".to_string(),
            headers: HashMap::new(),
            body: "test".to_string(),
        };
        assert!(response.is_ok());

        let response = HttpResponse {
            status: 404,
            status_text: "Not Found".to_string(),
            headers: HashMap::new(),
            body: "error".to_string(),
        };
        assert!(!response.is_ok());
    }
}
