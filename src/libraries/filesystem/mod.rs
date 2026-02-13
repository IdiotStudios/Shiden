use std::fs;
use std::path::Path;

pub mod helpers {
    use std::collections::BTreeMap;

    pub fn get_fs_helpers() -> BTreeMap<&'static str, Vec<u8>> {
        let mut m = BTreeMap::new();

        m.insert(
            "fs_read",
            vec![
                0x55, 0x48, 0x89, 0xE5, 0x48, 0x81, 0xEC, 0x00, 0x01, 0x00, 0x00, 0x53, 0x41, 0x54,
                0x41, 0x55, 0x49, 0x89, 0xF4, 0x49, 0x89, 0xD5, 0x48, 0x8D, 0x9D, 0x00, 0xFF, 0xFF,
                0xFF, 0x31, 0xC9, 0x48, 0x85, 0xFF, 0x74, 0x18, 0x48, 0x8B, 0x7F, 0x08, 0x48, 0x85,
                0xFF, 0x74, 0x0F, 0x48, 0x8B, 0x07, 0x88, 0x04, 0x0B, 0x48, 0x8B, 0x7F, 0x08, 0x48,
                0xFF, 0xC1, 0xEB, 0xED, 0xC6, 0x04, 0x0B, 0x00, 0x48, 0x89, 0xDF, 0x31, 0xF6, 0xB8,
                0x02, 0x00, 0x00, 0x00, 0x0F, 0x05, 0x48, 0x89, 0xC3, 0x48, 0x83, 0xFB, 0x00, 0x7C,
                0x22, 0x48, 0x89, 0xDF, 0x4C, 0x89, 0xE6, 0x4C, 0x89, 0xEA, 0xB8, 0x00, 0x00, 0x00,
                0x00, 0x0F, 0x05, 0x49, 0x89, 0xC4, 0x48, 0x89, 0xDF, 0xB8, 0x03, 0x00, 0x00, 0x00,
                0x0F, 0x05, 0x4C, 0x89, 0xE0, 0xEB, 0x07, 0x48, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF,
                0x41, 0x5D, 0x41, 0x5C, 0x5B, 0x48, 0x89, 0xEC, 0x5D, 0xC3,
            ],
        );

        m
    }
}

fn cstr_to_string(ptr: usize, memory: &[u8]) -> Result<String, String> {
    if ptr == 0 {
        return Err("Null pointer".to_string());
    }

    let mut result = String::new();
    let mut i = ptr;
    while i < memory.len() && memory[i] != 0 {
        result.push(memory[i] as char);
        i += 1;
    }
    Ok(result)
}

pub fn allocate_string(content: &str) -> (usize, Vec<u8>) {
    let bytes = content.as_bytes().to_vec();
    let len = bytes.len();
    (len, bytes)
}

pub fn fs_read(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("Failed to read file '{}': {}", path, e))
}

pub fn fs_write(path: &str, content: &str) -> Result<(), String> {
    fs::write(path, content).map_err(|e| format!("Failed to write file '{}': {}", path, e))
}

pub fn fs_exists(path: &str) -> i64 {
    if Path::new(path).exists() { 1 } else { 0 }
}

pub fn fs_delete_file(path: &str) -> Result<(), String> {
    fs::remove_file(path).map_err(|e| format!("Failed to delete file '{}': {}", path, e))
}

pub fn fs_file_size(path: &str) -> Result<i64, String> {
    fs::metadata(path)
        .map(|m| m.len() as i64)
        .map_err(|e| format!("Failed to get file size '{}': {}", path, e))
}

pub fn fs_mkdir(path: &str) -> Result<(), String> {
    fs::create_dir_all(path).map_err(|e| format!("Failed to create directory '{}': {}", path, e))
}

pub fn fs_list_dir(path: &str) -> Result<String, String> {
    let entries =
        fs::read_dir(path).map_err(|e| format!("Failed to list directory '{}': {}", path, e))?;

    let mut files = Vec::new();
    for entry in entries {
        match entry {
            Ok(e) => {
                if let Ok(name) = e.file_name().into_string() {
                    files.push(name);
                }
            }
            Err(_) => continue,
        }
    }

    Ok(files.join("\n"))
}

pub fn fs_edit(path: &str, key: &str, value: &str) -> Result<(), String> {
    fs_edit_strategy(path, key, value, "replace")
}

pub fn fs_edit_strategy(path: &str, key: &str, value: &str, strategy: &str) -> Result<(), String> {
    let content = fs::read_to_string(path).unwrap_or_else(|_| "{}".to_string());

    let mut json: serde_json::Value =
        serde_json::from_str(&content).map_err(|e| format!("Invalid JSON in '{}': {}", path, e))?;

    let parsed_value: serde_json::Value = serde_json::from_str(value)
        .unwrap_or_else(|_| serde_json::Value::String(value.to_string()));

    match strategy {
        "replace" => {
            json[key] = parsed_value;
        }
        "merge" => {
            if let (Some(obj), serde_json::Value::Object(val_map)) =
                (json.as_object_mut(), &parsed_value)
            {
                for (k, v) in val_map {
                    obj.insert(k.clone(), v.clone());
                }
            }
        }
        "append" => {
            if !json[key].is_array() {
                json[key] = serde_json::json!([]);
            }
            json[key].as_array_mut().unwrap().push(parsed_value);
        }
        "unique" => {
            if !json[key].is_array() {
                json[key] = serde_json::json!([]);
            }
            let arr = json[key].as_array_mut().unwrap();
            if !arr.contains(&parsed_value) {
                arr.push(parsed_value);
            }
        }
        _ => return Err(format!("Unknown strategy: {}", strategy)),
    }

    fs::write(path, json.to_string())
        .map_err(|e| format!("Failed to write JSON file '{}': {}", path, e))
}

pub fn fs_delete(path: &str, key: &str) -> Result<(), String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read '{}': {}", path, e))?;

    let mut json: serde_json::Value =
        serde_json::from_str(&content).map_err(|e| format!("Invalid JSON in '{}': {}", path, e))?;

    if let Some(obj) = json.as_object_mut() {
        obj.remove(key);
    }

    fs::write(path, json.to_string())
        .map_err(|e| format!("Failed to write JSON file '{}': {}", path, e))
}

pub fn fs_array_append(path: &str, key: &str, value: &str) -> Result<(), String> {
    let content = fs::read_to_string(path).unwrap_or_else(|_| "{}".to_string());

    let mut json: serde_json::Value =
        serde_json::from_str(&content).map_err(|e| format!("Invalid JSON in '{}': {}", path, e))?;

    if !json[key].is_array() {
        json[key] = serde_json::json!([]);
    }

    let parsed_value: serde_json::Value = serde_json::from_str(value)
        .unwrap_or_else(|_| serde_json::Value::String(value.to_string()));

    json[key].as_array_mut().unwrap().push(parsed_value);

    fs::write(path, json.to_string())
        .map_err(|e| format!("Failed to write JSON file '{}': {}", path, e))
}

pub fn fs_merge(path: &str, json_str: &str) -> Result<(), String> {
    let file_content = fs::read_to_string(path).unwrap_or_else(|_| "{}".to_string());

    let mut file_json: serde_json::Value = serde_json::from_str(&file_content)
        .map_err(|e| format!("Invalid JSON in '{}': {}", path, e))?;

    let merge_json: serde_json::Value =
        serde_json::from_str(json_str).map_err(|e| format!("Invalid JSON to merge: {}", e))?;

    deep_merge(&mut file_json, merge_json);

    fs::write(path, file_json.to_string())
        .map_err(|e| format!("Failed to write JSON file '{}': {}", path, e))
}

fn deep_merge(target: &mut serde_json::Value, source: serde_json::Value) {
    match (target, source) {
        (serde_json::Value::Object(target_map), serde_json::Value::Object(source_map)) => {
            for (key, value) in source_map {
                if target_map.contains_key(&key) {
                    deep_merge(&mut target_map[&key], value);
                } else {
                    target_map.insert(key, value);
                }
            }
        }
        (target_ref, source_ref) => {
            *target_ref = source_ref;
        }
    }
}
