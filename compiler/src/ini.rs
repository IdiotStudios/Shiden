use std::collections::HashMap;

pub type Ini = HashMap<String, HashMap<String, String>>;

pub fn parse_ini(src: &str) -> Ini {
    let mut out: Ini = HashMap::new();
    let mut cur_section = String::from("default");
    out.insert(cur_section.clone(), HashMap::new());

    for line in src.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if line.starts_with('#') || line.starts_with(';') {
            continue;
        }
        if line.starts_with('[') && line.ends_with(']') {
            cur_section = line[1..line.len() - 1].trim().to_string();
            out.entry(cur_section.clone()).or_insert_with(HashMap::new);
            continue;
        }
        if let Some(eq) = line.find('=') {
            let k = line[..eq].trim().to_string();
            let v = line[eq + 1..].trim().to_string();
            if let Some(sec) = out.get_mut(&cur_section) {
                sec.insert(k, v);
            }
        }
    }

    out
}

pub fn get_str(ini: &Ini, section: &str, key: &str) -> Option<String> {
    ini.get(section).and_then(|s| s.get(key).map(|v| v.clone()))
}

pub fn get_u64(ini: &Ini, section: &str, key: &str) -> Option<u64> {
    get_str(ini, section, key).and_then(|s| s.parse::<u64>().ok())
}

pub fn get_bool(ini: &Ini, section: &str, key: &str) -> Option<bool> {
    get_str(ini, section, key).and_then(|s| match s.to_lowercase().as_str() {
        "yes" | "true" | "1" => Some(true),
        "no" | "false" | "0" => Some(false),
        _ => None,
    })
}

pub fn get_list(ini: &Ini, section: &str, key: &str) -> Option<Vec<String>> {
    get_str(ini, section, key).map(|s| {
        s.split(',')
            .map(|p| p.trim().to_string())
            .filter(|p| !p.is_empty())
            .collect()
    })
}
