use crate::ini;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Target {
    pub name: String,
    pub triple: String,
    pub word_bits: u64,
    pub pointer_bits: u64,
    pub endianness: String,
    pub return_register: Option<String>,
    pub arg_registers: Vec<String>,
    pub sections: Vec<String>,
    pub relocations: bool,
    pub pic: Option<String>,
    pub raw: HashMap<String, HashMap<String, String>>,
}

impl Default for Target {
    fn default() -> Self {
        Self {
            name: "unknown".to_string(),
            triple: "".to_string(),
            word_bits: 64,
            pointer_bits: 64,
            endianness: "little".to_string(),
            return_register: None,
            arg_registers: Vec::new(),
            sections: vec!["text".to_string(), "data".to_string()],
            relocations: true,
            pic: None,
            raw: HashMap::new(),
        }
    }
}

pub fn parse_target_from_ini_text(ini_text: &str) -> Target {
    let parsed = ini::parse_ini(ini_text);
    let mut t = Target::default();
    t.raw = parsed.clone();

    if let Some(meta_name) = ini::get_str(&parsed, "meta", "name") {
        t.name = meta_name;
    }
    if let Some(triple) = ini::get_str(&parsed, "target", "triple") {
        t.triple = triple;
    }
    if let Some(w) = ini::get_u64(&parsed, "target", "word_bits") {
        t.word_bits = w;
    }
    if let Some(p) = ini::get_u64(&parsed, "target", "pointer_bits") {
        t.pointer_bits = p;
    }
    if let Some(e) = ini::get_str(&parsed, "target", "endianness") {
        t.endianness = e;
    }

    if let Some(rr) = ini::get_str(&parsed, "abi", "return_register") {
        t.return_register = Some(rr);
    }
    if let Some(args) = ini::get_list(&parsed, "abi", "arg_registers") {
        t.arg_registers = args;
    }
    if let Some(secs) = ini::get_list(&parsed, "sections", "default_section_order") {
        t.sections = secs;
    }

    if let Some(rel) = ini::get_bool(&parsed, "props", "relocations") {
        t.relocations = rel;
    }
    if let Some(pic) = ini::get_str(&parsed, "props", "pic") {
        t.pic = Some(pic);
    }

    t
}
