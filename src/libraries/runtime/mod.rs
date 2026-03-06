use std::collections::BTreeMap;

include!(concat!(env!("OUT_DIR"), "/helpers.rs"));

pub fn get_helpers(target: &str) -> BTreeMap<&'static str, Vec<u8>> {
    generated_get_helpers(target)
}
