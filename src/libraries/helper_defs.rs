use std::collections::BTreeMap;

pub type HelperGenerator = fn(&str) -> Vec<u8>;

pub enum HelperDef {
    Bytecode(Vec<u8>),
    Generated(HelperGenerator),
}

impl HelperDef {
    pub fn bytecode(bytes: Vec<u8>) -> Self {
        Self::Bytecode(bytes)
    }

    pub fn generated(generator: HelperGenerator) -> Self {
        Self::Generated(generator)
    }

    pub fn lower(&self, target: &str) -> Vec<u8> {
        match self {
            Self::Bytecode(bytes) => bytes.clone(),
            Self::Generated(generator) => generator(target),
        }
    }
}

pub fn lower_helper_defs(
    defs: &BTreeMap<&'static str, HelperDef>,
    target: &str,
) -> BTreeMap<&'static str, Vec<u8>> {
    let mut lowered = BTreeMap::new();
    for (name, def) in defs {
        lowered.insert(*name, def.lower(target));
    }
    lowered
}
