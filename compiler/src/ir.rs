use crate::ir_types::IrNode;
use std::fs::File;
use std::io::{BufWriter, Write};

pub fn write_ir(path: &str, entries: &[IrNode]) -> std::io::Result<()> {
    let f = File::create(path)?;
    let mut w = BufWriter::new(f);
    writeln!(w, "{{\"protocol_version\":1,\"entries\": [")?;
    for (i, e) in entries.iter().enumerate() {
        let s = e.to_json_string();
        if i + 1 == entries.len() {
            writeln!(w, "{}", s)?;
        } else {
            writeln!(w, "{},", s)?;
        }
    }
    writeln!(w, "]}}")?;
    Ok(())
}
