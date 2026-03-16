use crate::target::Target;
use std::fs;

pub fn run_backend_sd(
    tcfg: Option<&Target>,
    backend_sd: &str,
    ir_path: &str,
    out_path: &str,
) -> Result<(), String> {
    let txt = fs::read_to_string(backend_sd)
        .map_err(|e| format!("failed to read backend sd {}: {}", backend_sd, e))?;

    if txt.contains("emit-flat") {
        if let Some(t) = tcfg {
            if t.name == "linux-x86" || t.triple == "x86_64-pc-linux-gnu" {
                return crate::backend_linux_x86::emit_backend(t, ir_path, out_path);
            } else {
                return Err(format!("emit-flat not implemented for target {}", t.name));
            }
        } else {
            return Err("no target configuration provided to bootstrap runner".to_string());
        }
    }

    Err("backend.sd did not declare a known backend action (look for 'emit-flat')".to_string())
}
