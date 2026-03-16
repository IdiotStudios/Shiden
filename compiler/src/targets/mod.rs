pub fn get_ini(name: &str) -> Option<&'static str> {
    match name {
        "linux-x86" | "x86_64-pc-linux-gnu" | "linux" => Some(include_str!("./linux-x86.ini")),
        _ => None,
    }
}
