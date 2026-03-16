#[derive(Debug, Clone)]
pub enum IrNode {
    FnStart {
        name: String,
        args: Vec<String>,
    },
    FnEnd,
    Let {
        name: String,
        rhs: String,
    },
    Call {
        call: String,
    },
    Instr {
        opcode: String,
        operands: Vec<String>,
    },
    Label {
        name: String,
    },
    Ret {
        value: Option<String>,
    },
}

impl IrNode {
    pub fn to_json_string(&self) -> String {
        match self {
            IrNode::FnStart { name, args } => format!(
                "{{\"op\":\"fn-start\",\"name\":\"{}\",\"args\":\"{}\"}}",
                name,
                args.join(" ")
            ),
            IrNode::FnEnd => "{\"op\":\"fn-end\"}".to_string(),
            IrNode::Let { name, rhs } => format!(
                "{{\"op\":\"let\",\"name\":\"{}\",\"rhs\":\"{}\"}}",
                name, rhs
            ),
            IrNode::Call { call } => format!("{{\"op\":\"call\",\"call\":\"{}\"}}", call),
            IrNode::Instr { opcode, operands } => format!(
                "{{\"op\":\"instr\",\"opcode\":\"{}\",\"operands\":\"{}\"}}",
                opcode,
                operands.join(" ")
            ),
            IrNode::Label { name } => format!("{{\"op\":\"label\",\"name\":\"{}\"}}", name),
            IrNode::Ret { value } => match value {
                Some(v) => format!("{{\"op\":\"ret\",\"value\":\"{}\"}}", v),
                None => "{\"op\":\"ret\"}".to_string(),
            },
        }
    }
}
