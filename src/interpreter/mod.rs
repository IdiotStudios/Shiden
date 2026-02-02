#[allow(dead_code)]
pub struct Interpreter {
    out: Vec<u8>,
    globals: HashMap<String, Value>,
    mutables: HashSet<String>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            out: Vec::new(),
            globals: HashMap::new(),
            mutables: HashSet::new(),
        }
    }

    pub fn run_program(&mut self, prog: &Program) -> Result<(), String> {
        let (params, _ret, body) =
            Self::find_function(prog, "main").ok_or("no 'main' function found".to_string())?;
        if !params.is_empty() {
            return Err("'main' must not accept parameters".into());
        }

        self.out.clear();
        match self.execute_block(prog, body)? {
            ExecOutcome::Break | ExecOutcome::Continue => {
                return Err("break or continue outside loop".into());
            }
            _ => (),
        }
        Ok(())
    }

    fn find_function<'a>(
        prog: &'a Program,
        name: &str,
    ) -> Option<(
        &'a Vec<(String, Option<String>)>,
        &'a Option<String>,
        &'a Vec<Stmt>,
    )> {
        for item in &prog.items {
            match item {
                Item::Function {
                    name: n,
                    params,
                    ret,
                    body,
                } if n == name => return Some((params, ret, body)),
                _ => (),
            }
        }
        None
    }

    fn execute_block(&mut self, prog: &Program, stmts: &Vec<Stmt>) -> Result<ExecOutcome, String> {
        let mut env: HashMap<String, Value> = HashMap::new();
        for s in stmts {
            match self.execute_stmt(prog, s, &mut env)? {
                ExecOutcome::None => {}
                ExecOutcome::Return(v) => return Ok(ExecOutcome::Return(v)),
                ExecOutcome::Break => return Ok(ExecOutcome::Break),
                ExecOutcome::Continue => return Ok(ExecOutcome::Continue),
            }
        }
        Ok(ExecOutcome::None)
    }

    fn execute_block_with_env(
        &mut self,
        prog: &Program,
        stmts: &Vec<Stmt>,
        env: &mut HashMap<String, Value>,
    ) -> Result<ExecOutcome, String> {
        for s in stmts {
            match self.execute_stmt(prog, s, env)? {
                ExecOutcome::None => {}
                ExecOutcome::Return(v) => return Ok(ExecOutcome::Return(v)),
                ExecOutcome::Break => return Ok(ExecOutcome::Break),
                ExecOutcome::Continue => return Ok(ExecOutcome::Continue),
            }
        }
        Ok(ExecOutcome::None)
    }

    fn value_type_name(v: &Value) -> String {
        match v {
            Value::Str(_) => "str".to_string(),
            Value::Int(_) => "i64".to_string(),
            Value::UInt(_) => "u64".to_string(),
            Value::Float(_) => "f64".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::Char(_) => "char".to_string(),
            Value::Array(_, _) => "array".to_string(),
            Value::Unit => "unit".to_string(),
        }
    }

    fn convert_number_to_int_range(i: i64, min: i64, max: i64) -> Result<Value, String> {
        if i >= min && i <= max {
            Ok(Value::Int(i))
        } else {
            Err(format!("value out of range for {}..{}", min, max))
        }
    }

    fn convert_value_to_type(v: Value, tname: &str) -> Result<Value, String> {
        match tname {
            "str" => match v {
                Value::Str(s) => Ok(Value::Str(s)),
                _ => Err("type mismatch: expected str".into()),
            },
            "char" => match v {
                Value::Char(c) => Ok(Value::Char(c)),
                _ => Err("type mismatch: expected char".into()),
            },
            "bool" => match v {
                Value::Bool(b) => Ok(Value::Bool(b)),
                _ => Err("type mismatch: expected bool".into()),
            },
            "f32" | "f64" => match v {
                Value::Float(f) => Ok(Value::Float(f)),
                Value::Int(i) => Ok(Value::Float(i as f64)),
                Value::UInt(u) => Ok(Value::Float(u as f64)),
                _ => Err("type mismatch: expected float".into()),
            },
            "u8" => match v {
                Value::UInt(u) => {
                    if u <= u8::MAX as u64 {
                        Ok(Value::UInt(u))
                    } else {
                        Err("value out of range for u8".into())
                    }
                }
                Value::Int(i) => {
                    if i >= 0 && i as u64 <= u8::MAX as u64 {
                        Ok(Value::UInt(i as u64))
                    } else {
                        Err("value out of range for u8".into())
                    }
                }
                Value::Float(f) => {
                    if f.fract() == 0.0 && f >= 0.0 && f <= u8::MAX as f64 {
                        Ok(Value::UInt(f as u64))
                    } else {
                        Err("value out of range for u8".into())
                    }
                }
                _ => Err("type mismatch: expected number".into()),
            },
            "u16" => match v {
                Value::UInt(u) => {
                    if u <= u16::MAX as u64 {
                        Ok(Value::UInt(u))
                    } else {
                        Err("value out of range for u16".into())
                    }
                }
                Value::Int(i) => {
                    if i >= 0 && i as u64 <= u16::MAX as u64 {
                        Ok(Value::UInt(i as u64))
                    } else {
                        Err("value out of range for u16".into())
                    }
                }
                Value::Float(f) => {
                    if f.fract() == 0.0 && f >= 0.0 && f <= u16::MAX as f64 {
                        Ok(Value::UInt(f as u64))
                    } else {
                        Err("value out of range for u16".into())
                    }
                }
                _ => Err("type mismatch: expected number".into()),
            },
            "u32" => match v {
                Value::UInt(u) => {
                    if u <= u32::MAX as u64 {
                        Ok(Value::UInt(u))
                    } else {
                        Err("value out of range for u32".into())
                    }
                }
                Value::Int(i) => {
                    if i >= 0 && i as u64 <= u32::MAX as u64 {
                        Ok(Value::UInt(i as u64))
                    } else {
                        Err("value out of range for u32".into())
                    }
                }
                Value::Float(f) => {
                    if f.fract() == 0.0 && f >= 0.0 && f <= u32::MAX as f64 {
                        Ok(Value::UInt(f as u64))
                    } else {
                        Err("value out of range for u32".into())
                    }
                }
                _ => Err("type mismatch: expected number".into()),
            },
            "u64" | "usize" => match v {
                Value::UInt(u) => Ok(Value::UInt(u)),
                Value::Int(i) => {
                    if i >= 0 {
                        Ok(Value::UInt(i as u64))
                    } else {
                        Err("value out of range for u64".into())
                    }
                }
                Value::Float(f) => {
                    if f.fract() == 0.0 && f >= 0.0 {
                        Ok(Value::UInt(f as u64))
                    } else {
                        Err("value out of range for u64".into())
                    }
                }
                _ => Err("type mismatch: expected number".into()),
            },
            "array" => match v {
                Value::Array(a, t) => Ok(Value::Array(a, t)),
                _ => Err("type mismatch: expected array".into()),
            },
            "i8" => match v {
                Value::Int(i) => {
                    if i >= i64::from(i8::MIN) && i <= i64::from(i8::MAX) {
                        Ok(Value::Int(i))
                    } else {
                        Err("value out of range for i8".into())
                    }
                }
                Value::UInt(u) => {
                    if u <= i64::MAX as u64
                        && (u as i64) >= i64::from(i8::MIN)
                        && (u as i64) <= i64::from(i8::MAX)
                    {
                        Ok(Value::Int(u as i64))
                    } else {
                        Err("value out of range for i8".into())
                    }
                }
                Value::Float(f) => {
                    if f.fract() == 0.0
                        && f >= i64::from(i8::MIN) as f64
                        && f <= i64::from(i8::MAX) as f64
                    {
                        Ok(Value::Int(f as i64))
                    } else {
                        Err("value out of range for i8".into())
                    }
                }
                _ => Err("type mismatch: expected number".into()),
            },
            "i16" => match v {
                Value::Int(i) => {
                    if i >= i64::from(i16::MIN) && i <= i64::from(i16::MAX) {
                        Ok(Value::Int(i))
                    } else {
                        Err("value out of range for i16".into())
                    }
                }
                Value::UInt(u) => {
                    if u <= i64::MAX as u64
                        && (u as i64) >= i64::from(i16::MIN)
                        && (u as i64) <= i64::from(i16::MAX)
                    {
                        Ok(Value::Int(u as i64))
                    } else {
                        Err("value out of range for i16".into())
                    }
                }
                Value::Float(f) => {
                    if f.fract() == 0.0
                        && f >= i64::from(i16::MIN) as f64
                        && f <= i64::from(i16::MAX) as f64
                    {
                        Ok(Value::Int(f as i64))
                    } else {
                        Err("value out of range for i16".into())
                    }
                }
                _ => Err("type mismatch: expected number".into()),
            },
            "i32" => match v {
                Value::Int(i) => {
                    if i >= i64::from(i32::MIN) && i <= i64::from(i32::MAX) {
                        Ok(Value::Int(i))
                    } else {
                        Err("value out of range for i32".into())
                    }
                }
                Value::UInt(u) => {
                    if u <= i64::MAX as u64
                        && (u as i64) >= i64::from(i32::MIN)
                        && (u as i64) <= i64::from(i32::MAX)
                    {
                        Ok(Value::Int(u as i64))
                    } else {
                        Err("value out of range for i32".into())
                    }
                }
                Value::Float(f) => {
                    if f.fract() == 0.0
                        && f >= i64::from(i32::MIN) as f64
                        && f <= i64::from(i32::MAX) as f64
                    {
                        Ok(Value::Int(f as i64))
                    } else {
                        Err("value out of range for i32".into())
                    }
                }
                _ => Err("type mismatch: expected number".into()),
            },
            "i64" => match v {
                Value::Int(i) => Ok(Value::Int(i)),
                Value::UInt(u) => {
                    if u <= i64::MAX as u64 {
                        Ok(Value::Int(u as i64))
                    } else {
                        Err("value out of range for i64".into())
                    }
                }
                Value::Float(f) => {
                    if f.fract() == 0.0 && f >= i64::MIN as f64 && f <= i64::MAX as f64 {
                        Ok(Value::Int(f as i64))
                    } else {
                        Err("value out of range for i64".into())
                    }
                }
                _ => Err("type mismatch: expected number".into()),
            },
            other => Err(format!("unknown type annotation: {}", other)),
        }
    }

    fn value_to_json(v: &Value) -> Result<serde_json::Value, String> {
        match v {
            Value::Str(s) => Ok(serde_json::Value::String(s.clone())),
            Value::Int(i) => Ok(serde_json::Value::Number(serde_json::Number::from(*i))),
            Value::UInt(u) => Ok(serde_json::Value::Number(serde_json::Number::from(*u))),
            Value::Float(f) => serde_json::Number::from_f64(*f)
                .map(serde_json::Value::Number)
                .ok_or_else(|| "invalid float".to_string()),
            Value::Bool(b) => Ok(serde_json::Value::Bool(*b)),
            Value::Char(c) => Ok(serde_json::Value::String(c.to_string())),
            Value::Array(arr, _t) => {
                let mut vec = Vec::new();
                for e in arr.iter() {
                    vec.push(Self::value_to_json(e)?);
                }
                Ok(serde_json::Value::Array(vec))
            }
            Value::Unit => Err("cannot convert unit to json".into()),
        }
    }

    fn json_deep_merge(dst: &mut serde_json::Value, src: &serde_json::Value) {
        match (dst, src) {
            (serde_json::Value::Object(dst_map), serde_json::Value::Object(src_map)) => {
                for (k, v) in src_map {
                    if dst_map.contains_key(k) {
                        Self::json_deep_merge(dst_map.get_mut(k).unwrap(), v);
                    } else {
                        dst_map.insert(k.clone(), v.clone());
                    }
                }
            }
            (serde_json::Value::Array(dst_arr), serde_json::Value::Array(src_arr)) => {
                dst_arr.extend(src_arr.clone());
            }
            (d, s) => {
                *d = s.clone();
            }
        }
    }

    fn execute_stmt(
        &mut self,
        prog: &Program,
        stmt: &Stmt,
        env: &mut HashMap<String, Value>,
    ) -> Result<ExecOutcome, String> {
        match stmt {
            Stmt::Let {
                name,
                value,
                mutable,
                ty,
            } => {
                let v = self.eval_expr(prog, value, env)?;

                let stored = if let Some(tname) = ty {
                    Self::convert_value_to_type(v, tname)?
                } else {
                    v
                };
                env.insert(name.clone(), stored);
                if *mutable {
                    self.mutables.insert(name.clone());
                }
                Ok(ExecOutcome::None)
            }
            Stmt::Assign { name, value } => {
                if !self.mutables.contains(name) {
                    return Err(format!("assignment to non-mutable variable: {}", name));
                }
                if !env.contains_key(name) {
                    return Err(format!("assignment to unknown variable: {}", name));
                }
                let v = self.eval_expr(prog, value, env)?;
                env.insert(name.clone(), v);
                Ok(ExecOutcome::None)
            }
            Stmt::AssignIndex { name, index, value } => {
                if !self.mutables.contains(name) {
                    return Err(format!("assignment to non-mutable variable: {}", name));
                }
                let v = self.eval_expr(prog, value, env)?;
                let vtype = Self::value_type_name(&v);
                let idxv = self.eval_expr(prog, index, env)?;
                match idxv {
                    Value::Int(i) => {
                        let idx = i as usize;
                        if let Some(base) = env.get_mut(name) {
                            match base {
                                Value::Array(a, t) => {
                                    if idx >= a.len() {
                                        return Err("array index out of bounds".into());
                                    }
                                    if let Some(existing) = t.as_ref() {
                                        if existing != &vtype {
                                            return Err("type mismatch in array assignment".into());
                                        }
                                    } else {
                                        *t = Some(vtype);
                                    }
                                    a[idx] = v;
                                    Ok(ExecOutcome::None)
                                }
                                _ => Err(format!("{} is not an array", name)),
                            }
                        } else if let Some(base) = self.globals.get_mut(name) {
                            match base {
                                Value::Array(a, t) => {
                                    if idx >= a.len() {
                                        return Err("array index out of bounds".into());
                                    }
                                    if let Some(existing) = t.as_ref() {
                                        if existing != &vtype {
                                            return Err("type mismatch in array assignment".into());
                                        }
                                    } else {
                                        *t = Some(vtype);
                                    }
                                    a[idx] = v;
                                    Ok(ExecOutcome::None)
                                }
                                _ => Err(format!("{} is not an array", name)),
                            }
                        } else {
                            Err(format!("assignment to unknown variable: {}", name))
                        }
                    }
                    _ => Err("array index must be a number".into()),
                }
            }
            Stmt::Expr(expr) => {
                let _ = self.eval_expr(prog, expr, env)?;
                Ok(ExecOutcome::None)
            }
            Stmt::Return(expr, _ty) => {
                let v = self.eval_expr(prog, expr, env)?;
                Ok(ExecOutcome::Return(v))
            }
            Stmt::Break => Ok(ExecOutcome::Break),
            Stmt::Continue => Ok(ExecOutcome::Continue),
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let c = self.eval_expr(prog, cond, env)?;
                match c {
                    Value::Bool(true) => {
                        match self.execute_block_with_env(prog, then_block, env)? {
                            ExecOutcome::None => {}
                            ExecOutcome::Return(v) => return Ok(ExecOutcome::Return(v)),
                            ExecOutcome::Break => return Ok(ExecOutcome::Break),
                            ExecOutcome::Continue => return Ok(ExecOutcome::Continue),
                        }
                    }
                    Value::Bool(false) => {
                        if let Some(eb) = else_block {
                            match self.execute_block_with_env(prog, eb, env)? {
                                ExecOutcome::None => {}
                                ExecOutcome::Return(v) => return Ok(ExecOutcome::Return(v)),
                                ExecOutcome::Break => return Ok(ExecOutcome::Break),
                                ExecOutcome::Continue => return Ok(ExecOutcome::Continue),
                            }
                        }
                    }
                    _ => return Err("if condition did not evaluate to boolean".into()),
                }
                Ok(ExecOutcome::None)
            }
            Stmt::While { cond, body } => {
                loop {
                    let c = self.eval_expr(prog, cond, env)?;
                    match c {
                        Value::Bool(true) => match self.execute_block_with_env(prog, body, env)? {
                            ExecOutcome::None => {}
                            ExecOutcome::Return(v) => return Ok(ExecOutcome::Return(v)),
                            ExecOutcome::Break => break,
                            ExecOutcome::Continue => continue,
                        },
                        Value::Bool(false) => break,
                        _ => return Err("while condition did not evaluate to boolean".into()),
                    }
                }
                Ok(ExecOutcome::None)
            }

            Stmt::For {
                var,
                iterable,
                body,
            } => {
                let iv = self.eval_expr(prog, iterable, env)?;
                match iv {
                    Value::Array(a, _typ) => {
                        for item in a {
                            env.insert(var.clone(), item.clone());
                            match self.execute_block_with_env(prog, &body, env)? {
                                ExecOutcome::None => {}
                                ExecOutcome::Return(v) => return Ok(ExecOutcome::Return(v)),
                                ExecOutcome::Break => break,
                                ExecOutcome::Continue => continue,
                            }
                        }
                        Ok(ExecOutcome::None)
                    }
                    Value::Int(n) => {
                        if n <= 0 {
                            return Ok(ExecOutcome::None);
                        }
                        let mut i = 0;
                        while i < n {
                            env.insert(var.clone(), Value::Int(i));
                            match self.execute_block_with_env(prog, &body, env)? {
                                ExecOutcome::None => {}
                                ExecOutcome::Return(v) => return Ok(ExecOutcome::Return(v)),
                                ExecOutcome::Break => break,
                                ExecOutcome::Continue => {
                                    i += 1;
                                    continue;
                                }
                            }
                            i += 1;
                        }
                        Ok(ExecOutcome::None)
                    }
                    Value::Str(s) => {
                        for ch in s.chars() {
                            env.insert(var.clone(), Value::Char(ch));
                            match self.execute_block_with_env(prog, &body, env)? {
                                ExecOutcome::None => {}
                                ExecOutcome::Return(v) => return Ok(ExecOutcome::Return(v)),
                                ExecOutcome::Break => break,
                                ExecOutcome::Continue => continue,
                            }
                        }
                        Ok(ExecOutcome::None)
                    }
                    _ => Err("for loop iterable must be array, number, or string".into()),
                }
            }
        }
    }
    fn eval_expr(
        &mut self,
        prog: &Program,
        expr: &Expr,
        env: &mut HashMap<String, Value>,
    ) -> Result<Value, String> {
        return match expr {
            Expr::Identifier(name) => {
                if let Some(v) = env.get(name) {
                    Ok(v.clone())
                } else if let Some(v) = self.globals.get(name) {
                    Ok(v.clone())
                } else {
                    Err(format!("undefined identifier: {}", name))
                }
            }
            Expr::StringLiteral(s) => Ok(Value::Str(s.clone())),
            Expr::Char(c) => Ok(Value::Char(*c)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Number(n) => {
                if n.contains('.') {
                    let parsed = n
                        .parse::<f64>()
                        .map_err(|e| format!("invalid float {}: {}", n, e))?;
                    Ok(Value::Float(parsed))
                } else {
                    let parsed = n
                        .parse::<i64>()
                        .map_err(|e| format!("invalid number {}: {}", n, e))?;
                    Ok(Value::Int(parsed))
                }
            }
            Expr::ArrayLiteral(elems) => {
                let mut out = Vec::new();
                for e in elems {
                    out.push(self.eval_expr(prog, e, env)?);
                }

                let etype = if out.is_empty() {
                    None
                } else {
                    let first = Self::value_type_name(&out[0]);
                    for v in &out[1..] {
                        if Self::value_type_name(v) != first {
                            return Err("array literal elements must have the same type".into());
                        }
                    }
                    Some(first)
                };
                Ok(Value::Array(out, etype))
            }
            Expr::Index { expr, index } => {
                let base = self.eval_expr(prog, expr, env)?;
                let idxv = self.eval_expr(prog, index, env)?;
                match (base, idxv) {
                    (Value::Array(a, _typ), Value::Int(i)) => {
                        let idx = i as usize;
                        if idx >= a.len() {
                            return Err("array index out of bounds".into());
                        }
                        Ok(a[idx].clone())
                    }
                    _ => Err("type mismatch in indexing".into()),
                }
            }
            Expr::Unary { op, expr } => {
                let v = self.eval_expr(prog, expr, env)?;
                match op {
                    crate::syntax::UnaryOp::Not => match v {
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        _ => Err("type mismatch in '!'".into()),
                    },
                    crate::syntax::UnaryOp::Neg => match v {
                        Value::Int(n) => Ok(Value::Int(-n)),
                        Value::Float(f) => Ok(Value::Float(-f)),
                        Value::UInt(_) => Err("cannot apply unary '-' to unsigned value".into()),
                        _ => Err("type mismatch in unary '-'".into()),
                    },
                }
            }
            Expr::Call { name, args } => {
                let mut evaled = Vec::new();
                for a in args {
                    evaled.push(self.eval_expr(prog, a, env)?);
                }

                if name == "println" {
                    if evaled.is_empty() {
                        writeln!(&mut self.out).map_err(|e| e.to_string())?;
                        return Ok(Value::Unit);
                    }

                    let mut out_s = evaled[0].to_string();
                    for a in &evaled[1..] {
                        if let Some(pos) = out_s.find("{}") {
                            out_s.replace_range(pos..pos + 2, &a.to_string());
                        } else {
                            out_s.push(' ');
                            out_s.push_str(&a.to_string());
                        }
                    }
                    writeln!(&mut self.out, "{}", out_s).map_err(|e| e.to_string())?;
                    return Ok(Value::Unit);
                }

                if name == "print" {
                    if evaled.is_empty() {
                        return Ok(Value::Unit);
                    }
                    let mut out_s = evaled[0].to_string();
                    for a in &evaled[1..] {
                        out_s.push(' ');
                        out_s.push_str(&a.to_string());
                    }
                    write!(&mut self.out, "{}", out_s).map_err(|e| e.to_string())?;
                    return Ok(Value::Unit);
                }

                if name == "write_file" {
                    if evaled.len() != 2 {
                        return Err("write_file(path, content) expects 2 args".into());
                    }
                    match (&evaled[0], &evaled[1]) {
                        (Value::Str(path), Value::Str(content)) => {
                            std::fs::write(path, content).map_err(|e| e.to_string())?;
                            return Ok(Value::Unit);
                        }
                        _ => return Err("write_file expects string arguments".into()),
                    }
                }

                if name == "len" {
                    if evaled.len() != 1 {
                        return Err("len(arr) expects 1 arg".into());
                    }
                    match &evaled[0] {
                        Value::Array(a, _t) => return Ok(Value::Int(a.len() as i64)),
                        Value::Str(s) => return Ok(Value::Int(s.chars().count() as i64)),
                        _ => return Err("len expects an array or string".into()),
                    }
                }

                if name == "push" {
                    if args.len() != 2 {
                        return Err("push(arr, val) expects 2 args".into());
                    }
                    match &args[0] {
                        crate::syntax::Expr::Identifier(var) => {
                            let val = evaled.get(1).ok_or("missing value argument")?.clone();
                            let vtype = Self::value_type_name(&val);

                            if !self.mutables.contains(var) {
                                return Err(format!("assignment to non-mutable variable: {}", var));
                            }
                            if let Some(base) = env.get_mut(var) {
                                match base {
                                    Value::Array(a, t) => {
                                        if let Some(existing) = t.as_ref() {
                                            if existing != &vtype {
                                                return Err("type mismatch in push".into());
                                            }
                                        } else {
                                            *t = Some(vtype);
                                        }
                                        a.push(val);
                                        return Ok(Value::Unit);
                                    }
                                    _ => return Err(format!("{} is not an array", var)),
                                }
                            } else if let Some(base) = self.globals.get_mut(var) {
                                match base {
                                    Value::Array(a, t) => {
                                        if let Some(existing) = t.as_ref() {
                                            if existing != &vtype {
                                                return Err("type mismatch in push".into());
                                            }
                                        } else {
                                            *t = Some(vtype);
                                        }
                                        a.push(val);
                                        return Ok(Value::Unit);
                                    }
                                    _ => return Err(format!("{} is not an array", var)),
                                }
                            } else {
                                return Err(format!("push to unknown variable: {}", var));
                            }
                        }
                        _ => return Err("push expects variable name as first arg".into()),
                    }
                }

                if name == "range" {
                    if evaled.len() != 2 {
                        return Err("range(start, end) expects 2 args".into());
                    }
                    match (&evaled[0], &evaled[1]) {
                        (Value::Int(s), Value::Int(e)) => {
                            let mut v = Vec::new();
                            let mut i = *s;
                            while i < *e {
                                v.push(Value::Int(i));
                                i += 1;
                            }
                            return Ok(Value::Array(v, Some("i64".to_string())));
                        }
                        _ => return Err("range expects integer arguments".into()),
                    }
                }

                if name == "pop" {
                    if args.len() != 1 {
                        return Err("pop(arr) expects 1 arg".into());
                    }
                    match &args[0] {
                        crate::syntax::Expr::Identifier(var) => {
                            if !self.mutables.contains(var) {
                                return Err(format!("assignment to non-mutable variable: {}", var));
                            }
                            if let Some(base) = env.get_mut(var) {
                                match base {
                                    Value::Array(a, t) => {
                                        if a.is_empty() {
                                            return Err("pop from empty array".into());
                                        }
                                        let v = a.pop().unwrap();

                                        return Ok(v);
                                    }
                                    _ => return Err(format!("{} is not an array", var)),
                                }
                            } else if let Some(base) = self.globals.get_mut(var) {
                                match base {
                                    Value::Array(a, t) => {
                                        if a.is_empty() {
                                            return Err("pop from empty array".into());
                                        }
                                        let v = a.pop().unwrap();
                                        return Ok(v);
                                    }
                                    _ => return Err(format!("{} is not an array", var)),
                                }
                            } else {
                                return Err(format!("pop from unknown variable: {}", var));
                            }
                        }
                        _ => return Err("pop expects variable name as first arg".into()),
                    }
                }

                if name == "append_file" {
                    if evaled.len() != 2 {
                        return Err("append_file(path, content) expects 2 args".into());
                    }
                    match (&evaled[0], &evaled[1]) {
                        (Value::Str(path), Value::Str(content)) => {
                            use std::io::Write as _;
                            let mut f = std::fs::OpenOptions::new()
                                .create(true)
                                .append(true)
                                .open(path)
                                .map_err(|e| e.to_string())?;
                            f.write_all(content.as_bytes()).map_err(|e| e.to_string())?;
                            return Ok(Value::Unit);
                        }
                        _ => return Err("append_file expects string arguments".into()),
                    }
                }

                if name == "read_file" || name == "fs_read" {
                    if evaled.len() != 1 {
                        return Err("read_file(path) expects 1 arg".into());
                    }
                    return match &evaled[0] {
                        Value::Str(path) => {
                            let s = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
                            Ok(Value::Str(s))
                        }
                        _ => Err("read_file expects a string path".into()),
                    };
                }

                if name == "write_file" || name == "fs_write" {
                    if evaled.len() != 2 {
                        return Err("write_file(path, content) expects 2 args".into());
                    }
                    return match (&evaled[0], &evaled[1]) {
                        (Value::Str(path), Value::Str(content)) => {
                            std::fs::write(path, content).map_err(|e| e.to_string())?;
                            Ok(Value::Unit)
                        }
                        _ => Err("write_file expects string arguments".into()),
                    };
                }

                if name == "fs_edit" {
                    if evaled.len() != 3 && evaled.len() != 4 {
                        return Err(
                            "fs_edit(path, key, value[, strategy]) expects 3 or 4 args".into()
                        );
                    }

                    let strategy = if evaled.len() == 4 {
                        match &evaled[3] {
                            Value::Str(s) => s.clone(),
                            _ => return Err("strategy must be a string".into()),
                        }
                    } else {
                        "replace".to_string()
                    };

                    return match (&evaled[0], &evaled[1]) {
                        (Value::Str(path), Value::Str(key)) => {
                            let new_val = Self::value_to_json(&evaled[2])?;
                            let contents = std::fs::read_to_string(path).unwrap_or_else(|_| "{}".to_string());
                            let mut json: serde_json::Value = serde_json::from_str(&contents).unwrap_or_else(|_| serde_json::Value::Object(serde_json::Map::new()));
                            if !json.is_object() {
                                json = serde_json::Value::Object(serde_json::Map::new());
                            }
                            let obj = json.as_object_mut().unwrap();

                            match strategy.as_str() {
                                "replace" => {
                                    obj.insert(key.clone(), new_val);
                                }
                                "merge" => {
                                    let entry = obj.entry(key.clone()).or_insert(serde_json::Value::Object(serde_json::Map::new()));
                                    Self::json_deep_merge(entry, &new_val);
                                }
                                "append" => {
                                    let entry = obj.entry(key.clone()).or_insert(serde_json::Value::Array(Vec::new()));
                                    if let serde_json::Value::Array(a) = entry {
                                        match new_val {
                                            serde_json::Value::Array(mut na) => a.append(&mut na),
                                            other => a.push(other),
                                        }
                                    } else {
                                        return Err("append strategy requires existing value to be an array or absent".into());
                                    }
                                }
                                "unique" => {
                                    let entry = obj.entry(key.clone()).or_insert(serde_json::Value::Array(Vec::new()));
                                    if let serde_json::Value::Array(a) = entry {
                                        match new_val {
                                            serde_json::Value::Array(na) => {
                                                for v in na {
                                                    if !a.iter().any(|x| x == &v) {
                                                        a.push(v);
                                                    }
                                                }
                                            }
                                            other => {
                                                if !a.iter().any(|x| x == &other) {
                                                    a.push(other);
                                                }
                                            }
                                        }
                                    } else {
                                        return Err("unique strategy requires existing value to be an array or absent".into());
                                    }
                                }
                                _ => return Err("unknown strategy, valid: replace, merge, append, unique".into()),
                            }

                            let s = serde_json::to_string_pretty(&json).map_err(|e| e.to_string())?;
                            std::fs::write(path, s).map_err(|e| e.to_string())?;
                            Ok(Value::Unit)
                        }
                        _ => Err("fs_edit expects (path, key, value[, strategy]) where path and key are strings".into()),
                    };
                }

                if name == "fs_delete" {
                    if evaled.len() != 2 {
                        return Err("fs_delete(path, key) expects 2 args".into());
                    }
                    return match (&evaled[0], &evaled[1]) {
                        (Value::Str(path), Value::Str(key)) => {
                            let contents =
                                std::fs::read_to_string(path).unwrap_or_else(|_| "{}".to_string());
                            let mut json: serde_json::Value = serde_json::from_str(&contents)
                                .unwrap_or_else(|_| {
                                    serde_json::Value::Object(serde_json::Map::new())
                                });
                            if let serde_json::Value::Object(obj) = &mut json {
                                obj.remove(key);
                            }
                            let s =
                                serde_json::to_string_pretty(&json).map_err(|e| e.to_string())?;
                            std::fs::write(path, s).map_err(|e| e.to_string())?;
                            Ok(Value::Unit)
                        }
                        _ => Err(
                            "fs_delete expects (path, key) where path and key are strings".into(),
                        ),
                    };
                }

                if name == "fs_array_append" {
                    if evaled.len() != 3 {
                        return Err("fs_array_append(path, key, value) expects 3 args".into());
                    }
                    return match (&evaled[0], &evaled[1]) {
                        (Value::Str(path), Value::Str(key)) => {
                            let new_val = Self::value_to_json(&evaled[2])?;
                            let contents = std::fs::read_to_string(path).unwrap_or_else(|_| "{}".to_string());
                            let mut json: serde_json::Value = serde_json::from_str(&contents).unwrap_or_else(|_| serde_json::Value::Object(serde_json::Map::new()));
                            if !json.is_object() {
                                json = serde_json::Value::Object(serde_json::Map::new());
                            }
                            let entry = json.as_object_mut().unwrap().entry(key.clone()).or_insert(serde_json::Value::Array(Vec::new()));
                            if let serde_json::Value::Array(a) = entry {
                                match new_val {
                                    serde_json::Value::Array(mut na) => a.append(&mut na),
                                    other => a.push(other),
                                }
                            } else {
                                return Err("existing value is not an array".into());
                            }
                            let s = serde_json::to_string_pretty(&json).map_err(|e| e.to_string())?;
                            std::fs::write(path, s).map_err(|e| e.to_string())?;
                            Ok(Value::Unit)
                        }
                        _ => Err("fs_array_append expects (path, key, value) where path and key are strings".into()),
                    };
                }

                if name == "fs_merge" {
                    if evaled.len() != 2 {
                        return Err("fs_merge(path, json_str) expects 2 args".into());
                    }
                    return match (&evaled[0], &evaled[1]) {
                        (Value::Str(path), Value::Str(json_str)) => {
                            println!("fs_merge called with path={} json_str={}", path, json_str);
                            let incoming: serde_json::Value =
                                serde_json::from_str(json_str).map_err(|e| e.to_string())?;
                            let contents =
                                std::fs::read_to_string(path).unwrap_or_else(|_| "{}".to_string());
                            let mut base: serde_json::Value = serde_json::from_str(&contents)
                                .unwrap_or_else(|_| {
                                    serde_json::Value::Object(serde_json::Map::new())
                                });
                            Self::json_deep_merge(&mut base, &incoming);
                            let s =
                                serde_json::to_string_pretty(&base).map_err(|e| e.to_string())?;
                            std::fs::write(path, s).map_err(|e| e.to_string())?;
                            Ok(Value::Unit)
                        }
                        _ => Err(
                            "fs_merge expects (path, json_str) where both args are strings".into(),
                        ),
                    };
                }

                if name == "append_file" {
                    if evaled.len() != 2 {
                        return Err("append_file(path, content) expects 2 args".into());
                    }
                    return match (&evaled[0], &evaled[1]) {
                        (Value::Str(path), Value::Str(content)) => {
                            use std::io::Write as _;
                            let mut f = std::fs::OpenOptions::new()
                                .create(true)
                                .append(true)
                                .open(path)
                                .map_err(|e| e.to_string())?;
                            f.write_all(content.as_bytes()).map_err(|e| e.to_string())?;
                            Ok(Value::Unit)
                        }
                        _ => Err("append_file expects string arguments".into()),
                    };
                }

                if let Some((params, ret, body)) = Self::find_function(prog, name) {
                    if params.len() != evaled.len() {
                        return Err(format!("argument count mismatch for {}", name));
                    }
                    let mut fn_env: HashMap<String, Value> = HashMap::new();

                    for (i, (pname, ptype)) in params.iter().enumerate() {
                        let arg = evaled[i].clone();
                        let stored = if let Some(pt) = ptype {
                            Self::convert_value_to_type(arg, pt)?
                        } else {
                            arg
                        };
                        fn_env.insert(pname.clone(), stored);
                    }
                    match self.execute_block_with_env(prog, body, &mut fn_env)? {
                        ExecOutcome::Return(v) => {
                            if let Some(rt) = ret {
                                let r = Self::convert_value_to_type(v, rt)?;
                                return Ok(r);
                            }
                            return Ok(v);
                        }
                        ExecOutcome::Break | ExecOutcome::Continue => {
                            return Err("break or continue outside loop".into());
                        }
                        ExecOutcome::None => {
                            if let Some(rt) = ret {
                                return Self::convert_value_to_type(Value::Unit, rt);
                            }
                            return Ok(Value::Unit);
                        }
                    }
                } else {
                    return Err(format!("unknown function: {}", name));
                }
            }
            Expr::Binary { left, op, right } => match op {
                crate::syntax::BinOp::And => {
                    let l = self.eval_expr(prog, left, env)?;
                    match l {
                        Value::Bool(false) => return Ok(Value::Bool(false)),
                        Value::Bool(true) => {
                            let r = self.eval_expr(prog, right, env)?;
                            match r {
                                Value::Bool(b) => return Ok(Value::Bool(b)),
                                _ => return Err("type mismatch in '&&'".into()),
                            }
                        }
                        _ => return Err("type mismatch in '&&'".into()),
                    }
                }
                crate::syntax::BinOp::Or => {
                    let l = self.eval_expr(prog, left, env)?;
                    match l {
                        Value::Bool(true) => return Ok(Value::Bool(true)),
                        Value::Bool(false) => {
                            let r = self.eval_expr(prog, right, env)?;
                            match r {
                                Value::Bool(b) => return Ok(Value::Bool(b)),
                                _ => return Err("type mismatch in '||'".into()),
                            }
                        }
                        _ => return Err("type mismatch in '||'".into()),
                    }
                }
                _ => {
                    let l = self.eval_expr(prog, left, env)?;
                    let r = self.eval_expr(prog, right, env)?;

                    fn to_f64(v: &Value) -> Option<f64> {
                        match v {
                            Value::Int(i) => Some(*i as f64),
                            Value::UInt(u) => Some(*u as f64),
                            Value::Float(f) => Some(*f),
                            _ => None,
                        }
                    }

                    match op {
                        crate::syntax::BinOp::Eq => match (&l, &r) {
                            (Value::Int(a), Value::Int(b)) => return Ok(Value::Bool(a == b)),
                            (Value::UInt(a), Value::UInt(b)) => return Ok(Value::Bool(a == b)),
                            (Value::Float(a), Value::Float(b)) => return Ok(Value::Bool(a == b)),
                            (Value::Str(a), Value::Str(b)) => return Ok(Value::Bool(a == b)),
                            (Value::Bool(a), Value::Bool(b)) => return Ok(Value::Bool(a == b)),
                            (Value::Char(a), Value::Char(b)) => return Ok(Value::Bool(a == b)),

                            _ => {
                                if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                    return Ok(Value::Bool(af == bf));
                                } else {
                                    return Err("type mismatch in '=='".into());
                                }
                            }
                        },
                        crate::syntax::BinOp::Ne => match (&l, &r) {
                            (Value::Int(a), Value::Int(b)) => return Ok(Value::Bool(a != b)),
                            (Value::UInt(a), Value::UInt(b)) => return Ok(Value::Bool(a != b)),
                            (Value::Float(a), Value::Float(b)) => return Ok(Value::Bool(a != b)),
                            (Value::Str(a), Value::Str(b)) => return Ok(Value::Bool(a != b)),
                            (Value::Bool(a), Value::Bool(b)) => return Ok(Value::Bool(a != b)),
                            (Value::Char(a), Value::Char(b)) => return Ok(Value::Bool(a != b)),

                            _ => {
                                if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                    return Ok(Value::Bool(af != bf));
                                } else {
                                    return Err("type mismatch in '!='".into());
                                }
                            }
                        },
                        crate::syntax::BinOp::Lt => {
                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                return Ok(Value::Bool(af < bf));
                            } else {
                                return Err("type mismatch in '<'".into());
                            }
                        }
                        crate::syntax::BinOp::Le => {
                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                return Ok(Value::Bool(af <= bf));
                            } else {
                                return Err("type mismatch in '<='".into());
                            }
                        }
                        crate::syntax::BinOp::Gt => {
                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                return Ok(Value::Bool(af > bf));
                            } else {
                                return Err("type mismatch in '>'".into());
                            }
                        }
                        crate::syntax::BinOp::Ge => {
                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                return Ok(Value::Bool(af >= bf));
                            } else {
                                return Err("type mismatch in '>='".into());
                            }
                        }
                        crate::syntax::BinOp::Add => {
                            if let (Value::Str(a), Value::Str(b)) = (&l, &r) {
                                return Ok(Value::Str(format!("{}{}", a, b)));
                            }

                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                if matches!(&l, Value::Float(_)) || matches!(&r, Value::Float(_)) {
                                    return Ok(Value::Float(af + bf));
                                }
                            }

                            if let (Value::UInt(a), Value::UInt(b)) = (l.clone(), r.clone()) {
                                return a
                                    .checked_add(b)
                                    .map(Value::UInt)
                                    .ok_or_else(|| "unsigned overflow".into());
                            }

                            if let (Value::Int(a), Value::Int(b)) = (l.clone(), r.clone()) {
                                return a
                                    .checked_add(b)
                                    .map(Value::Int)
                                    .ok_or_else(|| "signed overflow".into());
                            }

                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                return Ok(Value::Float(af + bf));
                            }
                            return Err("type mismatch in '+'".into());
                        }
                        crate::syntax::BinOp::Sub => {
                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                if matches!(&l, Value::Float(_)) || matches!(&r, Value::Float(_)) {
                                    return Ok(Value::Float(af - bf));
                                }
                            }
                            if let (Value::UInt(a), Value::UInt(b)) = (l.clone(), r.clone()) {
                                return a
                                    .checked_sub(b)
                                    .map(Value::UInt)
                                    .ok_or_else(|| "unsigned overflow".into());
                            }
                            if let (Value::Int(a), Value::Int(b)) = (l.clone(), r.clone()) {
                                return a
                                    .checked_sub(b)
                                    .map(Value::Int)
                                    .ok_or_else(|| "signed overflow".into());
                            }
                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                return Ok(Value::Float(af - bf));
                            }
                            Err("type mismatch in '-'".into())
                        }
                        crate::syntax::BinOp::Mul => {
                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                if matches!(&l, Value::Float(_)) || matches!(&r, Value::Float(_)) {
                                    return Ok(Value::Float(af * bf));
                                }
                            }
                            if let (Value::UInt(a), Value::UInt(b)) = (l.clone(), r.clone()) {
                                return a
                                    .checked_mul(b)
                                    .map(Value::UInt)
                                    .ok_or_else(|| "unsigned overflow".into());
                            }
                            if let (Value::Int(a), Value::Int(b)) = (l.clone(), r.clone()) {
                                return a
                                    .checked_mul(b)
                                    .map(Value::Int)
                                    .ok_or_else(|| "signed overflow".into());
                            }
                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                return Ok(Value::Float(af * bf));
                            }
                            Err("type mismatch in '*'".into())
                        }
                        crate::syntax::BinOp::Div => {
                            if let (Value::Int(a), Value::Int(b)) = (l.clone(), r.clone()) {
                                if b == 0 {
                                    return Err("division by zero".into());
                                }
                                return a
                                    .checked_div(b)
                                    .map(Value::Int)
                                    .ok_or_else(|| "signed division error".into());
                            }

                            if let (Value::UInt(a), Value::UInt(b)) = (l.clone(), r.clone()) {
                                if b == 0 {
                                    return Err("division by zero".into());
                                }
                                return Ok(Value::UInt(a / b));
                            }

                            if let (Some(af), Some(bf)) = (to_f64(&l), to_f64(&r)) {
                                return Ok(Value::Float(af / bf));
                            }
                            Err("type mismatch in '/'".into())
                        }
                        _ => unreachable!(),
                    }
                }
            },
        };
    }

    pub fn output(&self) -> String {
        String::from_utf8_lossy(&self.out).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::parse;

    #[test]
    fn run_main_prints() {
        let src =
            "fn new main/\n    let name = \"Nayte\"/str\n    println(\"Hello {}\", name)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "Hello Nayte\n");
    }

    #[test]
    fn run_multiple_prints() {
        let src = "fn new main/\n    println(\"one\")/unit\n    println(\"{} {}\", \"a\", \"b\")/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "one\na b\n");
    }

    #[test]
    fn call_with_params_and_print() {
        let src = "fn new greet(name)/\n    println(\"hi {}\", name)/unit\nfn/\nfn new main/\n    greet(\"alice\")/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "hi alice\n");
    }

    #[test]
    fn builtin_file_write_and_read() {
        let base = std::env::temp_dir().join(format!("shiden_test_fs_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let _ = std::fs::create_dir_all(&base);
        let path = base.join("out.txt");
        let pstr = path.to_str().unwrap();
        let src = format!(
            "fn new main/\n    write_file(\"{}\", \"hello\")/unit\n    let s = read_file(\"{}\")/str\n    println(\"{{}}\", s)/unit\nfn/",
            pstr, pstr
        );
        let prog = parse(&src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "hello\n");
        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn builtin_fs_edit() {
        let base = std::env::temp_dir().join(format!("shiden_test_fs_edit_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let _ = std::fs::create_dir_all(&base);
        let path = base.join("example.json");
        let pstr = path.to_str().unwrap();

        let src = format!(
            "fn new main/\n    write_file(\"{}\", \"\")/unit\n    fs_edit(\"{}\", \"example\", false)/unit\n    let s = read_file(\"{}\")/str\n    println(\"{{}}\", s)/unit\nfn/",
            pstr, pstr, pstr
        );
        let prog = parse(&src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        let out = it.output();
        assert!(out.contains("\"example\": false"));

        let src2 = format!(
            "fn new main/\n    write_file(\"{}\", \"\")/unit\n    fs edit(\"{}\", \"example\", false)/unit\n    let s = read_file(\"{}\")/str\n    println(\"{{}}\", s)/unit\nfn/",
            pstr, pstr, pstr
        );
        let prog2 = parse(&src2).expect("parse failed");
        let mut it2 = Interpreter::new();
        it2.run_program(&prog2).expect("runtime error");
        let out2 = it2.output();
        assert!(out2.contains("\"example\": false"));
        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn builtin_fs_array_append() {
        let base =
            std::env::temp_dir().join(format!("shiden_test_fs_array_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let _ = std::fs::create_dir_all(&base);
        let path = base.join("example.json");
        let pstr = path.to_str().unwrap();
        let src = format!(
            "fn new main/\n    let a = [1]/array\n    fs_edit(\"{}\", \"arr\", a)/unit\n    fs_array_append(\"{}\", \"arr\", 2)/unit\n    let s = read_file(\"{}\")/str\n    println(\"{{}}\", s)/unit\nfn/",
            pstr, pstr, pstr
        );
        let prog = parse(&src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        let out = it.output();
        let j: serde_json::Value = serde_json::from_str(&out).expect("file should be JSON");
        assert!(j["arr"].is_array());
        let arr = j["arr"].as_array().unwrap();
        assert!(
            arr.iter()
                .any(|v| v == &serde_json::Value::Number(1.into()))
        );
        assert!(
            arr.iter()
                .any(|v| v == &serde_json::Value::Number(2.into()))
        );
        let _ = std::fs::remove_dir_all(&base);
        let _ = std::fs::create_dir_all(&base);

        let src2 = format!(
            "fn new main/\n    let a = [1]/array\n    fs edit(\"{}\", \"arr\", a)/unit\n    fs array-append(\"{}\", \"arr\", 2)/unit\n    let s = read_file(\"{}\")/str\n    println(\"{{}}\", s)/unit\nfn/",
            pstr, pstr, pstr
        );
        let prog2 = parse(&src2).expect("parse failed");
        let mut it2 = Interpreter::new();
        it2.run_program(&prog2).expect("runtime error");
        let out2 = it2.output();
        let j2: serde_json::Value = serde_json::from_str(&out2).expect("file should be JSON");
        assert!(j2["arr"].is_array());
        let arr2 = j2["arr"].as_array().unwrap();
        assert!(
            arr2.iter()
                .any(|v| v == &serde_json::Value::Number(1.into()))
        );
        assert!(
            arr2.iter()
                .any(|v| v == &serde_json::Value::Number(2.into()))
        );
        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn builtin_fs_edit_append_and_unique() {
        let base =
            std::env::temp_dir().join(format!("shiden_test_fs_edit_append_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let _ = std::fs::create_dir_all(&base);
        let path = base.join("example.json");
        let pstr = path.to_str().unwrap();
        let src = format!(
            "fn new main/\n    let a = [1]/array\n    let b = [2]/array\n    fs_edit(\"{}\", \"arr\", a)/unit\n    fs_edit(\"{}\", \"arr\", b, \"append\")/unit\n    fs_edit(\"{}\", \"arr\", 2, \"unique\")/unit\n    let s = read_file(\"{}\")/str\n    println(\"{{}}\", s)/unit\nfn/",
            pstr, pstr, pstr, pstr
        );
        let prog = parse(&src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        let out = it.output();
        let j: serde_json::Value = serde_json::from_str(&out).expect("file should be JSON");
        assert!(j["arr"].is_array());
        let arr = j["arr"].as_array().unwrap();
        assert!(
            arr.iter()
                .any(|v| v == &serde_json::Value::Number(1.into()))
        );
        assert!(
            arr.iter()
                .any(|v| v == &serde_json::Value::Number(2.into()))
        );
        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn builtin_fs_edit_merge_and_delete() {
        let base =
            std::env::temp_dir().join(format!("shiden_test_fs_merge_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let _ = std::fs::create_dir_all(&base);
        let path = base.join("example.json");
        let pstr = path.to_str().unwrap();

        let src = format!(
            "fn new main/\n    let a = [1]/array\n    fs_edit(\"{}\", \"obj\", a)/unit\n    fs_array_append(\"{}\", \"obj\", 2)/unit\n    fs_delete(\"{}\", \"obj\")/unit\n    let s = read_file(\"{}\")/str\n    println(\"{{}}\", s)/unit\nfn/",
            pstr, pstr, pstr, pstr
        );
        let prog = parse(&src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        let out = it.output();

        assert!(!out.contains("\"obj\""));
        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn function_return_value() {
        let src = "fn new echo(x)/\n    return x/str\nfn/\nfn new main/\n    let y = echo(\"ok\")/str\n    println(\"{}\", y)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "ok\n");
    }

    #[test]
    fn if_else_runtime() {
        let src = "fn new main/\n    if 1 == 1/\n        println(\"yes\")/unit\n    else/\n        println(\"no\")/unit\n    fn/\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "yes\n");
    }

    #[test]
    fn mutable_assignment() {
        let src = "fn new main/\n    let mut x = \"a\"/str\n    x = \"b\"/str\n    println(\"{}\", x)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "b\n");
    }

    #[test]
    fn logic_and_short_circuit() {
        let src = "fn new main/\n    println(\"{}\", 1 == 0 && unknown())/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "false\n");
    }

    #[test]
    fn logic_or_short_circuit() {
        let src = "fn new main/\n    println(\"{}\", 1 == 1 || unknown())/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "true\n");
    }

    #[test]
    fn unary_not_behaviour() {
        let src = "fn new main/\n    println(\"{} {}\", !(1 == 1), !(1 == 0))/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "false true\n");
    }

    #[test]
    fn arithmetic_precedence() {
        let src = "fn new main/\n    let y = 1 + 2 * 3/i64\n    println(\"{}\", y)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "7\n");
    }

    #[test]
    fn parentheses_precedence() {
        let src = "fn new main/\n    let z = (1 + 2) * 3/i64\n    println(\"{}\", z)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "9\n");
    }

    #[test]
    fn division_behavior() {
        let src = "fn new main/\n    let a = 7 / 2/i64\n    println(\"{}\", a)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "3\n");
    }

    #[test]
    fn while_increments() {
        let src = "fn new main/\n    let mut x = 0/i64\n    while x < 3/\n        x = x + 1/i64\n    fn/\n    println(\"{}\", x)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "3\n");
    }

    #[test]
    fn while_never_runs() {
        let src = "fn new main/\n    let mut x = 4/i64\n    while x < 3/\n        println(\"nope\")/unit\n    fn/\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "");
    }

    #[test]
    fn array_indexing_and_assignment() {
        let src = "fn new main/\n    let mut a = [1, 2, 3]/array\n    a[0] = 10/i64\n    println(\"{} {} {}\", a[0], a[1], a[2])/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "10 2 3\n");
    }

    #[test]
    fn builtin_len_on_array() {
        let src =
            "fn new main/\n    let a = [\"x\", \"y\"]/array\n    println(\"{}\", len(a))/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "2\n");
    }

    #[test]
    fn float_literal_and_math() {
        let src = "fn new main/\n    let x = 1.5/f64\n    let y = 2.0/f64\n    println(\"{}\", x + y)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "3.5\n");
    }

    #[test]
    fn char_and_bool_literals() {
        let src = "fn new main/\n    let c = 'a'/char\n    let b = true/bool\n    println(\"{} {}\", c, b)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "a true\n");
    }

    #[test]
    fn unsigned_and_usize() {
        let src = "fn new main/\n    let u = 255/u8\n    let s = 123456/usize\n    println(\"{} {}\", u, s)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "255 123456\n");
    }

    #[test]
    fn builtin_push_and_pop() {
        let src = "fn new main/\n    let mut a = [1, 2]/array\n    push(a, 3)/unit\n    println(\"{}\", a[2])/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "3\n");
    }

    #[test]
    fn typed_function_params_and_return() {
        let src = "fn new add(a/i64, b/i64)/i64\n    return a + b/i64\nfn/\nfn new main/\n    let y = add(1, 2)/i64\n    println(\"{}\", y)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "3\n");
    }

    #[test]
    fn typed_function_param_mismatch_errors() {
        let src = "fn new add(a/i64, b/i64)/i64\n    return a + b/i64\nfn/\nfn new main/\n    add(\"x\", 2)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        assert!(it.run_program(&prog).is_err());
    }

    #[test]
    fn function_return_type_enforced() {
        let src = "fn new bad()/i64\n    return \"oops\"/str\nfn/\nfn new main/\n    let _ = bad()/i64\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        assert!(it.run_program(&prog).is_err());
    }

    #[test]
    fn recursion_factorial() {
        let src = "fn new fact(n)/i64\n    if n == 0/\n        return 1/i64\n    fn/\n    return n * fact(n - 1)/i64\nfn/\nfn new main/\n    let y = fact(5)/i64\n    println(\"{}\", y)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "120\n");
    }

    #[test]
    fn recursion_fibonacci() {
        let src = "fn new fib(n)/i64\n    if n == 0/\n        return 0/i64\n    fn/\n    if n == 1/\n        return 1/i64\n    fn/\n    return fib(n - 1) + fib(n - 2)/i64\nfn/\nfn new main/\n    let y = fib(6)/i64\n    println(\"{}\", y)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "8\n");
    }

    #[test]
    fn builtin_pop_returns_value() {
        let src = "fn new main/\n    let mut a = [1, 2]/array\n    let v = pop(a)/i64\n    println(\"{}\", v)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "2\n");
    }

    #[test]
    fn pop_empty_array_error() {
        let src = "fn new main/\n    let mut a = []/array\n    let v = pop(a)/i64\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        assert!(it.run_program(&prog).is_err());
    }

    #[test]
    fn push_requires_mutable() {
        let src = "fn new main/\n    let a = [1]/array\n    push(a, 2)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        assert!(it.run_program(&prog).is_err());
    }

    #[test]
    fn push_type_mismatch_errors() {
        let src = "fn new main/\n    let mut a = [1]/array\n    push(a, \"x\")/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        assert!(it.run_program(&prog).is_err());
    }

    #[test]
    fn push_sets_type_on_empty_array() {
        let src = "fn new main/\n    let mut a = []/array\n    push(a, 10)/unit\n    let v = pop(a)/i64\n    println(\"{}\", v)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "10\n");
    }

    #[test]
    fn assign_index_type_mismatch_errors() {
        let src = "fn new main/\n    let mut a = [1]/array\n    a[0] = \"x\"/str\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        assert!(it.run_program(&prog).is_err());
    }

    #[test]
    fn for_loop_range_sum() {
        let src = "fn new main/\n    let mut s = 0/i64\n    for i in range(0, 5)/\n        s = s + i/i64\n    fn/\n    println(\"{}\", s)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "10\n");
    }

    #[test]
    fn for_loop_array_iter() {
        let src = "fn new main/\n    let mut s = 0/i64\n    for v in [1, 2, 3]/\n        s = s + v/i64\n    fn/\n    println(\"{}\", s)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "6\n");
    }

    #[test]
    fn for_loop_break_continue() {
        let src = "fn new main/\n    let mut s = 0/i64\n    for i in range(0, 5)/\n        if i == 3/\n            break/\n        fn/\n        if i == 1/\n            continue/\n        fn/\n        s = s + i/i64\n    fn/\n    println(\"{}\", s)/unit\nfn/";
        let prog = parse(src).expect("parse failed");
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");

        assert_eq!(it.output(), "2\n");
    }

    #[test]
    fn example_brainfuck_runs() {
        let src = r#"fn new run_bf(code)/
    let mut cmds = []/array
    for c in code/
        push(cmds, c)/unit
    fn/

    let mut tape = []/array
    push(tape, 0)/unit
    let mut ptr = 0/i64
    let mut ip = 0/i64

    let mut n = len(cmds)/i64
    while ip < n/
        let cmd = cmds[ip]/char

        if cmd == '>'/
            ptr = ptr + 1/i64
            while ptr >= len(tape)/i64/
                push(tape, 0)/unit
            fn/
        fn/

        if cmd == '<'/
            if ptr == 0/i64/
                ip = n/i64
            fn/
            ptr = ptr - 1/i64
        fn/

        if cmd == '+'/
            let v = tape[ptr]/i64
            tape[ptr] = v + 1/i64
        fn/

        if cmd == '-'/
            let v = tape[ptr]/i64
            tape[ptr] = v - 1/i64
        fn/

        if cmd == '.'/
            println("{}", tape[ptr])/unit
        fn/

        if cmd == ','/
            fn/

        if cmd == '['/
            if tape[ptr] == 0/i64/
                let mut depth = 1/i64
                let mut j = ip + 1/i64
                while j < n/
                    let c2 = cmds[j]/char
                    if c2 == '['/
                        depth = depth + 1/i64
                    fn/
                    if c2 == ']'/
                        depth = depth - 1/i64
                        if depth == 0/i64/
                            ip = j/i64
                            break/
                        fn/
                    fn/
                    j = j + 1/i64
                fn/
            fn/
        fn/

        if cmd == ']'/
            if !(tape[ptr] == 0)/
                let mut depth = 1/i64
                let mut j = ip - 1/i64
                while j >= 0/i64/
                    let c2 = cmds[j]/char
                    if c2 == ']'/
                        depth = depth + 1/i64
                    fn/
                    if c2 == '['/
                        depth = depth - 1/i64
                        if depth == 0/i64/
                            ip = j/i64
                            break/
                        fn/
                    fn/
                    j = j - 1/i64
                fn/
            fn/
        fn/

        ip = ip + 1/i64
    fn/
fn/

fn new main/
    run_bf("++++++++++.")/unit
fn/"#
            .to_string();
        let prog = match crate::frontend::parse(&src) {
            Ok(p) => p,
            Err(e) => {
                let mut lx = crate::syntax::Lexer::new(&src);
                let mut toks = Vec::new();
                loop {
                    let t = lx.next_token();
                    toks.push(format!("{:?}", t));
                    if let crate::syntax::Token::Eof = t {
                        break;
                    }
                }
                panic!("parse failed: {}\nTOKENS:\n{}", e, toks.join("\n"));
            }
        };
        let mut it = Interpreter::new();
        it.run_program(&prog).expect("runtime error");
        assert_eq!(it.output(), "10\n");
    }
}
