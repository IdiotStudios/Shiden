use crate::compiler::types::*;
use std::collections::HashMap;

pub struct CodeGen {
    bytecode: Vec<u8>,
    label_counter: usize,
    context: CompileContext,
    function_offsets: HashMap<String, usize>,
    structs: HashMap<String, StructDef>,
    struct_offsets: HashMap<String, Vec<(String, usize)>>,
}

impl CodeGen {
    pub fn new(max_locals: usize) -> Self {
        CodeGen {
            bytecode: Vec::new(),
            label_counter: 0,
            context: CompileContext::new(max_locals),
            function_offsets: HashMap::new(),
            structs: HashMap::new(),
            struct_offsets: HashMap::new(),
        }
    }

    fn register_struct(&mut self, struct_def: &StructDef) {
        let mut field_offsets = Vec::new();
        let mut current_offset = 0;

        for (field_name, _field_type) in &struct_def.fields {
            field_offsets.push((field_name.clone(), current_offset));
            current_offset += 8;
        }

        self.structs
            .insert(struct_def.name.clone(), struct_def.clone());
        self.struct_offsets
            .insert(struct_def.name.clone(), field_offsets);
    }

    fn get_field_offset(&self, struct_name: &str, field_name: &str) -> Option<usize> {
        self.struct_offsets.get(struct_name).and_then(|fields| {
            fields
                .iter()
                .find(|(name, _)| name == field_name)
                .map(|(_, offset)| *offset)
        })
    }

    fn get_struct_size(&self, struct_name: &str) -> usize {
        if let Some(struct_def) = self.structs.get(struct_name) {
            struct_def.fields.len() * 8
        } else {
            0
        }
    }

    fn next_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn emit_byte(&mut self, byte: u8) {
        self.bytecode.push(byte);
    }

    fn emit_bytes(&mut self, bytes: &[u8]) {
        self.bytecode.extend_from_slice(bytes);
    }

    fn emit_i32(&mut self, val: i32) {
        self.emit_bytes(&val.to_le_bytes());
    }

    fn emit_i64(&mut self, val: i64) {
        self.emit_bytes(&val.to_le_bytes());
    }

    fn emit_mov_imm64(&mut self, reg: u8, imm: i64) {
        if imm >= i32::MIN as i64 && imm <= i32::MAX as i64 {
            self.emit_byte(0x48);
            self.emit_byte(0xC7);
            self.emit_byte(0xC0 | reg);
            self.emit_i32(imm as i32);
        } else {
            self.emit_byte(0x48);
            self.emit_byte(0xB8 | reg);
            self.emit_i64(imm);
        }
    }

    fn emit_mov_from_stack(&mut self, reg: u8, offset: usize) {
        self.emit_byte(0x48);
        self.emit_byte(0x8B);
        self.emit_byte(0x45);
        self.emit_byte(offset as u8);
    }

    fn emit_mov_to_stack(&mut self, reg: u8, offset: usize) {
        self.emit_byte(0x48);
        self.emit_byte(0x89);
        self.emit_byte(0x45);
        self.emit_byte(offset as u8);
    }

    fn emit_add(&mut self) {
        self.emit_bytes(&[0x48, 0x01, 0xF8]);
    }

    fn emit_sub(&mut self) {
        self.emit_bytes(&[0x48, 0x29, 0xF8]);
    }

    fn emit_imul(&mut self) {
        self.emit_bytes(&[0x48, 0x0F, 0xAF, 0xC7]);
    }

    fn emit_div(&mut self) {
        self.emit_bytes(&[0x48, 0x99]);

        self.emit_bytes(&[0x48, 0xF7, 0xFF]);
    }

    fn emit_mod(&mut self) {
        self.emit_bytes(&[0x48, 0x99]);

        self.emit_bytes(&[0x48, 0xF7, 0xFF]);

        self.emit_bytes(&[0x48, 0x89, 0xD0]);
    }

    fn emit_cmp(&mut self) {
        self.emit_bytes(&[0x48, 0x39, 0xF8]);
    }

    fn emit_jl(&mut self, offset: i8) {
        self.emit_byte(0x7C);
        self.emit_byte(offset as u8);
    }

    fn emit_jg(&mut self, offset: i8) {
        self.emit_byte(0x7F);
        self.emit_byte(offset as u8);
    }

    fn emit_jle(&mut self, offset: i8) {
        self.emit_byte(0x7E);
        self.emit_byte(offset as u8);
    }

    fn emit_jge(&mut self, offset: i8) {
        self.emit_byte(0x7D);
        self.emit_byte(offset as u8);
    }

    fn emit_je(&mut self, offset: i8) {
        self.emit_byte(0x74);
        self.emit_byte(offset as u8);
    }

    fn emit_jne(&mut self, offset: i8) {
        self.emit_byte(0x75);
        self.emit_byte(offset as u8);
    }

    fn emit_ret(&mut self) {
        self.emit_byte(0xC3);
    }

    fn emit_push_rbp(&mut self) {
        self.emit_byte(0x55);
    }

    fn emit_pop_rbp(&mut self) {
        self.emit_byte(0x5D);
    }

    fn emit_mov_rbp_rsp(&mut self) {
        self.emit_bytes(&[0x48, 0x89, 0xE5]);
    }

    fn emit_sub_rsp(&mut self, imm: u8) {
        self.emit_bytes(&[0x48, 0x83, 0xEC, imm]);
    }

    fn emit_add_rsp(&mut self, imm: u8) {
        self.emit_bytes(&[0x48, 0x83, 0xC4, imm]);
    }

    pub fn compile_function(&mut self, func: &Function) -> Result<Vec<u8>, String> {
        self.context.clear_bindings();
        self.context.current_function = Some(func.name.clone());
        self.bytecode.clear();

        self.emit_push_rbp();
        self.emit_mov_rbp_rsp();

        let frame_size = func.params.len() * 8 + 32;
        if frame_size > 0 {
            self.emit_sub_rsp(frame_size as u8);
        }

        for (i, (param_name, param_type)) in func.params.iter().enumerate() {
            let reg = self.get_param_register(i);
            let offset = 8 + (i * 8);

            self.emit_mov_to_stack(reg, offset);

            self.context
                .push_binding(param_name.clone(), param_type.clone(), false)?;
        }

        for stmt in &func.body {
            self.compile_stmt(stmt)?;
        }

        if func.ret_type == Type::Void {
            self.emit_mov_imm64(0, 0);
        }

        if frame_size > 0 {
            self.emit_add_rsp(frame_size as u8);
        }
        self.emit_pop_rbp();
        self.emit_ret();

        Ok(self.bytecode.clone())
    }

    fn get_param_register(&self, index: usize) -> u8 {
        match index {
            0 => 7,
            1 => 6,
            2 => 2,
            3 => 1,
            4 => 8,
            5 => 9,
            _ => 7,
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let {
                var,
                mutable,
                value,
            } => {
                self.compile_expr(value)?;

                let value_type = self.infer_expr_type(value)?;
                let offset = self
                    .context
                    .push_binding(var.clone(), value_type, *mutable)?;
                self.emit_mov_to_stack(0, offset);
                Ok(())
            }
            Stmt::Assign { var, value } => {
                self.compile_expr(value)?;
                let binding = self
                    .context
                    .find_binding(var)
                    .ok_or_else(|| format!("Undefined variable: {}", var))?;
                if !binding.mutable {
                    return Err(format!("Cannot assign to immutable variable: {}", var));
                }
                let offset = binding.stack_offset;
                self.emit_mov_to_stack(0, offset);
                Ok(())
            }
            Stmt::FieldAssign {
                object,
                field,
                value,
            } => {
                let obj_type = self.infer_expr_type(object)?;

                let struct_name = match obj_type {
                    Type::Struct(name) => name,
                    _ => return Err("Can only assign to fields of structs".to_string()),
                };

                let field_offset = self.get_field_offset(&struct_name, field).ok_or_else(|| {
                    format!("Unknown field '{}' in struct '{}'", field, struct_name)
                })?;

                self.compile_expr(value)?;

                match object.as_ref() {
                    Expr::Self_ => {
                        let self_binding = self
                            .context
                            .find_binding("self")
                            .ok_or("self not in scope")?;

                        if !self_binding.mutable {
                            return Err("Cannot mutate field of immutable self".to_string());
                        }

                        let self_offset = self_binding.stack_offset;
                        let total_offset = self_offset + field_offset;

                        self.emit_bytes(&[0x48, 0x89, 0x85]);
                        self.emit_i32(-(total_offset as i32));
                    }
                    Expr::Var(var_name) => {
                        let binding = self
                            .context
                            .find_binding(var_name)
                            .ok_or_else(|| format!("Undefined variable: {}", var_name))?;

                        if !binding.mutable {
                            return Err(format!(
                                "Cannot mutate field of immutable variable: {}",
                                var_name
                            ));
                        }

                        let var_offset = binding.stack_offset;
                        let total_offset = var_offset + field_offset;

                        self.emit_bytes(&[0x48, 0x89, 0x85]);
                        self.emit_i32(-(total_offset as i32));
                    }
                    _ => {
                        return Err(
                            "Field assignment only supported for variables and self".to_string()
                        );
                    }
                }

                Ok(())
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                Ok(())
            }
            Stmt::Return(expr) => {
                self.compile_expr(expr)?;

                self.emit_bytes(&[0x48, 0x89, 0xEC]);

                self.emit_byte(0x5D);

                self.emit_byte(0xC3);

                Ok(())
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                self.compile_expr(cond)?;
                let else_label = self.next_label();
                let end_label = self.next_label();

                let jne_offset = self.bytecode.len();
                self.emit_jne(0);

                for stmt in then_block {
                    self.compile_stmt(stmt)?;
                }

                if let Some(else_stmts) = else_block {
                    let jmp_offset = self.bytecode.len();
                    self.emit_byte(0xEB);
                    self.emit_byte(0);

                    for stmt in else_stmts {
                        self.compile_stmt(stmt)?;
                    }

                    self.bytecode[jne_offset + 1] = (self.bytecode.len() - jne_offset - 2) as u8;
                } else {
                    self.bytecode[jne_offset + 1] = (self.bytecode.len() - jne_offset - 2) as u8;
                }

                Ok(())
            }
            Stmt::While { cond, body } => {
                let loop_start = self.bytecode.len();

                self.compile_expr(cond)?;
                let jne_offset = self.bytecode.len();
                self.emit_jne(0);

                for stmt in body {
                    self.compile_stmt(stmt)?;
                }

                let jmp_offset = self.bytecode.len();
                self.emit_byte(0xE9);
                let rel_offset = loop_start as i32 - (jmp_offset as i32 + 5);
                self.emit_i32(rel_offset);

                self.bytecode[jne_offset + 1] = (self.bytecode.len() - jne_offset - 2) as u8;

                Ok(())
            }
            Stmt::For {
                var: _var,
                iter,
                body,
            } => {
                eprintln!("⚠️  For loops not yet fully implemented in bytecode compiler");

                self.compile_expr(iter)?;

                for stmt in body {
                    self.compile_stmt(stmt)?;
                }

                Ok(())
            }
        }
    }

    fn infer_expr_type(&self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::Literal(_) => Ok(Type::I64),
            Expr::Var(name) => self
                .context
                .find_binding(name)
                .map(|b| b.typ.clone())
                .ok_or_else(|| format!("Undefined variable: {}", name)),
            Expr::StructInit { struct_name, .. } => Ok(Type::Struct(struct_name.clone())),
            Expr::EnumVariant { enum_name, .. } => Ok(Type::Enum(enum_name.clone())),
            Expr::Binary { .. } => Ok(Type::I64),
            Expr::Unary { .. } => Ok(Type::I64),
            Expr::Call { .. } => Ok(Type::I64),
            Expr::If { then_val, .. } => self.infer_expr_type(then_val),
            Expr::FieldAccess { .. } => Ok(Type::I64),
            Expr::Match { arms, .. } => {
                if let Some((_variant, _binding, body_expr)) = arms.first() {
                    self.infer_expr_type(body_expr)
                } else {
                    Ok(Type::I64)
                }
            }
            Expr::MethodCall { .. } => Ok(Type::I64),
            Expr::AssociatedCall { type_name, .. } => Ok(Type::Struct(type_name.clone())),
            Expr::Self_ => self
                .context
                .find_binding("self")
                .map(|b| b.typ.clone())
                .ok_or_else(|| "self not available in this context".to_string()),
        }
    }

    fn resolve_method(&self, struct_name: &str, method_name: &str) -> Result<MethodDef, String> {
        let struct_def = self
            .structs
            .get(struct_name)
            .ok_or_else(|| format!("Unknown struct: {}", struct_name))?;

        struct_def
            .methods
            .iter()
            .find(|m| m.name == method_name)
            .ok_or_else(|| {
                format!(
                    "Unknown method '{}' on struct '{}'",
                    method_name, struct_name
                )
            })
            .map(|m| m.clone())
    }

    fn validate_method_call(method: &MethodDef, args: &[Expr]) -> Result<(), String> {
        let expected_params = if method
            .params
            .first()
            .map_or(false, |(name, _)| name == "self")
        {
            method.params.len() - 1
        } else {
            method.params.len()
        };

        if args.len() != expected_params {
            return Err(format!(
                "Method '{}' expects {} arguments, got {}",
                method.name,
                expected_params,
                args.len()
            ));
        }

        Ok(())
    }

    fn resolve_associated_function(
        &self,
        type_name: &str,
        func_name: &str,
    ) -> Result<Function, String> {
        let struct_def = self
            .structs
            .get(type_name)
            .ok_or_else(|| format!("Unknown type: {}", type_name))?;

        struct_def
            .associated_functions
            .iter()
            .find(|f| f.name == func_name)
            .ok_or_else(|| {
                format!(
                    "Unknown associated function '{}' on type '{}'",
                    func_name, type_name
                )
            })
            .map(|f| f.clone())
    }

    fn validate_function_call(func: &Function, args: &[Expr]) -> Result<(), String> {
        if args.len() != func.params.len() {
            return Err(format!(
                "Function '{}' expects {} arguments, got {}",
                func.name,
                func.params.len(),
                args.len()
            ));
        }
        Ok(())
    }

    fn resolve_self_type(&self, typ: &Type, struct_context: Option<&str>) -> Type {
        match typ {
            Type::Self_ => {
                if let Some(struct_name) = struct_context {
                    Type::Struct(struct_name.to_string())
                } else {
                    Type::Self_
                }
            }
            _ => typ.clone(),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Literal(val) => {
                self.emit_mov_imm64(0, *val);
                Ok(())
            }
            Expr::Var(name) => {
                let binding = self
                    .context
                    .find_binding(name)
                    .ok_or_else(|| format!("Undefined variable: {}", name))?;
                self.emit_mov_from_stack(0, binding.stack_offset);
                Ok(())
            }
            Expr::Binary { left, op, right } => {
                self.compile_expr(left)?;

                self.emit_byte(0x50);

                self.compile_expr(right)?;

                self.emit_byte(0x5F);

                match op {
                    BinOp::Add => self.emit_add(),
                    BinOp::Sub => self.emit_sub(),
                    BinOp::Mul => self.emit_imul(),
                    BinOp::Div => self.emit_div(),
                    BinOp::Mod => self.emit_mod(),
                    BinOp::Lt => {
                        self.emit_cmp();
                        self.emit_byte(0x0F);
                        self.emit_byte(0x9C);
                        self.emit_byte(0xC0);
                    }
                    BinOp::Gt => {
                        self.emit_cmp();
                        self.emit_byte(0x0F);
                        self.emit_byte(0x9F);
                        self.emit_byte(0xC0);
                    }
                    BinOp::Lte => {
                        self.emit_cmp();
                        self.emit_byte(0x0F);
                        self.emit_byte(0x9E);
                        self.emit_byte(0xC0);
                    }
                    BinOp::Gte => {
                        self.emit_cmp();
                        self.emit_byte(0x0F);
                        self.emit_byte(0x9D);
                        self.emit_byte(0xC0);
                    }
                    BinOp::Eq => {
                        self.emit_cmp();
                        self.emit_byte(0x0F);
                        self.emit_byte(0x94);
                        self.emit_byte(0xC0);
                    }
                    BinOp::Neq => {
                        self.emit_cmp();
                        self.emit_byte(0x0F);
                        self.emit_byte(0x95);
                        self.emit_byte(0xC0);
                    }
                    _ => return Err(format!("Unimplemented binary op: {:?}", op)),
                }

                Ok(())
            }
            Expr::Unary { op, expr } => {
                self.compile_expr(expr)?;
                match op {
                    UnOp::Neg => {
                        self.emit_bytes(&[0xF7, 0xD8]);
                    }
                    UnOp::Not => {
                        self.emit_bytes(&[0xF7, 0xD0]);
                    }
                }
                Ok(())
            }
            Expr::Call { func, args } => {
                for (i, arg) in args.iter().enumerate() {
                    self.compile_expr(arg)?;
                    if i == 0 {
                        self.emit_bytes(&[0x48, 0x89, 0xC7]);
                    } else if i == 1 {
                        self.emit_bytes(&[0x48, 0x89, 0xC6]);
                    }
                }

                Ok(())
            }
            Expr::If {
                cond,
                then_val,
                else_val,
            } => {
                self.compile_expr(cond)?;
                let else_label = self.next_label();
                let end_label = self.next_label();

                self.emit_bytes(&[0x48, 0x85, 0xC0]);
                let jne_offset = self.bytecode.len();
                self.emit_jne(0);

                self.compile_expr(then_val)?;
                let jmp_offset = self.bytecode.len();
                self.emit_byte(0xEB);
                self.emit_byte(0);

                self.bytecode[jne_offset + 1] = (self.bytecode.len() - jne_offset - 2) as u8;
                self.compile_expr(else_val)?;

                self.bytecode[jmp_offset + 1] = (self.bytecode.len() - jmp_offset - 2) as u8;

                Ok(())
            }
            Expr::FieldAccess { object, field } => {
                if let Expr::Var(var_name) = &**object {
                    let binding = self
                        .context
                        .find_binding(var_name)
                        .ok_or_else(|| format!("Undefined variable: {}", var_name))?;

                    if let Type::Struct(struct_name) = &binding.typ {
                        if let Some(field_offset) = self.get_field_offset(struct_name, field) {
                            let total_offset = binding.stack_offset + field_offset;

                            self.emit_bytes(&[0x48, 0x8B, 0x85]);
                            let disp = total_offset as i32;
                            self.emit_i32(-disp);
                            return Ok(());
                        } else {
                            return Err(format!(
                                "Unknown field '{}' in struct '{}'",
                                field, struct_name
                            ));
                        }
                    }
                }

                self.compile_expr(object)?;
                eprintln!("⚠️  Field access on complex expressions not fully implemented");
                Ok(())
            }
            Expr::StructInit {
                struct_name,
                fields: init_fields,
            } => {
                if !self.structs.contains_key(struct_name) {
                    return Err(format!("Unknown struct: {}", struct_name));
                }

                let struct_def = self.structs.get(struct_name).unwrap().clone();

                for (field_name, _field_type) in &struct_def.fields {
                    if let Some((_name, field_expr)) =
                        init_fields.iter().find(|(n, _)| n == field_name)
                    {
                        self.compile_expr(field_expr)?;
                    } else {
                        self.emit_mov_imm64(0, 0);
                    }
                }

                eprintln!("⚠️  Struct init partially implemented, returns last field value");
                Ok(())
            }
            Expr::EnumVariant {
                enum_name,
                variant,
                value,
            } => {
                let variant_idx = if let Some(enum_def) = self.structs.get(enum_name) {
                    0
                } else {
                    return Err(format!("Unknown enum: {}", enum_name));
                };

                if let Some(val_expr) = value {
                    self.compile_expr(val_expr)?;
                } else {
                    self.emit_mov_imm64(0, variant_idx as i64);
                }

                eprintln!("⚠️  Enum variants partially implemented");
                Ok(())
            }
            Expr::Match { expr, arms } => {
                self.compile_expr(expr)?;

                let mut arm_labels = Vec::new();

                for _i in 0..arms.len() {
                    arm_labels.push(self.next_label());
                }
                let _end_label = self.next_label();

                for (i, (_variant, _binding, body_expr)) in arms.iter().enumerate() {
                    eprintln!("⚠️  Match statement partially implemented - executes first arm");

                    if i == 0 {
                        self.compile_expr(body_expr)?;
                    }
                }
                let _ = arm_labels;

                Ok(())
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let receiver_type = self.infer_expr_type(receiver)?;

                let struct_name = match &receiver_type {
                    Type::Struct(name) => name.clone(),
                    _ => {
                        return Err(format!(
                            "Cannot call method '{}' on non-struct type",
                            method
                        ));
                    }
                };

                let method_def = self.resolve_method(&struct_name, method)?;

                Self::validate_method_call(&method_def, args)?;

                self.compile_expr(receiver)?;

                self.emit_byte(0x50);

                for arg in args {
                    self.compile_expr(arg)?;
                    self.emit_byte(0x50);
                }

                let self_offset = 8 + (self.context.bindings.len() * 8);
                self.context.push_binding(
                    "self".to_string(),
                    Type::Struct(struct_name.clone()),
                    true,
                )?;

                for stmt in &method_def.body {
                    self.compile_stmt(stmt)?;
                }

                self.context.bindings.pop();

                eprintln!(
                    "✓ Method call '{}' on '{}' resolved and compiled",
                    method, struct_name
                );
                Ok(())
            }
            Expr::Self_ => {
                self.emit_bytes(&[0x48, 0x8B, 0x85]);
                self.emit_i32(-16);
                Ok(())
            }
            Expr::AssociatedCall {
                type_name,
                function,
                args,
            } => {
                let func_def = self.resolve_associated_function(type_name, function)?;

                Self::validate_function_call(&func_def, args)?;

                for arg in args.iter().rev() {
                    self.compile_expr(arg)?;
                    self.emit_byte(0x50);
                }

                for (i, (param_name, param_type)) in func_def.params.iter().enumerate() {
                    let offset = 8 + (i * 8);
                    self.context.bindings.push(Binding {
                        name: param_name.clone(),
                        typ: param_type.clone(),
                        stack_offset: offset,
                        mutable: false,
                    });
                }

                for stmt in &func_def.body {
                    self.compile_stmt(stmt)?;
                }

                for _ in 0..func_def.params.len() {
                    self.context.bindings.pop();
                }

                eprintln!(
                    "✓ Associated function '{}::{}' resolved and compiled",
                    type_name, function
                );
                Ok(())
            }
        }
    }
}

pub fn compile_function_to_bytecode(
    func: &Function,
    structs: &[StructDef],
    max_locals: usize,
) -> Result<Vec<u8>, String> {
    let mut codegen = CodeGen::new(max_locals);

    for struct_def in structs {
        codegen.register_struct(struct_def);
    }

    codegen.compile_function(func)
}
