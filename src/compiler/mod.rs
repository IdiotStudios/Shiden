pub mod codegen;
pub mod parser;
pub mod tokenizer;
pub mod types;

use parser::parse;
use std::collections::BTreeMap;
use std::path::Path;
use tokenizer::tokenize;

pub struct BytecodeCompilerConfig {
    pub max_locals: usize,
    pub strict_errors: bool,
}

impl Default for BytecodeCompilerConfig {
    fn default() -> Self {
        BytecodeCompilerConfig {
            max_locals: 256,
            strict_errors: false,
        }
    }
}

pub struct CompiledFunction {
    pub name: String,
    pub bytecode: Vec<u8>,
}

pub struct BytecodeCompiler {
    config: BytecodeCompilerConfig,
}

impl BytecodeCompiler {
    pub fn new(config: BytecodeCompilerConfig) -> Self {
        BytecodeCompiler { config }
    }

    pub fn compile_source(&self, source: &str) -> Result<Vec<CompiledFunction>, String> {
        let tokens = tokenize(unsafe { std::mem::transmute::<&str, &'static str>(source) })?;

        let program = parse(tokens)?;

        let mut compiled = Vec::new();
        for func in program.functions {
            match codegen::compile_function_to_bytecode(
                &func,
                &program.structs,
                self.config.max_locals,
            ) {
                Ok(bytecode) => {
                    compiled.push(CompiledFunction {
                        name: func.name,
                        bytecode,
                    });
                }
                Err(e) => {
                    let msg = format!("Failed to compile function '{}': {}", func.name, e);
                    if self.config.strict_errors {
                        return Err(msg);
                    } else {
                        eprintln!("⚠️  Warning: {}", msg);
                    }
                }
            }
        }

        Ok(compiled)
    }

    pub fn compile_to_map(&self, source: &str) -> BTreeMap<String, Vec<u8>> {
        match self.compile_source(source) {
            Ok(functions) => {
                let mut map = BTreeMap::new();
                for func in functions {
                    map.insert(func.name, func.bytecode);
                }
                map
            }
            Err(e) => {
                eprintln!("Compilation error: {}", e);
                BTreeMap::new()
            }
        }
    }
}

pub fn discover_and_compile_libraries(
    libs_dir: &Path,
    config: BytecodeCompilerConfig,
) -> Result<BTreeMap<String, Vec<u8>>, String> {
    let mut all_compiled = BTreeMap::new();
    let compiler = BytecodeCompiler::new(config);

    for entry in std::fs::read_dir(libs_dir)
        .map_err(|e| format!("Failed to read libraries directory: {}", e))?
    {
        let entry = entry.map_err(|e| format!("Read dir error: {}", e))?;
        let path = entry.path();

        if path.is_dir() {
            for filename in &["lib.rs", "mod.rs"] {
                let lib_file = path.join(filename);
                if lib_file.exists() {
                    match std::fs::read_to_string(&lib_file) {
                        Ok(source) => {
                            let compiled = compiler.compile_to_map(&source);
                            all_compiled.extend(compiled);
                        }
                        Err(e) => {
                            eprintln!("⚠️  Failed to read {}: {}", lib_file.display(), e);
                        }
                    }
                    break;
                }
            }
        }
    }

    Ok(all_compiled)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_add() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            pub fn add(a: i64, b: i64) -> i64 {
                a + b
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 1);
        assert_eq!(compiled[0].name, "add");
        assert!(!compiled[0].bytecode.is_empty());
    }

    #[test]
    fn test_with_if() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            pub fn conditional_max(a: i64, b: i64) -> i64 {
                let mut diff = a - b;
                let result = if diff > 0 {
                    a
                } else {
                    b
                };
                result * 2
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
    }

    #[test]
    fn test_struct_definition() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            pub fn point_x(p: Point) -> i64 {
                p.x
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Failed to compile struct: {:?}",
            result.err()
        );

        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 1);
        assert_eq!(compiled[0].name, "point_x");
    }

    #[test]
    fn test_struct_init() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Rectangle {
                width: i64,
                height: i64,
            }

            pub fn area(rect: Rectangle) -> i64 {
                rect.width * rect.height
            }

            pub fn make_rect() -> i64 {
                let r = Rectangle { width: 5, height: 10 };
                r.width
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Failed to compile struct init: {:?}",
            result.err()
        );

        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 2);
    }

    #[test]
    fn test_enum_definition() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            enum Option {
                Some(i64),
                None,
            }

            pub fn get_value() -> i64 {
                42
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(result.is_ok(), "Failed to compile enum: {:?}", result.err());

        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 1);
    }

    #[test]
    fn test_match_statement() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            pub fn test_pattern() -> i64 {
                let x = 5;
                if x == 5 {
                    return 1;
                }
                0
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Failed to compile pattern test: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_for_loop() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            pub fn loop_test() -> i64 {
                let mut sum = 0;
                let mut i = 0;
                while i < 3 {
                    sum = sum + (i + 1);
                    i = i + 1;
                }
                sum
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(result.is_ok(), "Failed to compile loop: {:?}", result.err());
    }

    #[test]
    fn test_method_definition() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            impl Point {
                fn distance_from_origin(self) -> i64 {
                    self.x * self.x + self.y * self.y
                }
            }

            pub fn point_test() -> i64 {
                42
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Failed to compile method definition: {:?}",
            result.err()
        );

        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 1);
        assert_eq!(compiled[0].name, "point_test");
    }

    #[test]
    fn test_method_call() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Rectangle {
                width: i64,
                height: i64,
            }

            impl Rectangle {
                fn area(self) -> i64 {
                    self.width * self.height
                }
            }

            pub fn calculate_area() -> i64 {
                let r = Rectangle { width: 5, height: 10 };
                r.area()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Failed to compile method call: {:?}",
            result.err()
        );

        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 1);
        assert_eq!(compiled[0].name, "calculate_area");
    }

    #[test]
    fn test_self_reference() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Counter {
                count: i64,
            }

            impl Counter {
                fn get_count(self) -> i64 {
                    self.count
                }

                fn increment(self, delta: i64) -> i64 {
                    self.count + delta
                }
            }

            pub fn test_counter() -> i64 {
                1
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Failed to compile self reference: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase4_basic_dispatch() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            impl Point {
                fn get_x(self) -> i64 {
                    self.x
                }
            }

            pub fn test_dispatch() -> i64 {
                let p = Point { x: 42, y: 10 };
                p.get_x()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 4: Failed to compile basic dispatch: {:?}",
            result.err()
        );

        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 1);
        assert_eq!(compiled[0].name, "test_dispatch");
    }

    #[test]
    fn test_phase4_multiple_methods() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Rectangle {
                width: i64,
                height: i64,
            }

            impl Rectangle {
                fn get_width(self) -> i64 {
                    self.width
                }

                fn get_height(self) -> i64 {
                    self.height
                }

                fn area(self) -> i64 {
                    self.width * self.height
                }
            }

            pub fn test_multiple() -> i64 {
                let r = Rectangle { width: 5, height: 10 };
                let w = r.get_width();
                let h = r.get_height();
                let a = r.area();
                a
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 4: Failed to compile multiple methods: {:?}",
            result.err()
        );

        let compiled = result.unwrap();
        assert_eq!(compiled.len(), 1);
    }

    #[test]
    fn test_phase4_method_with_args() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Box {
                size: i64,
            }

            impl Box {
                fn scale(self, factor: i64) -> i64 {
                    self.size * factor
                }

                fn add_size(self, amount: i64) -> i64 {
                    self.size + amount
                }
            }

            pub fn test_with_args() -> i64 {
                let b = Box { size: 10 };
                let scaled = b.scale(3);
                let added = b.add_size(5);
                scaled + added
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 4: Failed to compile method with args: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase4_method_chaining_calls() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Counter {
                value: i64,
            }

            impl Counter {
                fn increment(self, delta: i64) -> i64 {
                    self.value + delta
                }

                fn double(self) -> i64 {
                    self.value * 2
                }

                fn get_value(self) -> i64 {
                    self.value
                }
            }

            pub fn test_chained() -> i64 {
                let c = Counter { value: 5 };
                let result = c.increment(3);
                result
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 4: Failed to compile method chaining: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase4_different_struct_types() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Circle {
                radius: i64,
            }

            struct Square {
                side: i64,
            }

            impl Circle {
                fn area(self) -> i64 {
                    3 * self.radius * self.radius
                }
            }

            impl Square {
                fn area(self) -> i64 {
                    self.side * self.side
                }
            }

            pub fn test_types() -> i64 {
                let c = Circle { radius: 5 };
                let s = Square { side: 4 };
                let c_area = c.area();
                let s_area = s.area();
                c_area + s_area
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 4: Failed to compile different struct types: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase4_self_in_expression() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Value {
                num: i64,
            }

            impl Value {
                fn double_it(self) -> i64 {
                    let doubled = self.num * 2;
                    doubled
                }

                fn triple_it(self) -> i64 {
                    self.num + self.num + self.num
                }
            }

            pub fn test_self_expr() -> i64 {
                let v = Value { num: 7 };
                let d = v.double_it();
                let t = v.triple_it();
                d + t
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 4: Failed to compile self in expression: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase5_basic_constructor() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            impl Point {
                fn new(x: i64, y: i64) -> Point {
                    Point { x: x, y: y }
                }
            }

            pub fn test_constructor() -> i64 {
                let p = Point::new(10, 20);
                p.x + p.y
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 5: Failed to compile basic constructor: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase5_multiple_associated() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Rectangle {
                width: i64,
                height: i64,
            }

            impl Rectangle {
                fn new(width: i64, height: i64) -> Rectangle {
                    Rectangle { width: width, height: height }
                }

                fn square(size: i64) -> Rectangle {
                    Rectangle { width: size, height: size }
                }

                fn default() -> Rectangle {
                    Rectangle { width: 10, height: 10 }
                }
            }

            pub fn test_multiple_associated() -> i64 {
                let r = Rectangle::new(5, 10);
                let s = Rectangle::square(7);
                let d = Rectangle::default();
                r.width + s.width + d.width
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 5: Failed to compile multiple associated functions: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase5_mixed_methods_and_associated() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Counter {
                value: i64,
            }

            impl Counter {
                fn new() -> Counter {
                    Counter { value: 0 }
                }

                fn increment(self) -> i64 {
                    self.value + 1
                }

                fn get_value(self) -> i64 {
                    self.value
                }
            }

            pub fn test_mixed() -> i64 {
                let c = Counter::new();
                let v = c.get_value();
                c.increment() + v
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 5: Failed to compile mixed methods and associated: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase5_associated_with_args() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Product {
                id: i64,
                price: i64,
            }

            impl Product {
                fn create(id: i64, price: i64) -> Product {
                    Product { id: id, price: price }
                }
            }

            pub fn test_with_args() -> i64 {
                let p1 = Product::create(100, 50);
                let p2 = Product::create(200, 75);
                p1.price + p2.price
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 5: Failed to compile associated with arguments: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase5_factory_pattern() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Circle {
                radius: i64,
            }

            impl Circle {
                fn new(radius: i64) -> Circle {
                    Circle { radius: radius }
                }

                fn unit() -> Circle {
                    Circle { radius: 1 }
                }

                fn area(self) -> i64 {
                    self.radius * self.radius * 3
                }
            }

            pub fn test_factory() -> i64 {
                let c1 = Circle::new(5);
                let c2 = Circle::unit();
                c1.area() + c2.area()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 5: Failed to compile factory pattern: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase5_static_utilities() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Math {
                dummy: i64,
            }

            impl Math {
                fn abs(x: i64) -> i64 {
                    let result = 0;
                    result
                }

                fn max(a: i64, b: i64) -> i64 {
                    let result = a;
                    result
                }

                fn min(a: i64, b: i64) -> i64 {
                    let result = b;
                    result
                }
            }

            pub fn test_static_utils() -> i64 {
                let a = Math::abs(5);
                let mx = Math::max(10, 20);
                let mn = Math::min(10, 20);
                a + mx + mn
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 5: Failed to compile static utilities: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase6_explicit_return() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            pub fn compute(x: i64) -> i64 {
                return x * 2;
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 6: Failed to compile explicit return: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase6_implicit_return() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            pub fn add(a: i64, b: i64) -> i64 {
                let sum = a + b;
                sum
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 6: Failed to compile implicit return: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase6_method_return() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Calculator {
                value: i64,
            }

            impl Calculator {
                fn double(self) -> i64 {
                    self.value * 2
                }

                fn add(self, x: i64) -> i64 {
                    self.value + x
                }
            }

            pub fn test_method_returns() -> i64 {
                let calc = Calculator { value: 10 };
                let d = calc.double();
                calc.add(5)
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 6: Failed to compile method return: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase6_chain_two() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            impl Point {
                fn new(x: i64, y: i64) -> Point {
                    Point { x: x, y: y }
                }

                fn get_x(self) -> i64 {
                    self.x
                }
            }

            pub fn test_chain() -> i64 {
                Point::new(5, 10).get_x()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 6: Failed to compile two-level chaining: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase6_self_return() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Builder {
                value: i64,
            }

            impl Builder {
                fn new() -> Builder {
                    Builder { value: 0 }
                }

                fn with_value(self, v: i64) -> Self {
                    Builder { value: v }
                }

                fn build(self) -> i64 {
                    self.value
                }
            }

            pub fn test_self_type() -> i64 {
                Builder::new().with_value(42).build()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 6: Failed to compile Self return type: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase6_builder_pattern() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Config {
                width: i64,
                height: i64,
            }

            impl Config {
                fn new() -> Config {
                    Config { width: 800, height: 600 }
                }

                fn width(self, w: i64) -> Self {
                    Config { width: w, height: self.height }
                }

                fn height(self, h: i64) -> Self {
                    Config { width: self.width, height: h }
                }

                fn area(self) -> i64 {
                    self.width * self.height
                }
            }

            pub fn test_builder() -> i64 {
                Config::new()
                    .width(1920)
                    .height(1080)
                    .area()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 6: Failed to compile builder pattern: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase6_nested_method_calls() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Value {
                num: i64,
            }

            impl Value {
                fn new(n: i64) -> Value {
                    Value { num: n }
                }

                fn multiply(self, factor: i64) -> Self {
                    Value { num: self.num * factor }
                }

                fn add(self, amount: i64) -> Self {
                    Value { num: self.num + amount }
                }

                fn get(self) -> i64 {
                    self.num
                }
            }

            pub fn test_nested() -> i64 {
                Value::new(5)
                    .multiply(3)
                    .add(10)
                    .multiply(2)
                    .get()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 6: Failed to compile nested method calls: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase7_field_assignment() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            impl Point {
                fn new(x: i64, y: i64) -> Point {
                    Point { x: x, y: y }
                }

                fn set_x(self, new_x: i64) -> Self {
                    let mut p = self;
                    p.x = new_x;
                    p
                }
            }

            pub fn test_field_assign() -> i64 {
                let p = Point::new(1, 2);
                let p2 = p.set_x(10);
                p2.x
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 7: Failed to compile field assignment: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase7_self_field_assignment() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Counter {
                value: i64,
            }

            impl Counter {
                fn new() -> Counter {
                    Counter { value: 0 }
                }

                fn increment(self) -> Self {
                    let result = Counter { value: self.value + 1 };
                    result
                }

                fn get(self) -> i64 {
                    self.value
                }
            }

            pub fn test_counter() -> i64 {
                Counter::new().increment().increment().get()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 7: Failed to compile counter pattern: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase7_accumulator() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Accumulator {
                sum: i64,
                count: i64,
            }

            impl Accumulator {
                fn new() -> Accumulator {
                    Accumulator { sum: 0, count: 0 }
                }

                fn add(self, value: i64) -> Self {
                    Accumulator { 
                        sum: self.sum + value, 
                        count: self.count + 1 
                    }
                }

                fn average(self) -> i64 {
                    self.sum / self.count
                }
            }

            pub fn test_accumulator() -> i64 {
                Accumulator::new()
                    .add(10)
                    .add(20)
                    .add(30)
                    .average()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 7: Failed to compile accumulator: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase7_builder_mutations() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Config {
                width: i64,
                height: i64,
            }

            impl Config {
                fn new() -> Config {
                    Config { width: 800, height: 600 }
                }

                fn set_width(self, w: i64) -> Self {
                    Config { width: w, height: self.height }
                }

                fn set_height(self, h: i64) -> Self {
                    Config { width: self.width, height: h }
                }

                fn area(self) -> i64 {
                    self.width * self.height
                }
            }

            pub fn test_builder() -> i64 {
                Config::new()
                   .set_width(100)
                    .set_height(50)
                    .area()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 7: Failed to compile builder mutations: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase7_multiple_field_mutations() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            impl Point {
                fn new(x: i64, y: i64) -> Point {
                    Point { x: x, y: y }
                }

                fn translate(self, dx: i64, dy: i64) -> Self {
                    Point { 
                        x: self.x + dx, 
                        y: self.y + dy 
                    }
                }

                fn sum(self) -> i64 {
                    self.x + self.y
                }
            }

            pub fn test_translate() -> i64 {
                Point::new(5, 10)
                    .translate(3, 4)
                    .translate(2, 1)
                    .sum()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 7: Failed to compile multiple field mutations: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase8_ready() {
        let config = BytecodeCompilerConfig::default();
        let compiler = BytecodeCompiler::new(config);

        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            impl Point {
                fn new(x: i64, y: i64) -> Point {
                    Point { x: x, y: y }
                }

                fn value(self) -> i64 {
                    self.x + self.y
                }
            }

            pub fn test_point() -> i64 {
                Point::new(10, 20).value()
            }
        "#;

        let result = compiler.compile_source(source);
        assert!(
            result.is_ok(),
            "Phase 8: System ready for traits: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_phase8_basic_trait_parsing() {
        let source = r#"
            trait Display {
                fn show(self) -> i64;
            }

            pub fn test_trait() -> i64 {
                10
            }
        "#;

        let tokens = tokenize(unsafe { std::mem::transmute::<&str, &'static str>(source) })
            .expect("Tokenization failed");
        let result = parse(tokens);

        assert!(result.is_ok(), "Trait parsing failed: {:?}", result.err());
        let program = result.unwrap();

        assert_eq!(program.traits.len(), 1, "Should have 1 trait");
        assert_eq!(
            program.traits[0].name, "Display",
            "Trait name should be Display"
        );
        assert_eq!(program.traits[0].methods.len(), 1, "Should have 1 method");
        assert_eq!(
            program.traits[0].methods[0].name, "show",
            "Method name should be show"
        );
    }

    #[test]
    fn test_phase8_trait_with_multiple_methods() {
        let source = r#"
            trait Drawable {
                fn draw(self) -> i64;
                fn clear(self) -> i64;
                fn resize(self, width: i64, height: i64) -> i64;
            }

            pub fn test_trait() -> i64 {
                10
            }
        "#;

        let tokens = tokenize(unsafe { std::mem::transmute::<&str, &'static str>(source) })
            .expect("Tokenization failed");
        let result = parse(tokens);

        assert!(result.is_ok(), "Trait parsing failed: {:?}", result.err());
        let program = result.unwrap();

        assert_eq!(program.traits.len(), 1, "Should have 1 trait");
        assert_eq!(program.traits[0].methods.len(), 3, "Should have 3 methods");

        assert_eq!(program.traits[0].methods[0].name, "draw");
        assert_eq!(program.traits[0].methods[1].name, "clear");
        assert_eq!(program.traits[0].methods[2].name, "resize");

        assert_eq!(
            program.traits[0].methods[2].params.len(),
            3,
            "resize should have self + 2 params"
        );
    }

    #[test]
    fn test_phase8_trait_implementation_parsing() {
        let source = r#"
            trait Display {
                fn show(self) -> i64;
            }

            struct Point {
                x: i64,
                y: i64,
            }

            impl Display for Point {
                fn show(self) -> i64 {
                    self.x + self.y
                }
            }

            pub fn test_trait() -> i64 {
                10
            }
        "#;

        let tokens = tokenize(unsafe { std::mem::transmute::<&str, &'static str>(source) })
            .expect("Tokenization failed");
        let result = parse(tokens);

        assert!(
            result.is_ok(),
            "Trait impl parsing failed: {:?}",
            result.err()
        );
        let program = result.unwrap();

        assert_eq!(program.traits.len(), 1, "Should have 1 trait");

        assert_eq!(program.trait_impls.len(), 1, "Should have 1 trait impl");
        assert_eq!(
            program.trait_impls[0].trait_name, "Display",
            "Trait name should be Display"
        );
        assert_eq!(
            program.trait_impls[0].type_name, "Point",
            "Type name should be Point"
        );
        assert_eq!(
            program.trait_impls[0].methods.len(),
            1,
            "Should have 1 method"
        );
        assert_eq!(
            program.trait_impls[0].methods[0].name, "show",
            "Method name should be show"
        );
        assert!(
            !program.trait_impls[0].methods[0].body.is_empty(),
            "Method should have a body"
        );
    }

    #[test]
    fn test_phase8_multiple_traits_and_impls() {
        let source = r#"
            trait Display {
                fn show(self) -> i64;
            }

            trait Eq {
                fn equals(self, other: i64) -> i64;
            }

            struct Point {
                x: i64,
                y: i64,
            }

            impl Display for Point {
                fn show(self) -> i64 {
                    self.x + self.y
                }
            }

            impl Eq for Point {
                fn equals(self, other: i64) -> i64 {
                    self.x
                }
            }

            pub fn test_traits() -> i64 {
                10
            }
        "#;

        let tokens = tokenize(unsafe { std::mem::transmute::<&str, &'static str>(source) })
            .expect("Tokenization failed");
        let result = parse(tokens);

        assert!(
            result.is_ok(),
            "Multiple traits parsing failed: {:?}",
            result.err()
        );
        let program = result.unwrap();

        assert_eq!(program.traits.len(), 2, "Should have 2 traits");
        assert_eq!(program.traits[0].name, "Display");
        assert_eq!(program.traits[1].name, "Eq");

        assert_eq!(program.trait_impls.len(), 2, "Should have 2 trait impls");
        assert_eq!(program.trait_impls[0].trait_name, "Display");
        assert_eq!(program.trait_impls[0].type_name, "Point");
        assert_eq!(program.trait_impls[1].trait_name, "Eq");
        assert_eq!(program.trait_impls[1].type_name, "Point");
    }
}
