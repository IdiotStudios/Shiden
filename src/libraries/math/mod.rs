#![allow(dead_code)]

pub mod helpers {
    use std::collections::BTreeMap;

    pub fn get_math_helpers() -> BTreeMap<&'static str, Vec<u8>> {
        let mut m = BTreeMap::new();

        m.insert(
            "math_abs",
            vec![
                0x48, 0x89, 0xF8, 0x48, 0x99, 0x48, 0x31, 0xC2, 0x48, 0x29, 0xD0, 0xC3,
            ],
        );

        m.insert(
            "math_min",
            vec![
                0x48, 0x89, 0xF8, 0x48, 0x39, 0xF0, 0x48, 0x0F, 0x4E, 0xC6, 0xC3,
            ],
        );

        m.insert(
            "math_max",
            vec![
                0x48, 0x89, 0xF8, 0x48, 0x39, 0xF0, 0x48, 0x0F, 0x4C, 0xC6, 0xC3,
            ],
        );

        m.insert(
            "math_pow",
            vec![
                0x48, 0xC7, 0xC0, 0x01, 0x00, 0x00, 0x00, 0x48, 0x85, 0xF6, 0x7E, 0x0A, 0x48, 0x0F,
                0xAF, 0xC7, 0x48, 0xFF, 0xCE, 0x75, 0xF7, 0xC3,
            ],
        );

        m.insert(
            "math_is_even",
            vec![
                0x48, 0x89, 0xF8, 0x48, 0x83, 0xE0, 0x01, 0x48, 0x83, 0xF8, 0x00, 0x75, 0x04, 0x48,
                0xC7, 0xC0, 0x01, 0x00, 0x00, 0x00, 0xC3, 0x48, 0xC7, 0xC0, 0x00, 0x00, 0x00, 0x00,
                0xC3,
            ],
        );

        m.insert(
            "math_is_odd",
            vec![0x48, 0x89, 0xF8, 0x48, 0x83, 0xE0, 0x01, 0xC3],
        );

        m.insert(
            "math_sign",
            vec![
                0x48, 0x85, 0xFF, 0x74, 0x06, 0x48, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF, 0x48, 0x0F,
                0x4F, 0xC7, 0xC3, 0x48, 0xC7, 0xC0, 0x00, 0x00, 0x00, 0x00, 0xC3,
            ],
        );

        m
    }
}

pub fn math_abs(x: i64) -> i64 {
    if x < 0 { -x } else { x }
}

pub fn math_min(a: i64, b: i64) -> i64 {
    if a < b { a } else { b }
}

pub fn math_max(a: i64, b: i64) -> i64 {
    if a > b { a } else { b }
}

pub fn math_pow(base: i64, exp: i64) -> i64 {
    if exp < 0 {
        return 0;
    }

    let mut result = 1i64;
    let mut e = exp;
    while e > 0 {
        result = result * base;
        e = e - 1;
    }
    result
}

pub fn math_gcd(mut a: i64, mut b: i64) -> i64 {
    if a < 0 {
        a = -a;
    }
    if b < 0 {
        b = -b;
    }

    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a
}

pub fn math_lcm(a: i64, b: i64) -> i64 {
    (a * b) / math_gcd(a, b)
}

pub fn math_is_even(x: i64) -> i64 {
    if x % 2 == 0 { 1 } else { 0 }
}

pub fn math_is_odd(x: i64) -> i64 {
    if x % 2 != 0 { 1 } else { 0 }
}

pub fn math_sqrt(x: i64) -> i64 {
    if x < 0 {
        return 0;
    }
    if x == 0 || x == 1 {
        return x;
    }

    let mut guess = x / 2;
    let mut prev = x;

    while guess != prev {
        prev = guess;
        guess = (guess + x / guess) / 2;
    }

    guess
}

pub fn math_factorial(n: i64) -> i64 {
    if n <= 1 {
        return 1;
    }

    let mut result = 1i64;
    let mut i = 2i64;
    while i <= n {
        result = result * i;
        i = i + 1;
    }
    result
}

pub fn math_sum(n: i64) -> i64 {
    if n <= 0 {
        return 0;
    }
    (n * (n + 1)) / 2
}

pub fn math_clamp(x: i64, min: i64, max: i64) -> i64 {
    if x < min {
        min
    } else if x > max {
        max
    } else {
        x
    }
}

pub fn math_sign(x: i64) -> i64 {
    if x < 0 {
        -1
    } else if x > 0 {
        1
    } else {
        0
    }
}
