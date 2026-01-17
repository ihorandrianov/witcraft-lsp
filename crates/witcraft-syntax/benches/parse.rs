use std::hint::black_box;
use std::time::Instant;

fn main() {
    let small_source = r#"package example:types@1.0.0;

interface api {
    record user {
        id: u64,
        name: string,
    }
    get-user: func(id: u64) -> user;
}
"#;

    let medium_source = generate_medium_source();
    let large_source = generate_large_source();

    println!("=== WIT Parser Benchmarks ===\n");

    // Benchmark lexing
    println!("--- Lexing ---");
    bench("lex small (~100 bytes)", 10000, || {
        black_box(witcraft_syntax::lex(small_source))
    });
    bench("lex medium (~5KB)", 1000, || {
        black_box(witcraft_syntax::lex(&medium_source))
    });
    bench("lex large (~50KB)", 100, || {
        black_box(witcraft_syntax::lex(&large_source))
    });

    println!("\n--- Lexing (non-trivia only) ---");
    bench("lex_non_trivia small", 10000, || {
        black_box(witcraft_syntax::lex_non_trivia(small_source))
    });
    bench("lex_non_trivia medium", 1000, || {
        black_box(witcraft_syntax::lex_non_trivia(&medium_source))
    });
    bench("lex_non_trivia large", 100, || {
        black_box(witcraft_syntax::lex_non_trivia(&large_source))
    });

    println!("\n--- Parsing ---");
    bench("parse small (~100 bytes)", 10000, || {
        black_box(witcraft_syntax::parse(small_source))
    });
    bench("parse medium (~5KB)", 1000, || {
        black_box(witcraft_syntax::parse(&medium_source))
    });
    bench("parse large (~50KB)", 100, || {
        black_box(witcraft_syntax::parse(&large_source))
    });

    println!("\n--- SymbolIndex ---");
    let small_result = witcraft_syntax::parse(small_source);
    let medium_result = witcraft_syntax::parse(&medium_source);
    let large_result = witcraft_syntax::parse(&large_source);

    bench("index small", 10000, || {
        black_box(witcraft_syntax::SymbolIndex::build(&small_result.root))
    });
    bench("index medium", 1000, || {
        black_box(witcraft_syntax::SymbolIndex::build(&medium_result.root))
    });
    bench("index large", 100, || {
        black_box(witcraft_syntax::SymbolIndex::build(&large_result.root))
    });

    println!("\n--- SymbolIndex::find_definition ---");
    let small_index = witcraft_syntax::SymbolIndex::build(&small_result.root);
    let medium_index = witcraft_syntax::SymbolIndex::build(&medium_result.root);
    let large_index = witcraft_syntax::SymbolIndex::build(&large_result.root);

    bench("find_definition small (existing)", 100000, || {
        black_box(small_index.find_definition("user"))
    });
    bench("find_definition medium (existing)", 100000, || {
        black_box(medium_index.find_definition("iface25"))
    });
    bench("find_definition large (existing)", 100000, || {
        black_box(large_index.find_definition("iface250"))
    });
    bench("find_definition large (not found)", 100000, || {
        black_box(large_index.find_definition("nonexistent"))
    });

    println!("\n--- LineIndex ---");
    bench("LineIndex::new small", 10000, || {
        black_box(witcraft_syntax::LineIndex::new(small_source))
    });
    bench("LineIndex::new medium", 1000, || {
        black_box(witcraft_syntax::LineIndex::new(&medium_source))
    });
    bench("LineIndex::new large", 100, || {
        black_box(witcraft_syntax::LineIndex::new(&large_source))
    });

    let small_line_index = witcraft_syntax::LineIndex::new(small_source);
    let large_line_index = witcraft_syntax::LineIndex::new(&large_source);

    bench("LineIndex::position small", 100000, || {
        black_box(small_line_index.position(50))
    });
    bench("LineIndex::position large (middle)", 100000, || {
        black_box(large_line_index.position(60000))
    });

    println!("\n--- Full LSP Flow (parse + index + line_index) ---");
    bench("full flow small", 10000, || {
        let result = witcraft_syntax::parse(small_source);
        let _index = witcraft_syntax::SymbolIndex::build(&result.root);
        let _line_index = witcraft_syntax::LineIndex::new(small_source);
        black_box(result)
    });
    bench("full flow medium", 1000, || {
        let result = witcraft_syntax::parse(&medium_source);
        let _index = witcraft_syntax::SymbolIndex::build(&result.root);
        let _line_index = witcraft_syntax::LineIndex::new(&medium_source);
        black_box(result)
    });
    bench("full flow large", 100, || {
        let result = witcraft_syntax::parse(&large_source);
        let _index = witcraft_syntax::SymbolIndex::build(&result.root);
        let _line_index = witcraft_syntax::LineIndex::new(&large_source);
        black_box(result)
    });

    // Memory info
    println!("\n--- Source Sizes ---");
    println!("small:  {} bytes", small_source.len());
    println!("medium: {} bytes", medium_source.len());
    println!("large:  {} bytes", large_source.len());

    // Token counts
    let small_tokens = witcraft_syntax::lex(small_source);
    let medium_tokens = witcraft_syntax::lex(&medium_source);
    let large_tokens = witcraft_syntax::lex(&large_source);
    println!("\n--- Token Counts ---");
    println!("small:  {} tokens", small_tokens.len());
    println!("medium: {} tokens", medium_tokens.len());
    println!("large:  {} tokens", large_tokens.len());

    // Definition counts
    println!("\n--- Definition Counts ---");
    println!("small:  {} definitions", small_index.definitions().len());
    println!("medium: {} definitions", medium_index.definitions().len());
    println!("large:  {} definitions", large_index.definitions().len());
}

fn bench<F, R>(name: &str, iterations: u32, mut f: F)
where
    F: FnMut() -> R,
{
    // Warmup
    for _ in 0..10 {
        black_box(f());
    }

    let start = Instant::now();
    for _ in 0..iterations {
        black_box(f());
    }
    let elapsed = start.elapsed();
    let per_iter = elapsed / iterations;

    println!(
        "{:45} {:>10.2?}/iter ({} iterations)",
        name, per_iter, iterations
    );
}

fn generate_medium_source() -> String {
    let mut source = String::from("package benchmark:medium@1.0.0;\n\n");

    for i in 0..50 {
        source.push_str(&format!(
            r#"interface iface{i} {{
    record rec{i} {{
        id: u64,
        name: string,
        data: list<u8>,
    }}

    variant result{i} {{
        ok(rec{i}),
        err(string),
    }}

    get-item: func(id: u64) -> result{i};
    list-items: func() -> list<rec{i}>;
}}

"#
        ));
    }

    source
}

fn generate_large_source() -> String {
    let mut source = String::from("package benchmark:large@1.0.0;\n\n");

    for i in 0..500 {
        source.push_str(&format!(
            r#"interface iface{i} {{
    record rec{i} {{
        id: u64,
        name: string,
        data: list<u8>,
    }}

    variant result{i} {{
        ok(rec{i}),
        err(string),
    }}

    get-item: func(id: u64) -> result{i};
    list-items: func() -> list<rec{i}>;
}}

"#
        ));
    }

    source
}
