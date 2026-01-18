//! Integration tests using real WASI WIT files.
//!
//! These tests verify that the parser correctly handles actual WASI interface
//! definitions from the WebAssembly/wasi-* repositories.

use witcraft_syntax::{ast::Item, node_at, parse, LineIndex, SymbolIndex};

const WASI_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../../examples/wasi");

fn read_wasi_file(name: &str) -> String {
    let path = format!("{}/{}", WASI_DIR, name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

#[test]
fn parse_wasi_io_poll() {
    let content = read_wasi_file("poll.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "poll.wit parse errors: {:?}", result.errors);

    let pkg = result.root.package.as_ref().expect("should have package");
    assert_eq!(pkg.namespace[0].name.as_ref(), "wasi");
    assert_eq!(pkg.name.name.as_ref(), "io");
    assert!(pkg.version.is_some());

    assert_eq!(result.root.items.len(), 1);
    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "poll");
        // resource pollable + poll function
        assert_eq!(iface.items.len(), 2);
    } else {
        panic!("expected interface, got {:?}", result.root.items[0]);
    }
}

#[test]
fn parse_wasi_io_error() {
    let content = read_wasi_file("error.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "error.wit parse errors: {:?}", result.errors);

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "error");
        // resource error with to-debug-string method
        assert_eq!(iface.items.len(), 1);
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_io_streams() {
    let content = read_wasi_file("streams.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "streams.wit parse errors: {:?}", result.errors);

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "streams");
        // uses + stream-error variant + input-stream + output-stream resources
        assert!(iface.items.len() >= 4, "expected at least 4 items, got {}", iface.items.len());
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_clocks_wall_clock() {
    let content = read_wasi_file("wall-clock.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "wall-clock.wit parse errors: {:?}", result.errors);

    let pkg = result.root.package.as_ref().expect("should have package");
    assert_eq!(pkg.namespace[0].name.as_ref(), "wasi");
    assert_eq!(pkg.name.name.as_ref(), "clocks");

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "wall-clock");
        // datetime record + now + resolution functions
        assert_eq!(iface.items.len(), 3);
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_clocks_monotonic_clock() {
    let content = read_wasi_file("monotonic-clock.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "monotonic-clock.wit parse errors: {:?}", result.errors);

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "monotonic-clock");
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_random_random() {
    let content = read_wasi_file("random.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "random.wit parse errors: {:?}", result.errors);

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "random");
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_random_insecure() {
    let content = read_wasi_file("insecure.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "insecure.wit parse errors: {:?}", result.errors);

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "insecure");
        // get-insecure-random-bytes + get-insecure-random-u64
        assert_eq!(iface.items.len(), 2);
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_random_insecure_seed() {
    let content = read_wasi_file("insecure-seed.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "insecure-seed.wit parse errors: {:?}", result.errors);

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "insecure-seed");
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_http_types() {
    let content = read_wasi_file("http-types.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "http-types.wit parse errors: {:?}", result.errors);

    // http-types.wit is an interface file without a package declaration
    assert!(result.root.items.len() >= 1, "should have at least one item");

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "types");
        // This is a large interface with many items
        assert!(iface.items.len() > 20, "expected many items in http types, got {}", iface.items.len());
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_http_handler() {
    let content = read_wasi_file("http-handler.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "http-handler.wit parse errors: {:?}", result.errors);
}

#[test]
fn parse_wasi_cli_command() {
    let content = read_wasi_file("command.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "command.wit parse errors: {:?}", result.errors);

    let pkg = result.root.package.as_ref().expect("should have package");
    assert_eq!(pkg.namespace[0].name.as_ref(), "wasi");
    assert_eq!(pkg.name.name.as_ref(), "cli");

    if let Item::World(world) = &result.root.items[0] {
        assert_eq!(world.name.name.as_ref(), "command");
    } else {
        panic!("expected world");
    }
}

#[test]
fn parse_wasi_cli_run() {
    let content = read_wasi_file("run.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "run.wit parse errors: {:?}", result.errors);
}

#[test]
fn parse_wasi_filesystem_types() {
    let content = read_wasi_file("filesystem-types.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "filesystem-types.wit parse errors: {:?}", result.errors);

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "types");
        // Large interface with many types and functions
        assert!(iface.items.len() > 15, "expected many items in filesystem types, got {}", iface.items.len());
    } else {
        panic!("expected interface");
    }
}

#[test]
fn parse_wasi_sockets_network() {
    let content = read_wasi_file("network.wit");
    let result = parse(&content);

    assert!(result.is_ok(), "network.wit parse errors: {:?}", result.errors);

    if let Item::Interface(iface) = &result.root.items[0] {
        assert_eq!(iface.name.name.as_ref(), "network");
    } else {
        panic!("expected interface");
    }
}

#[test]
fn all_wasi_files_parse_without_errors() {
    let files = [
        "poll.wit",
        "error.wit",
        "streams.wit",
        "wall-clock.wit",
        "monotonic-clock.wit",
        "random.wit",
        "insecure.wit",
        "insecure-seed.wit",
        "http-types.wit",
        "http-handler.wit",
        "command.wit",
        "run.wit",
        "filesystem-types.wit",
        "network.wit",
    ];

    let mut failures = Vec::new();

    for file in files {
        let content = read_wasi_file(file);
        let result = parse(&content);

        if !result.is_ok() {
            failures.push((file, result.errors));
        }
    }

    if !failures.is_empty() {
        let mut msg = String::from("The following WASI files failed to parse:\n");
        for (file, errors) in &failures {
            msg.push_str(&format!("\n{}:\n", file));
            for err in errors {
                msg.push_str(&format!("  - {}\n", err.message));
            }
        }
        panic!("{}", msg);
    }
}

#[test]
fn find_definitions_in_wasi_io_streams() {
    let content = read_wasi_file("streams.wit");
    let result = parse(&content);
    assert!(result.is_ok());

    let index = SymbolIndex::build(&result.root);

    // Should find stream-error, input-stream, output-stream definitions
    let defs = index.definitions();
    assert!(defs.len() >= 3, "expected at least 3 definitions, got {}", defs.len());

    // Check specific definitions exist
    let def_names: Vec<_> = defs.iter().map(|d| d.name.as_ref()).collect();
    assert!(def_names.contains(&"streams"), "should have 'streams' interface");
}

#[test]
fn find_definitions_in_wasi_http_types() {
    let content = read_wasi_file("http-types.wit");
    let result = parse(&content);
    assert!(result.is_ok());

    let index = SymbolIndex::build(&result.root);

    let defs = index.definitions();
    // http-types has many definitions: types interface, method, scheme, error-code, fields, etc.
    assert!(defs.len() >= 10, "expected at least 10 definitions, got {}", defs.len());
}

#[test]
fn byte_offset_lookup_works() {
    let content = read_wasi_file("poll.wit");
    let result = parse(&content);
    assert!(result.is_ok());

    // Find "pollable" in the content
    let pollable_pos = content.find("pollable").expect("should find pollable");

    let node = node_at(&result.root, pollable_pos as u32);
    assert!(node.is_some(), "should find node at pollable position");
}

#[test]
fn line_index_conversion() {
    let content = read_wasi_file("streams.wit");
    let line_index = LineIndex::new(&content);

    // Test roundtrip conversion
    for offset in [0u32, 100, 500, 1000, 2000] {
        if offset < content.len() as u32 {
            let pos = line_index.position(offset);
            let back = line_index.offset(pos).expect("offset should be valid");
            assert_eq!(offset, back, "offset roundtrip failed for {}", offset);
        }
    }
}
