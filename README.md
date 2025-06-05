# luadec

[![Crates.io](https://img.shields.io/crates/v/luadec.svg)](https://crates.io/crates/luadec)
[![Documentation](https://docs.rs/luadec/badge.svg)](https://docs.rs/luadec)
[![License: GPL-3.0](https://img.shields.io/badge/License-GPL%203.0-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A Lua 5.1 bytecode decompiler library written in Rust, originated from the `lbcdec` project.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
luadec = "0.2.0"
```

## Usage

### As a Library

#### Basic Decompilation

```rust
use luadec::LuaDecompiler;

// Read bytecode from file
let bytecode = std::fs::read("script.luac")?;

// Create decompiler and decompile
let decompiler = LuaDecompiler::new();
let source_code = decompiler.decompile(&bytecode)?;

println!("{}", source_code);
```

#### With Custom Options

```rust
use luadec::{LuaDecompiler, DecompileOptions};

let options = DecompileOptions {
    include_debug_comments: true,
    indent_width: 4,
    include_prototype_numbers: true,
};

let decompiler = LuaDecompiler::with_options(options);
let result = decompiler.decompile(&bytecode)?;
```

#### Batch Processing

```rust
use luadec::batch::{BatchOptions, DecompileOptions};

let batch_options = BatchOptions {
    parallel: true,
    output_extension: "lua".to_string(),
    decompile_options: DecompileOptions::default(),
};

// Decompile all .luac files in a directory
let results = luadec::batch::decompile_bytecode_files(
    "*.luac",
    Some("output/"),
    batch_options,
)?;
```

### File Operations

```rust
use luadec::LuaDecompiler;

let decompiler = LuaDecompiler::new();

// Decompile file to file
decompiler.decompile_file_to_file("input.luac", "output.lua")?;

// Decompile file to string
let source = decompiler.decompile_file("input.luac")?;
```

## Configuration Options

### DecompileOptions

```rust
pub struct DecompileOptions {
    /// Include debug comments in output
    pub include_debug_comments: bool,
    
    /// Number of spaces for indentation
    pub indent_width: usize,
    
    /// Include prototype numbers in function definitions
    pub include_prototype_numbers: bool,
}
```

### BatchOptions

```rust
pub struct BatchOptions {
    /// Enable parallel processing
    pub parallel: bool,
    
    /// Output file extension
    pub output_extension: String,
    
    /// Decompilation options
    pub decompile_options: DecompileOptions,
}
```

## Error Handling

The library provides detailed error information:

```rust
use luadec::{DecompileError, DecompileResult};

match decompiler.decompile(&bytecode) {
    Ok(source) => println!("Success: {}", source),
    Err(DecompileError::InvalidBytecode(msg)) => {
        eprintln!("Invalid bytecode: {}", msg);
    },
    Err(DecompileError::DecompileError(msg)) => {
        eprintln!("Decompilation failed: {}", msg);
    },
    Err(DecompileError::IoError(err)) => {
        eprintln!("IO error: {}", err);
    },
}
```

## Performance

- **Memory Efficient**: Processes bytecode without loading entire files into memory
- **Parallel Processing**: Batch operations can utilize multiple CPU cores
- **Optimized Parsing**: Efficient bytecode parsing with minimal allocations

## Limitations

- **Lua 5.1 Only**: Currently supports Lua 5.1 bytecode format only
- **Debug Information**: Some debug information may be lost during compilation
- **Variable Names**: Local variable names may be generic if debug info is stripped
- **Comments**: Original source comments are not preserved in bytecode

## Development

### Building from Source

```bash
git clone https://github.com/ItsLucas/luadec.git
cd luadec
cargo build --release
```

### Running Tests

```bash
cargo test
```

### Generating Documentation

```bash
cargo doc --open
```

## Related Projects

- [**lbcdec**](https://github.com/lbcdec/lbcdec): The original project this library is based on

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Original `lbcdec` project by Dwayne Slater

## Changelog

### Version 0.2.0
- Initial Release
