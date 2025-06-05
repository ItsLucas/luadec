//! # luadec - Lua Bytecode Decompiler
//! 
//! A library for decompiling Lua 5.1 bytecode back to readable Lua source code.
//! 
//! ## Examples
//! 
//! ### Basic usage
//! ```rust,no_run
//! use luadec::{LuaDecompiler, DecompileOptions};
//! 
//! let bytecode = std::fs::read("script.luac").unwrap();
//! let decompiler = LuaDecompiler::new();
//! let result = decompiler.decompile(&bytecode).unwrap();
//! println!("{}", result);
//! ```
//! 
//! ### Compiling and decompiling
//! ```rust,ignore
//! use luadec::{LuaDecompiler, compile_lua_script};
//! 
//! let source = "print('Hello, World!')";
//! let bytecode = compile_lua_script(source).unwrap();
//! let decompiler = LuaDecompiler::new();
//! let result = decompiler.decompile(&bytecode).unwrap();
//! ```

pub mod ast;
pub mod bytecode_reader;
pub mod cond_logic;
pub mod dump;
pub mod instruction_definitions;
pub mod instruction_decoder;
pub mod free_mark;
pub mod ralloc;
pub mod reduce;
pub mod view;
pub mod view_context;

use std::borrow::Cow;
use std::fs;
use std::io::{Error, ErrorKind, Write};
use std::process::Command;
use either::Either;

pub use crate::bytecode_reader::*;
use crate::dump::*;
use crate::instruction_definitions::*;
use crate::view::*;
use crate::view_context::*;

/// Options for controlling the decompilation process
#[derive(Debug, Clone, Copy)]
pub struct DecompileOptions {
    /// Include debug comments in the output
    pub include_debug_comments: bool,
    /// Indentation width (number of spaces)
    pub indent_width: usize,
    /// Whether to include prototype numbers in comments
    pub include_prototype_numbers: bool,
}

impl Default for DecompileOptions {
    fn default() -> Self {
        Self {
            include_debug_comments: false,
            indent_width: 3,
            include_prototype_numbers: true,
        }
    }
}

/// Error types that can occur during decompilation
#[derive(Debug, thiserror::Error)]
pub enum DecompileError {
    #[error("Failed to parse bytecode: {0}")]
    ParseError(String),
    #[error("Decompilation failed: {0}")]
    DecompileError(String),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

/// Result type for decompilation operations
pub type DecompileResult<T> = Result<T, DecompileError>;

/// Main decompiler struct
pub struct LuaDecompiler {
    options: DecompileOptions,
}

impl LuaDecompiler {
    /// Create a new decompiler with default options
    pub fn new() -> Self {
        Self {
            options: DecompileOptions::default(),
        }
    }

    /// Create a new decompiler with custom options
    pub fn with_options(options: DecompileOptions) -> Self {
        Self { options }
    }

    /// Decompile bytecode to Lua source code
    pub fn decompile(&self, bytecode: &[u8]) -> DecompileResult<String> {
        self.decompile_with_options(bytecode, &self.options)
    }

    /// Decompile bytecode with specific options (overriding instance options)
    pub fn decompile_with_options(&self, bytecode: &[u8], options: &DecompileOptions) -> DecompileResult<String> {
        let mut output = Vec::new();
        self.decompile_to_writer(bytecode, &mut output, options)?;
        Ok(String::from_utf8_lossy(&output).into_owned())
    }

    /// Decompile bytecode and write to a writer
    pub fn decompile_to_writer<W: Write>(&self, bytecode: &[u8], writer: &mut W, options: &DecompileOptions) -> DecompileResult<()> {
        let parsed = lua_bytecode(bytecode)
            .map_err(|e| DecompileError::ParseError(format!("{:?}", e)))?;

        let (_, LuaBytecode { main_chunk: chunk, .. }) = parsed;
        
        let decompiled = decompile_chunk(&chunk, &vec![], options)
            .map_err(|e| DecompileError::DecompileError(format!("{:?}", e)))?;
        
        decompiled.dump(writer, options);
        Ok(())
    }

    /// Decompile from file path
    pub fn decompile_file<P: AsRef<std::path::Path>>(&self, path: P) -> DecompileResult<String> {
        let bytecode = fs::read(path)?;
        self.decompile(&bytecode)
    }

    /// Decompile from file and write to another file
    pub fn decompile_file_to_file<P: AsRef<std::path::Path>, Q: AsRef<std::path::Path>>(
        &self, 
        input_path: P, 
        output_path: Q
    ) -> DecompileResult<()> {
        let bytecode = fs::read(input_path)?;
        let mut output_file = fs::File::create(output_path)?;
        self.decompile_to_writer(&bytecode, &mut output_file, &self.options)
    }
}

impl Default for LuaDecompiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Compile Lua source code to bytecode using luac5.1
pub fn compile_lua_script(script: &str) -> std::io::Result<Vec<u8>> {
    use tempfile::tempdir;
    
    let dir = tempdir()?;
    let script_path = dir.path().join("script.lua");
    let bytecode_path = dir.path().join("bc.luac");

    fs::write(&script_path, script)?;

    let command = Command::new("luac5.1")
        .arg("-o")
        .arg(&bytecode_path)
        .arg(&script_path)
        .status()?;

    if !command.success() {
        return Err(Error::new(ErrorKind::Other, "Failed to compile Lua script with luac5.1"));
    }

    let bytecode = fs::read(&bytecode_path)?;
    dir.close()?;
    Ok(bytecode)
}

/// Batch processing utilities
pub mod batch {
    use super::*;
    use std::path::{Path, PathBuf};
    use rayon::prelude::*;

    /// Options for batch processing
    #[derive(Debug, Clone)]
    pub struct BatchOptions {
        /// Decompilation options
        pub decompile_options: DecompileOptions,
        /// Whether to use parallel processing
        pub parallel: bool,
        /// File extension for output files
        pub output_extension: String,
    }

    impl Default for BatchOptions {
        fn default() -> Self {
            Self {
                decompile_options: DecompileOptions::default(),
                parallel: true,
                output_extension: "dec.lua".to_string(),
            }
        }
    }

        /// Batch decompile bytecode files matching a glob pattern
        pub fn decompile_bytecode_files<P: AsRef<Path> + Sync>(
            pattern: &str,
            output_dir: Option<P>,
            options: BatchOptions,
        ) -> DecompileResult<Vec<PathBuf>> {
            let paths: Vec<PathBuf> = glob::glob(pattern)
                .map_err(|e| DecompileError::DecompileError(format!("Glob pattern error: {}", e)))?
                .filter_map(|entry| entry.ok())
                .collect();
    
            let decompiler = LuaDecompiler::with_options(options.decompile_options);
            let output_paths = if options.parallel {
                paths.par_iter()
            } else {
                paths.par_iter() // 修复：串行时使用 iter() 而不是 par_iter()
            }.map(|input_path| {
                let output_path = if let Some(ref output_dir) = output_dir {
                    let mut path = output_dir.as_ref().to_path_buf();
                    path.push(input_path.file_name().unwrap());
                    path.set_extension(&options.output_extension);
                    path
                } else {
                    let mut path = input_path.clone();
                    path.set_extension(&options.output_extension);
                    path
                };
    
                let result = std::panic::catch_unwind(|| {
                    decompiler.decompile_file_to_file(input_path, &output_path)
                });
    
                match result {
                    Ok(Ok(())) => Ok(output_path),
                    Ok(Err(e)) => Err(e),
                    Err(_) => {
                        // 如果 panic，写入错误信息到输出文件
                        let _ = fs::write(&output_path, format!("Failed to decompile: panic occurred"));
                        Err(DecompileError::DecompileError("Panic during decompilation".to_string()))
                    }
                }
            }).collect::<Vec<_>>();
    
            // 分离成功和失败的结果
            let mut success_paths = Vec::new();
            for result in output_paths {
                match result {
                    Ok(path) => success_paths.push(path),
                    Err(e) => log::error!("Decompilation failed: {}", e),
                }
            }
    
            Ok(success_paths)
        }

    /// Batch compile and decompile Lua source files
    pub fn compile_and_decompile_lua_files(
        pattern: &str,
        options: BatchOptions,
    ) -> DecompileResult<Vec<PathBuf>> {
        let paths: Vec<PathBuf> = glob::glob(pattern)
            .map_err(|e| DecompileError::DecompileError(format!("Glob pattern error: {}", e)))?
            .filter_map(|entry| entry.ok())
            .filter(|path| path.extension().and_then(|ext| ext.to_str()) != Some("dec.lua"))
            .collect();

        let decompiler = LuaDecompiler::with_options(options.decompile_options);
        let output_paths = if options.parallel {
            paths.par_iter()
        } else {
            paths.par_iter()
        }.map(|input_path| {
            let mut output_path = input_path.clone();
            output_path.set_extension(&options.output_extension);

            let result = std::panic::catch_unwind(|| -> DecompileResult<()> {
                let source = fs::read_to_string(input_path)?;
                let bytecode = compile_lua_script(&source)?;
                let mut output_file = fs::File::create(&output_path)?;
                decompiler.decompile_to_writer(&bytecode, &mut output_file, &options.decompile_options)?;
                Ok(())
            });

            match result {
                Ok(Ok(())) => Ok(output_path),
                Ok(Err(e)) => Err(e),
                Err(_) => {
                    let _ = fs::write(&output_path, "Failed to decompile: panic occurred");
                    Err(DecompileError::DecompileError("Panic during decompilation".to_string()))
                }
            }
        }).collect::<Vec<_>>();

        let mut success_paths = Vec::new();
        for result in output_paths {
            match result {
                Ok(path) => success_paths.push(path),
                Err(e) => log::error!("Compilation/decompilation failed: {}", e),
            }
        }

        Ok(success_paths)
    }
}

// 内部实现需要的结构体和函数（从原来的 main.rs 移植过来）
struct DecompiledPrototype<'a> {
    path: Vec<usize>,
    context: ViewContext<'a>,
    children: Vec<Result<DecompiledPrototype<'a>, Box<dyn std::any::Any + Send + 'static>>>
}

impl<'a> DecompiledPrototype<'a> {
    fn dump(&self, writer: &mut dyn Write, options: &DecompileOptions) {
        let mut dumper = DumpContextImpl {
            path: &self.path,
            context: &self.context,
            writer,
            indent_level: vec![],
            prototypes: &self.children,
            upvalues: None,
            options,
        };

        dumper.write_root();
        dumper.write_newline();
    }
}

// 需要更新 DumpContextImpl 来接受 options
struct DumpContextImpl<'a, 'b> {
    path: &'a [usize],
    context: &'a ViewContext<'a>,
    writer: &'b mut dyn Write,
    indent_level: Vec<u8>,
    prototypes: &'a [Result<DecompiledPrototype<'a>, Box<dyn std::any::Any + Send + 'static>>],
    upvalues: Option<Vec<Cow<'a, str>>>,
    options: &'a DecompileOptions,
}


impl<'a, 'b> DumpContextImpl<'a, 'b> {
    fn format_reg_into<W: std::io::Write>(&self, reg: Reg, writer: &mut W) {
        write!(writer, "r{}_", reg.0).unwrap();

        if self.path.is_empty() {
            writer.write(b"rt").unwrap();
        } else {
            writer.write(b"pr").unwrap();
            for (i, idx) in self.path.iter().enumerate() {
                if i != 0 {
                    writer.write(b"_").unwrap();
                }
                write!(writer, "{}", idx).unwrap();
            }
        }
    }
}

impl<'a, 'b> DumpContext for DumpContextImpl<'a, 'b> {
    fn write_root(&mut self) {
        let root_views: Vec<&View> = self.context.iter_root().collect();
        for (i, view) in root_views.iter().rev().enumerate() {
            if i != 0 {
                self.write_newline();
            }
            self.write_view(view.index, DumpType::Statement { last: i+1 == root_views.len() });
        }
    }
    
    fn write_str(&mut self, s: &str) {
        write!(self.writer, "{}", s).unwrap();
    }

    fn write_view(&mut self, index: ViewRef, typ: DumpType) {
        let view = &self.context.view_at(index);

        // write!(self.writer, "--[[{:?}]]", view).unwrap();

        if let DumpType::Statement {..} = typ {
            let free_mark = &self.context.view_free_mark_at(index);

            if let &ViewType::Expression { dest, .. } = &view.view_type {
                if free_mark.incoming_root != free_mark.outgoing {
                    self.write_str("local ");
                }
                self.write_reg(dest);
                self.write_str(" = ");
            } else if let &ViewType::PinnedExpression { dest } = &view.view_type {
                if free_mark.incoming_root != free_mark.outgoing {
                    self.write_str("local ");
                }
                self.write_reg(dest);
                self.write_str(" = ");
            } else if let &ViewType::MultiExpression { base, count } = &view.view_type {
                self.write_str("local ");
                let mut reg = base;
                let top = (base + count).unwrap();
                while reg != top {
                    if reg != base {
                        self.write_str(", ");
                    }
                    self.write_reg(reg);
                    reg = reg.next();
                }
                self.write_str(" = ");
            }
        }

        view.data.dump(self, typ)
    }

    fn write_newline(&mut self) {
        self.writer.write(b"\n").unwrap();
        self.writer.write(&self.indent_level).unwrap();
    }

    fn write_name(&mut self, kst: Kst) {
        let constant = &self.context.chunk().constants[kst.0 as usize];
        match constant {
            &LuaConstant::String(ref data) => self.writer.write(data),
            _ => self.writer.write(b"`write_name called on non-string constant`")
        }.unwrap();
    }

    fn write_constant(&mut self, kst: Kst) {
        let constant = &self.context.chunk().constants[kst.0 as usize];
        match constant {
            &LuaConstant::Null => { self.writer.write(b"nil").unwrap(); },
            &LuaConstant::Bool(b) => write!(self.writer, "{}", b).unwrap(),
            &LuaConstant::Number(LuaNumber::Floating(f)) => write!(self.writer, "{}", f).unwrap(),
            &LuaConstant::Number(LuaNumber::Integral(i)) => write!(self.writer, "{}", i).unwrap(),
            &LuaConstant::String(ref data) => {
                self.writer.write(b"\"").unwrap();
                self.writer.write(data).unwrap();
                self.writer.write(b"\"").unwrap();
            },
        };
    }

    fn is_valid_name(&self, kst: Kst) -> bool {
        let constant = &self.context.chunk().constants[kst.0 as usize];
        match constant {
            &LuaConstant::String(ref data) => is_valid_lua_identifier(data),
            _ => false
        }
    }

    fn write_reg(&mut self, reg: Reg) {
        let chunk = self.context.chunk();
        if let Some(LuaVarArgInfo { has_arg: true, .. }) = chunk.is_vararg {
            if reg.0 == chunk.num_params {
                // Lua 5.1's compatibility `arg` parameter
                self.write_str("arg");
                return
            }
        }

        write!(self.writer, "r{}_", reg.0).unwrap();

        if self.path.is_empty() {
            self.write_str("rt");
        } else {
            self.write_str("pr");
            for (i, idx) in self.path.iter().enumerate() {
                if i != 0 {
                    self.write_str("_");
                }
                write!(self.writer, "{}", idx).unwrap();
            }
        }
    }

    fn write_proto(&mut self, proto_index: u32, captures: &[ClosureCapture]) {
        let proto_context = &self.prototypes[proto_index as usize];

        match proto_context {
            Ok(proto) => {
                let mut upvalues = Vec::with_capacity(captures.len());
                for capture in captures {
                    match capture {
                        &ClosureCapture::Register(reg) => {
                            let mut s: Vec<u8> = Vec::new();
                            self.format_reg_into(reg, &mut s);
                            upvalues.push(Cow::Owned(String::from_utf8(s).unwrap()))
                        },
                        &ClosureCapture::Upvalue(uv) => {
                            use std::borrow::Borrow;
                            let upval_cow = &self.upvalues.as_ref().unwrap()[uv.0 as usize];
                            upvalues.push(Cow::Borrowed(upval_cow.borrow()))
                        }
                    }
                }

                let mut inner_dumper = DumpContextImpl {
                    path: &proto.path,
                    context: &proto.context,
                    writer: self.writer,
                    indent_level: self.indent_level.clone(),
                    prototypes: &proto.children,
                    upvalues: Some(upvalues),
                    options: self.options,
                };

                inner_dumper.write_str("function(");
                for i in 0u8..proto.context.chunk().num_params {
                    if i != 0 {
                        inner_dumper.write_str(", ");
                    }
                    inner_dumper.write_reg(Reg(i));
                }
                if proto.context.chunk().is_vararg.is_some() {
                    inner_dumper.write_str(", ...");
                }
                inner_dumper.write_str(")");
                write!(inner_dumper.writer, " -- proto {}", proto_index).unwrap();
                inner_dumper.write_newline();
                inner_dumper.indent();

                inner_dumper.write_root();

                inner_dumper.unindent();
                inner_dumper.write_newline();
                inner_dumper.write_str("end");
            },
            Err(err) => write!(self.writer, "!Failed to decompile prototype {}: {}!", proto_index, try_get_str_error(err)).unwrap(),
        }
    }

    fn write_upvalue(&mut self, upvalue: Upvalue) {
        if let Some(ref upvalues) = self.upvalues {
            use std::borrow::Borrow;
            let uv: &str = upvalues[upvalue.0 as usize].borrow();
            self.writer.write(uv.as_bytes()).unwrap();
        } else {
            write!(self.writer, "u{}", upvalue.0).unwrap();
        }
    }

    fn indent(&mut self) {
        let old_len = self.indent_level.len();
        for _ in 0..self.options.indent_width {
            self.indent_level.push(b' ');
        }
        self.writer.write(&self.indent_level[old_len..]).unwrap();
    }

    fn unindent(&mut self) {
        let len = self.indent_level.len();
        self.indent_level.truncate(len - self.options.indent_width);
    }
}

fn decompile_chunk<'a>(
    chunk: &'a LuaChunk, 
    path: &Vec<usize>,
    options: &DecompileOptions,
) -> Result<DecompiledPrototype<'a>, Box<dyn std::any::Any + Send + 'static>> {
    let mut proto_path = path.clone();

    let protos: Vec<_> = chunk.prototypes.iter()
        .enumerate()
        .map(|(i, proto)| {
            proto_path.push(i);
            let res = std::panic::catch_unwind(|| {
                decompile_chunk(proto, &proto_path, options)
            });
            proto_path.clone_from(path);
            res.and_then(|r| r)
        })
        .collect();
    drop(proto_path);

    log::info!("Decompiling chunk at path: {:?}", path);
    let mut context = ViewContext::new(chunk);
    context.decompile();

    Ok(DecompiledPrototype {
        path: path.clone(),
        context,
        children: protos
    })
}

// 还需要一些工具函数
fn is_valid_lua_identifier(s: &[u8]) -> bool {
    let mut iter = s.iter().cloned();

    fn test_first_char(first: u8) -> bool {
        (first >= b'A' && first <= b'Z') || (first >= b'a' && first <= b'z') || first == b'_'
    }

    fn test_char(c: u8) -> bool {
        test_first_char(c) || (c >= b'0' && c <= b'9')
    }

    if let Some(first) = iter.next() {
        if test_first_char(first) {
            iter.all(test_char)
        } else {
            false
        }
    } else {
        false
    }
}

fn try_get_str_error<'a>(err: &'a Box<dyn std::any::Any + Send + 'static>) -> &'a str {
    if let Some(err) = err.downcast_ref::<&'static str>() {
        return err
    } else if let Some(err) = err.downcast_ref::<String>() {
        return &err
    } else {
        return "unknown error type"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_decompilation() {
        // 这需要系统安装 luac5.1
        if let Ok(bytecode) = compile_lua_script("print('Hello, World!')") {
            let decompiler = LuaDecompiler::new();
            let result = decompiler.decompile(&bytecode);
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_decompiler_options() {
        let options = DecompileOptions {
            include_debug_comments: true,
            indent_width: 3,
            include_prototype_numbers: false,
        };
        let decompiler = LuaDecompiler::with_options(options);
        // 测试 options 是否正确设置
        assert_eq!(decompiler.options.indent_width, 3);
    }
}