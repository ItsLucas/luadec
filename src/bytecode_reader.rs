use nom::{
    IResult,
    Err as NomErr,
    error::{Error as NomError, ErrorKind},
    bytes::complete::{tag, take},
    combinator::map,
    multi::length_count,
    number::complete::{
        be_u8, be_i8,
        u16, u32, u64 as nom_u64,
        i16, i32, i64 as nom_i64,
        f32 as nom_f32, f64 as nom_f64,
    },
    number::Endianness,
};

#[derive(Clone,Debug,PartialEq,Eq)]
pub struct LuaHeader {
    pub format_version: u8,
    pub big_endian: bool,
    pub integer_size: u8,
    pub size_t_size: u8,
    pub instruction_size: u8,
    pub number_size: u8,
    pub number_integral: bool
}

impl LuaHeader {
    fn endian(&self) -> Endianness {
        if self.big_endian {
            Endianness::Big
        } else {
            Endianness::Little
        }
    }
}

#[derive(Debug)]
pub enum LuaNumber {
    Integral(i64),
    Floating(f64)
}

#[derive(Debug)]
pub enum LuaConstant {
    Null,
    Bool(bool),
    Number(LuaNumber),
    String(Vec<u8>)
}

#[derive(Debug)]
pub struct LuaLocal {
    pub name: Vec<u8>,
    pub start_pc: u64,
    pub end_pc: u64
}

#[derive(Debug)]
pub struct LuaVarArgInfo {
    pub has_arg: bool,
    pub needs_arg: bool,
}

#[derive(Debug)]
pub struct LuaChunk {
    pub name: Vec<u8>,
    pub line_defined: u64,
    pub last_line_defined: u64,
    pub num_upvalues: u8,
    pub num_params: u8,
    pub is_vararg: Option<LuaVarArgInfo>,
    pub max_stack: u8,
    pub instructions: Vec<u32>,
    pub constants: Vec<LuaConstant>,
    pub prototypes: Vec<LuaChunk>,
    pub source_lines: Vec<u64>,
    pub locals: Vec<LuaLocal>,
    pub upvalue_names: Vec<Vec<u8>>,
}

#[derive(Debug)]
pub struct LuaBytecode {
    pub header: LuaHeader,
    pub main_chunk: LuaChunk
}

pub fn lua_header(input: &[u8]) -> IResult<&[u8], LuaHeader> {
    let (input, _) = tag(b"\x1BLua")(input)?;
    let (input, _) = tag(b"\x51")(input)?;
    let (input, format_version) = be_u8(input)?;
    let (input, big_endian_val) = be_u8(input)?;
    let (input, integer_size) = be_u8(input)?;
    let (input, size_t_size) = be_u8(input)?;
    let (input, instruction_size) = be_u8(input)?;
    let (input, number_size) = be_u8(input)?;
    let (input, number_integral_val) = be_u8(input)?;

    Ok((input, LuaHeader {
        format_version,
        big_endian: big_endian_val != 1,
        integer_size,
        size_t_size,
        instruction_size,
        number_size,
        number_integral: number_integral_val != 0,
    }))
}

// --- Direct parsing functions ---

// These functions take `header: &LuaHeader` and their lifetime 'b is tied to the input slice.
// The returned owned types (Vec<u8>, LuaNumber, etc.) do not borrow from `header`.

fn parse_lua_integer_direct<'b>(input: &'b [u8], header: &LuaHeader) -> IResult<&'b [u8], u64> {
    let endianness = header.endian();
    match header.integer_size {
        1 => map(be_u8, |v| v as u64)(input),
        2 => map(u16(endianness), |v| v as u64)(input),
        4 => map(u32(endianness), |v| v as u64)(input),
        8 => nom_u64(endianness)(input),
        _ => Err(NomErr::Failure(NomError::new(input, ErrorKind::Switch))),
    }
}

fn parse_lua_size_t_direct<'b>(input: &'b [u8], header: &LuaHeader) -> IResult<&'b [u8], u64> {
    let endianness = header.endian();
    match header.size_t_size {
        1 => map(be_u8, |v| v as u64)(input),
        2 => map(u16(endianness), |v| v as u64)(input),
        4 => map(u32(endianness), |v| v as u64)(input),
        8 => nom_u64(endianness)(input),
        _ => Err(NomErr::Failure(NomError::new(input, ErrorKind::Switch))),
    }
}

fn parse_lua_instruction_direct<'b>(input: &'b [u8], header: &LuaHeader) -> IResult<&'b [u8], u32> {
    let endianness = header.endian();
    match header.instruction_size {
        4 => u32(endianness)(input),
        _ => Err(NomErr::Failure(NomError::new(input, ErrorKind::Switch))),
    }
}

fn parse_lua_number_direct<'b>(input: &'b [u8], header: &LuaHeader) -> IResult<&'b [u8], LuaNumber> {
    let endianness = header.endian();
    if header.number_integral {
        match header.number_size {
            1 => map(be_i8, |v| LuaNumber::Integral(v as i64))(input),
            2 => map(i16(endianness), |v| LuaNumber::Integral(v as i64))(input),
            4 => map(i32(endianness), |v| LuaNumber::Integral(v as i64))(input),
            8 => map(nom_i64(endianness), LuaNumber::Integral)(input),
            _ => Err(NomErr::Failure(NomError::new(input, ErrorKind::Switch))),
        }
    } else {
        match header.number_size {
            4 => map(nom_f32(endianness), |v| LuaNumber::Floating(v as f64))(input),
            8 => map(nom_f64(endianness), LuaNumber::Floating)(input),
            _ => Err(NomErr::Failure(NomError::new(input, ErrorKind::Switch))),
        }
    }
}

fn parse_lua_string_owned_direct<'b>(input: &'b [u8], header: &LuaHeader) -> IResult<&'b [u8], Vec<u8>> {
    let (input_after_len, len) = parse_lua_size_t_direct(input, header)?;
    if len == 0 {
        return Ok((input_after_len, Vec::new()));
    }
    let (input_after_data, s_slice) = take(len as usize)(input_after_len)?;
    if s_slice.is_empty() {
         Ok((input_after_data, Vec::new()))
    } else {
        Ok((input_after_data, s_slice[..s_slice.len() - 1].to_vec()))
    }
}

fn parse_lua_local_direct<'b>(input: &'b [u8], header: &LuaHeader) -> IResult<&'b [u8], LuaLocal> {
    let (input, name) = parse_lua_string_owned_direct(input, header)?;
    let (input, start_pc) = parse_lua_integer_direct(input, header)?;
    let (input, end_pc) = parse_lua_integer_direct(input, header)?;
    Ok((input, LuaLocal { name, start_pc, end_pc }))
}

// lua_chunk_parser rewritten
// 'chunk_input_lifetime is the lifetime of the slice being parsed by this specific call to lua_chunk_parser.
// header is a reference that must live at least as long as 'chunk_input_lifetime.
// The returned LuaChunk is fully owned and does not borrow from header.
pub fn lua_chunk_parser<'chunk_input_lifetime>(
    input: &'chunk_input_lifetime [u8],
    header: &LuaHeader, // header's lifetime is managed by the caller (lua_bytecode)
) -> IResult<&'chunk_input_lifetime [u8], LuaChunk> {

    // Helper closure for length_count's count_parser argument
    // It captures `header` by reference.
    // The lifetime 'a here will be 'chunk_input_lifetime.
    let count_parser = |i: &'chunk_input_lifetime [u8]| parse_lua_integer_direct(i, header);

    let (input, name) = parse_lua_string_owned_direct(input, header)?;
    let (input, line_defined) = parse_lua_integer_direct(input, header)?;
    let (input, last_line_defined) = parse_lua_integer_direct(input, header)?;
    let (input, num_upvalues) = be_u8(input)?;
    let (input, num_params) = be_u8(input)?;
    let (input, is_vararg_byte) = be_u8(input)?;
    let (input, max_stack) = be_u8(input)?;

    let (input, instructions) = length_count(
        count_parser, // Use the count_parser closure
        |i| parse_lua_instruction_direct(i, header),
    )(input)?;
    
    let (input, constants) = length_count(
        count_parser,
        |i_const: &'chunk_input_lifetime [u8]| {
            let (i_const, const_type) = be_u8(i_const)?;
            match const_type {
                0 => Ok((i_const, LuaConstant::Null)),
                1 => map(be_u8, |v| LuaConstant::Bool(v != 0))(i_const),
                // Pass header to the direct parser
                3 => map(|ii| parse_lua_number_direct(ii, header), LuaConstant::Number)(i_const),
                4 => map(|ii| parse_lua_string_owned_direct(ii, header), LuaConstant::String)(i_const),
                _ => Err(NomErr::Failure(NomError::new(i_const, ErrorKind::Switch))),
            }
        },
    )(input)?;

    let (input, prototypes) = length_count(
        count_parser,
        |i_proto| lua_chunk_parser(i_proto, header), // Recursive call, header is passed along
    )(input)?;

    let (input, source_lines) = length_count(
        count_parser,
        |i_line| parse_lua_integer_direct(i_line, header),
    )(input)?;

    let (input, locals) = length_count(
        count_parser,
        |i_local| parse_lua_local_direct(i_local, header),
    )(input)?;
    
    let (input, upvalue_names) = length_count(
        count_parser,
        |i_upval| parse_lua_string_owned_direct(i_upval, header),
    )(input)?;

    let is_vararg = if (is_vararg_byte & 2) != 0 {
        Some(LuaVarArgInfo {
            has_arg: (is_vararg_byte & 1) != 0,
            needs_arg: (is_vararg_byte & 4) != 0,
        })
    } else {
        None
    };

    Ok((input, LuaChunk {
        name, line_defined, last_line_defined, num_upvalues, num_params,
        is_vararg, max_stack, instructions, constants, prototypes,
        source_lines, locals, upvalue_names,
    }))
}

pub fn lua_bytecode(input: &[u8]) -> IResult<&[u8], LuaBytecode> {
    let (input, owned_header) = lua_header(input)?;
    let (input, main_chunk) = lua_chunk_parser(input, &owned_header)?;

    Ok((input, LuaBytecode {
        header: owned_header,
        main_chunk,
    }))
}