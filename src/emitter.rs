//! Converts the abstract syntax representation to machine code

use std::collections::HashMap;
use std::fmt::{Display,Formatter};

use crate::{Operand, Value, SyntaxTree};

/// A compile-time error that occured during the emitting stage
#[derive(Debug)]
pub enum Error {
    /// Invalid combination (field 0) of operands
    InvalidOperands([Operand;3]),
    /// Invalid destination (field 0) of vector jump
    InvalidRstDest(i32),
    /// Identifier (field 0) as destination of vector jump, which is unsupported
    IdentAsRstDest(String),
    /// Literal (field 1) which is outside the range of it's type (field 0)
    InvalidLiteral(String, i32),
    /// The emitted machine code is larger than the Gameboy's RAM, this is extremely
    /// unlikely to happen
    CodeTooLarge,
    /// Usage of a label (field 0) that doesn't exist
    UnknownLabel(String),
    /// The offset (field 1) of a label (field 0) is out of range for a particular 
    /// immediate type (field 2)
    LabelOffsetOutOfRange(String,usize,ReplacementType),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(),std::fmt::Error> {
        match self {
            Self::InvalidOperands(x) => 
                write!(f, "Invalid combination of operands: {:?}", x),
            Self::InvalidRstDest(x) => 
                write!(f, "Invalid destination of vector jump: {:#x}", x),
            Self::IdentAsRstDest(x) => 
                write!(f, "Identifier used as vector jump destination: {}", x),
            Self::InvalidLiteral(x,y) => 
                write!(f, "Literal '{}' out of range:  {:?}", x,y),
            Self::CodeTooLarge => 
                write!(f, "Code exceeds Gameboy memory"),
            Self::UnknownLabel(x) => 
                write!(f, "Unknown label: {}", x),
            Self::LabelOffsetOutOfRange(x,y,z) => 
                write!(f, "Label '{}' with offset {:#x} out of range for type {}", x,y,z),
        }
    }
}

/// The type to be inserted into the replacement slot in the `SyntaxTree`
#[derive(Debug,Copy,Clone,PartialEq,Eq)]
pub enum ReplacementType {
    /// u16
    U16,
    /// u8
    U8,
    /// i8
    I8,
}

impl Display for ReplacementType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(),std::fmt::Error> {
        match self {
            Self::U16 => write!(f, "u16"),
            Self::U8 => write!(f, "u8"),
            Self::I8 => write!(f, "i8"),
        }
    }
}

/// Returns the proper opcode for a rst instruction (jump to vector)
///
/// The vector offset is passed by the `x` argument
fn emit_rst(x: Value) -> Result<u8,Error> {
    if let Value::Literal(x) = x {
        return match x {
            0x00 => Ok(0xC7),
            0x08 => Ok(0xCF),
            0x10 => Ok(0xD7),
            0x18 => Ok(0xDF),
            0x20 => Ok(0xE7),
            0x28 => Ok(0xEF),
            0x30 => Ok(0xF7),
            0x38 => Ok(0xFF),
            _ => Err(Error::InvalidRstDest(x)),
        };
    } else if let Value::Ident(x) = x {
        return Err(Error::IdentAsRstDest(x));
    }
    unreachable!();
}

/// Emits a `Vec` of machine-code for a given `SyntaxTree`
///
/// Works in two phases, since the byte offsets of labels can't be known immediately, due
/// to variable instruction length.
///
/// First all instructions are compiled to machine code, but label references are 
/// filled with placeholder `0xFF` bytes, and added to the `replacements` vector.
/// Any labels and their byte offsets are recorded in the `labels` vector.
///
/// In the second phase all label offsets are known, and then inserted into all locations
/// recorded in the `replacements` vector.
pub fn emit(statements: SyntaxTree) -> Result<Vec<u8>,Error> {
    // Skips about a hundred repitions in the match statements further down
    use Operand::*;
    use ReplacementType::*;

    let mut output = Vec::<u8>::new();
    // List of all labels in the file
    let mut labels = HashMap::<String, u16>::new();
    // List of all locations which contain references to labels, and therefor need
    // to be replaced with the actual location in the second phase of emitting
    let mut replacements = Vec::<(usize, ReplacementType, String)>::new();
    
    // The statement (triplet of `Operand`'s that is currently being emitted, this is 
    // used for generating errors in the `p` macro
    let mut cur_statement;

    // Push arbitrary number of bytes to output, This macro is kinda hard to understand
    // but saves a lot of repition.
    macro_rules! p {
        // This branch pushes an opcode, adds filler bytes, and adds a record to the
        // `replacements` vector
        ($op:expr, replace $type:expr, $value:expr) => {{
            output.push($op);
            match $value {
                // If the value is a literal, it can be inserted immediately
                Value::Literal(x) => {
                    match $type {
                        U16 => { 
                            let bytes = u16::try_from(x)
                                .or(Err(Error::InvalidLiteral("u16".to_string(), x)))?
                                .to_le_bytes();
                            output.push(bytes[0]);
                            output.push(bytes[1]);
                        },
                        U8 => {
                            output.push(u8::try_from(x)
                                .or(Err(Error::InvalidLiteral("u8".to_string(), x)))?
                            );
                        }
                        I8 => {
                            output.push(i8::try_from(x)
                                .or(Err(Error::InvalidLiteral("i8".to_string(), x)))?
                                .to_le_bytes()
                                [0]
                            );
                        }
                    }
                },
                // If the value is an identifier (reference to label) it is instead 
                // recorded in the `replacements` table
                Value::Ident(s) => {
                    output.push(0xFF);
                    replacements.push((output.len()-1, $type, s.to_string()));
                    // `u16`'s need are two bytes long and thus needs another filler byte
                    if $type == U16 { output.push(0xFF); }
                }
            }
        }};
        // Many instructions are encoded in rows of 8, with the same order of arguments,
        // this macro thus saves 7 match-arms per use
        (row, $op:expr, $offset:expr) => {{
            output.push(match $op {
                RegB => $offset,
                RegC => $offset+1,
                RegD => $offset+2,
                RegE => $offset+3,
                RegH => $offset+4,
                RegL => $offset+5,
                LocHl => $offset+6,
                RegA => $offset+7,
                _ => return Err(Error::InvalidOperands(cur_statement)),
            });
        }};
        // Same as the branch above, but preceeds the instruction with a `0xCB` byte,
        // which unlocks 256 more instructions
        (cb_row, $op:expr, $offset:expr) => {{
            output.push(0xCB);
            output.push(match $op {
                RegB => $offset,
                RegC => $offset+1,
                RegD => $offset+2,
                RegE => $offset+3,
                RegH => $offset+4,
                RegL => $offset+5,
                LocHl => $offset+6,
                RegA => $offset+7,
                _ => return Err(Error::InvalidOperands(cur_statement)),
            });
        }};
        // This branch is the simplest! It just pushes an arbitrary number of bytes
        ($($arg:expr),+) => {{
            $(output.push($arg);)+
        }};
    }

    for statement in statements {
        //println!("{statement:#x?}");
        // First pass: match all possible `Operand` combinations, and emit the right 
        // machine code for that particular combination
        cur_statement = statement.clone();
        match statement {
            [Nop, Empty, Empty] => p!(0x00),
            [Ld, RegBc, Imm(x)] => p!(0x01, replace U16,x),
            [Ld, LocBc, RegA] => p!(0x02),
            [Inc, RegBc, Empty] => p!(0x03),
            [Inc, RegB, Empty] => p!(0x04),
            [Dec, RegB, Empty] => p!(0x05),
            [Ld, RegB, Imm(x)] => p!(0x06, replace U8,x),
            [Rlca, Empty, Empty] => p!(0x07),
            [Ld, LocImm(x), RegSp] => p!(0x08, replace U16,x),
            [Add, RegHl, RegBc] => p!(0x09),
            [Ld, RegA, LocBc] => p!(0x0A),
            [Dec, RegBc, Empty] => p!(0x0B),
            [Inc, RegC, Empty] => p!(0x0C),
            [Dec, RegC, Empty] => p!(0x0D),
            [Ld, RegC, Imm(x)] => p!(0x0E, replace U8,x),
            [Rrca, Empty, Empty] => p!(0x0F),
            [Stop, Empty, Empty] => p!(0x10),
            [Ld, RegDe, Imm(x)] => p!(0x11, replace U16,x),
            [Ld, RegDe, RegA] => p!(0x12),
            [Inc, RegDe, Empty] => p!(0x13),
            [Inc, RegD, Empty] => p!(0x14),
            [Dec, RegD, Empty] => p!(0x15),
            [Ld, RegD, Imm(x)] => p!(0x16, replace U8,x),
            [Rla, Empty, Empty] => p!(0x17),
            [Jr, Imm(x), Empty] => p!(0x18, replace I8,x),
            [Add, RegHl, RegDe] => p!(0x19),
            [Ld, RegA, LocDe] => p!(0x1A),
            [Dec, RegDe, Empty] => p!(0x1B),
            [Inc, RegE, Empty] => p!(0x1C),
            [Dec, RegE, Empty] => p!(0x1D),
            [Ld, RegE, Imm(x)] => p!(0x1E, replace U8,x),
            [Rra, Empty, Empty] => p!(0x1F),
            [Jr, ConNz, Imm(x)] => p!(0x20, replace I8,x),
            [Ld, RegHl, Imm(x)] => p!(0x21, replace U16,x),
            [Ld, LocHlP, RegA] => p!(0x22),
            [Inc, RegHl, Empty] => p!(0x23),
            [Inc, RegH, Empty] => p!(0x24),
            [Dec, RegH, Empty] => p!(0x25),
            [Ld, RegH, Imm(x)] => p!(0x26, replace U8,x),
            [Daa, Empty, Empty] => p!(0x27),
            [Jr, ConZ, Imm(x)] => p!(0x28, replace I8,x),
            [Add, RegHl, RegHl] => p!(0x29),
            [Ld, RegA, LocHlP] => p!(0x2A),
            [Dec, RegHl, Empty] => p!(0x2B),
            [Inc, RegL, Empty] => p!(0x2C),
            [Dec, RegL, Empty] => p!(0x2D),
            [Ld, RegL, Imm(x)] => p!(0x2E, replace U8,x),
            [Cpl, Empty, Empty] => p!(0x2F),
            [Jr, ConNc, Imm(x)] => p!(0x30, replace I8,x),
            [Ld, RegSp, Imm(x)] => p!(0x31, replace U16,x),
            [Ld, LocHlM, RegA] => p!(0x32),
            [Inc, RegSp, Empty] => p!(0x33),
            [Inc, LocHl, Empty] => p!(0x34),
            [Dec, LocHl, Empty] => p!(0x35),
            [Ld, LocHl, Imm(x)] => p!(0x36, replace U8,x),
            [Scf, Empty, Empty] => p!(0x37),
            [Jr, RegC, Imm(x)] => p!(0x38, replace I8,x),
            [Add, RegHl, RegSp] => p!(0x39),
            [Ld, RegA, LocHlM] => p!(0x3A),
            [Dec, RegSp, Empty] => p!(0x3B),
            [Inc, RegA, Empty] => p!(0x3C),
            [Dec, RegA, Empty] => p!(0x3D),
            [Ld, RegA, Imm(x)] => p!(0x3E, replace U8,x),
            [Ccf, Empty, Empty] => p!(0x3F),

            [Halt, Empty, Empty] => p!(0x76),

            [Ret, ConNz, Empty] => p!(0xC0),
            [Pop, RegBc, Empty] => p!(0xC1),
            [Jp, ConNz, Imm(x)] => p!(0xC2, replace U16,x),
            [Jp, Imm(x), Empty] => p!(0xC3, replace U16,x),
            [Call, ConNz, Imm(x)] => p!(0xC4, replace U16,x),
            [Push, RegBc, Empty] => p!(0xC5),
            [Add, RegA, Imm(x)] => p!(0xC6, replace U8,x),
            [Ret, ConZ, Empty] => p!(0xC8),
            [Ret, Empty, Empty] => p!(0xC9),
            [Jp, ConZ, Imm(x)] => p!(0xCA, replace U16,x),
            [Call, ConZ, Imm(x)] => p!(0xCC, replace U16,x),
            [Call, Imm(x), Empty] => p!(0xCD, replace U16,x),
            [Adc, RegA, Imm(x)] => p!(0xCE, replace U8,x),
            [Ret, ConNc, Empty] => p!(0xD0),
            [Pop, RegDe, Empty] => p!(0xD1),
            [Jp, ConNc, Imm(x)] => p!(0xD2, replace U16,x),
            [Call, ConNc, Imm(x)] => p!(0xD4, replace U16,x),
            [Push, RegDe, Empty] => p!(0xD5),
            [Sub, RegA, Imm(x)] => p!(0xD6, replace U8,x),
            [Ret, RegC, Empty] => p!(0xD8),
            [Reti, Empty, Empty] => p!(0xD9),
            [Jp, RegC, Imm(x)] => p!(0xDA, replace U16,x),
            [Call, RegC, Imm(x)] => p!(0xDC, replace U16,x),
            [Sbc, RegA, Imm(x)] => p!(0xDE, replace U8,x),
            [Ld, LocLpI(x), RegA] => p!(0xE0, replace U8,x),
            [Pop, RegHl, Empty] => p!(0xE1),
            [Ld, LocLpC, RegA] => p!(0xE2),
            [Push, RegHl, Empty] => p!(0xE5),
            [And, RegA, Imm(x)] => p!(0xE6, replace U8,x),
            [Add, RegSp, Imm(x)] => p!(0xE8, replace U8,x),
            [Jp, RegHl, Empty] => p!(0xE9),
            [Ld, LocImm(x), RegA] => p!(0xEA, replace U16,x),
            [Xor, RegA, Imm(x)] => p!(0xEE, replace U8,x),
            [Ld, RegA, LocLpI(x)] => p!(0xF0, replace U8,x),
            [Pop, RegAf, Empty] => p!(0xF1),
            [Ld, RegA, LocLpC] => p!(0xF2),
            [Di, Empty, Empty] => p!(0xF3),
            [Push, RegAf, Empty] => p!(0xF5),
            [Or, RegA, Imm(x)] => p!(0xF6, replace U8,x),
            [Ld, RegHl, RegSpI(x)] => p!(0xF8, replace I8,x),
            [Ld, RegSp, RegHl] => p!(0xF9),
            [Ld, RegA, LocImm(x)] => p!(0xFA, replace U16,x),
            [Ei, Empty, Empty] => p!(0xFB),
            [Cp, RegA, Imm(x)] => p!(0xFE, replace U8,x),
            
            [Rst, Imm(x), Empty] => p!(emit_rst(x)?),
            
            [Ld, RegB, x] => p!(row,x, 0x40),
            [Ld, RegC, x] => p!(row,x, 0x48),
            [Ld, RegD, x] => p!(row,x, 0x50),
            [Ld, RegE, x] => p!(row,x, 0x58),
            [Ld, RegH, x] => p!(row,x, 0x60),
            [Ld, RegL, x] => p!(row,x, 0x68),
            [Ld, LocHl, x] => p!(row,x, 0x70),
            [Ld, RegA, x] => p!(row,x, 0x78),

            [Add, RegA, x] => p!(row,x, 0x80),
            [Adc, RegA, x] => p!(row,x, 0x88),
            [Sub, RegA, x] => p!(row,x, 0x90),
            [Sbc, RegA, x] => p!(row,x, 0x98),
            [And, RegA, x] => p!(row,x, 0xA0),
            [Xor, RegA, x] => p!(row,x, 0xA8),
            [Or, RegA, x] => p!(row,x, 0xB0),
            [Cp, RegA, x] => p!(row,x, 0xB8),

            // CB
            [Rlc, x, Empty] => p!(cb_row,x, 0x00),
            [Rrc, x, Empty] => p!(cb_row,x, 0x08),
            [Rl, x, Empty] => p!(cb_row,x, 0x10),
            [Rr, x, Empty] => p!(cb_row,x, 0x18),
            [Sla, x, Empty] => p!(cb_row,x, 0x20),
            [Sra, x, Empty] => p!(cb_row,x, 0x28),
            [Swap, x, Empty] => p!(cb_row,x, 0x30),
            [Srl, x, Empty] => p!(cb_row,x, 0x38),

            [Bit, Imm(Value::Literal(0)), x] => p!(cb_row,x, 0x40),
            [Bit, Imm(Value::Literal(1)), x] => p!(cb_row,x, 0x48),
            [Bit, Imm(Value::Literal(2)), x] => p!(cb_row,x, 0x50),
            [Bit, Imm(Value::Literal(3)), x] => p!(cb_row,x, 0x58),
            [Bit, Imm(Value::Literal(4)), x] => p!(cb_row,x, 0x60),
            [Bit, Imm(Value::Literal(5)), x] => p!(cb_row,x, 0x68),
            [Bit, Imm(Value::Literal(6)), x] => p!(cb_row,x, 0x70),
            [Bit, Imm(Value::Literal(7)), x] => p!(cb_row,x, 0x78),

            [Res, Imm(Value::Literal(0)), x] => p!(cb_row,x, 0x80),
            [Res, Imm(Value::Literal(1)), x] => p!(cb_row,x, 0x88),
            [Res, Imm(Value::Literal(2)), x] => p!(cb_row,x, 0x90),
            [Res, Imm(Value::Literal(3)), x] => p!(cb_row,x, 0x98),
            [Res, Imm(Value::Literal(4)), x] => p!(cb_row,x, 0xA0),
            [Res, Imm(Value::Literal(5)), x] => p!(cb_row,x, 0xA8),
            [Res, Imm(Value::Literal(6)), x] => p!(cb_row,x, 0xB0),
            [Res, Imm(Value::Literal(7)), x] => p!(cb_row,x, 0xB8),

            [Set, Imm(Value::Literal(0)), x] => p!(cb_row,x, 0xC0),
            [Set, Imm(Value::Literal(1)), x] => p!(cb_row,x, 0xC8),
            [Set, Imm(Value::Literal(2)), x] => p!(cb_row,x, 0xD0),
            [Set, Imm(Value::Literal(3)), x] => p!(cb_row,x, 0xD8),
            [Set, Imm(Value::Literal(4)), x] => p!(cb_row,x, 0xE0),
            [Set, Imm(Value::Literal(5)), x] => p!(cb_row,x, 0xE8),
            [Set, Imm(Value::Literal(6)), x] => p!(cb_row,x, 0xF0),
            [Set, Imm(Value::Literal(7)), x] => p!(cb_row,x, 0xF8),
            
            // Labels are recorded in the `labels` map
            [Label(x), Empty, Empty] => { 
                labels.insert(
                    x.clone(), u16::try_from(output.len()).or(Err(Error::CodeTooLarge))?
                );
            },

            x => return Err(Error::InvalidOperands(x))
        };
    }
    
    // Second pass: take care of replacements
    for rep in replacements {
        // Try to find the label referenced by this particular replacement in the 
        // `labels` map
        let value = *labels.get(&rep.2).ok_or(Error::UnknownLabel(rep.2.to_string()))?;

        match rep.1 {
            U16 => {
                let bytes = value.to_le_bytes();
                output[rep.0] = bytes[0];
                output[rep.0+1] = bytes[1];
            },
            U8 => {
                output[rep.0] = u8::try_from(value)
                    .or(Err(Error::LabelOffsetOutOfRange(
                                rep.2.to_string(), 
                                rep.0,
                                U8
                    )))?;
            }
            I8 => {
                output[rep.0] = i8::try_from(value)
                    .or(Err(Error::LabelOffsetOutOfRange(
                                rep.2.to_string(), 
                                rep.0,
                                I8
                    )))?
                    .to_le_bytes()
                    [0];
            }
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use Operand::*;
    
    /// Simple test of a few instructions
    #[test]
    fn short() {
        let st: SyntaxTree = vec!(
            [Nop, Empty, Empty],
            [Nop, Empty, Empty],
            [Ld, RegC, RegA],
            [Rst, Imm(Value::Literal(0x0)), Empty],
        );
        let exp: Vec::<u8> = vec!(
            0x00,
            0x00,
            0x4F,
            0xC7,
        );
        let res = emit(st);
        assert!(res == exp);
    }
    
    /// Longer more advanced test contaning a modified snippet of the Gameboy boot ROM
    #[test]
    fn long() {
        let st: SyntaxTree = vec!(
            [Label("EntryPoint".to_string()), Empty, Empty],
            [Ld, RegSp, Imm(Value::Literal(0xfffe))],
            [Xor, RegA, RegA, ],
            [Ld, RegHl, Imm(Value::Literal(0x9fff))],
            [Label("clearVRAM".to_string()), Empty, Empty],
            [Ld, LocHlM, RegA],
            [Bit, Imm(Value::Literal(0x7)), RegH],
            [Jr, ConNz, Imm(Value::Literal(-5))],
            [Ld, RegHl, Imm(Value::Literal(0xff26))],
            [Ld, RegC, Imm(Value::Literal(0x11))],
            [Ld, RegA, Imm(Value::Literal(0x80))],
            [Ld, LocHlM, RegA],
            [Ld, LocLpC, RegA],
            [Inc, RegC, Empty],
            [Ld, RegA, Imm(Value::Literal(0xf3))],
            [Ld, LocLpC, RegA],
            [Ld, LocHlM, RegA],
            [Ld, RegA, Imm(Value::Literal(0x77))],
            [Ld, LocHl, RegA],
            [Ld, RegA, Imm(Value::Literal(0xfc))],
            [Ld, LocLpI(Value::Literal(0x47)), RegA, ],
            [Jp, Imm(Value::Ident("EntryPoint".to_string())), Empty],
        );
        let exp: Vec::<u8> = vec!(
			0x31, 0xfe, 0xff,
			0xaf,
			0x21, 0xff, 0x9f,
			0x32,
			0xcb, 0x7c,
			0x20, 0xfb,
			0x21, 0x26, 0xff,
			0x0e, 0x11,
			0x3e, 0x80,
			0x32,
			0xe2,
			0x0c,
			0x3e, 0xf3,
			0xe2,
			0x32,
			0x3e, 0x77,
			0x77,
			0x3e, 0xfc,
			0xe0, 0x47,
            0xc3, 0x00, 0x00,
        );
        let res = emit(st);
        assert!(res == exp);
    }
}
