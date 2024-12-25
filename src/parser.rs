//! Parses source code from text to an abstract syntax representation

use std::io::BufRead;
use std::fmt::{Display,Formatter};

use crate::{Operand, SyntaxTree, Value};

/// A compile-time error that occured during the parsing stage
#[derive(Debug)]
pub enum Error {
    /// More than three operands in one line of source code
    TooManyOperands,
    /// Source code contains invalid UTF8 characters
    InvalidUTF8,
    /// Invalid Operand
    InvalidOperand(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(),std::fmt::Error> {
        match self {
            Self::TooManyOperands => write!(f, "Too many operands"),
            Self::InvalidUTF8 => write!(f, "Source contains invalid character(s)"),
            Self::InvalidOperand(x) => write!(f, "Invalid operand: {}", x),
        }
    }
}


/// Parses a literal from string slice `s`
///
/// # Returns
/// `Value` enum, containing either an immediate number, or an identifier (label).
/// Supports both hex and decimal immediates.
fn parse_literal(s: &str) -> Value {
    // Decimal immediate
    if let Ok(i) = s.parse::<i32>() {
        return Value::Literal(i);
    }
    // Hex immediate
    if let Some(stripped) = s.strip_prefix("0x") {
        if let Ok(i) = i32::from_str_radix(stripped, 16) {
            return Value::Literal(i);
        }
    }
    if let Some(stripped) = s.strip_prefix("0b") {
        if let Ok(i) = i32::from_str_radix(stripped, 2) {
            return Value::Literal(i);
        }
    }
    // Not a number, meaning it probably is an identifier to a label
    Value::Ident(s.to_string())
}

/// Parses text from `input` to a `SyntaxTree`
pub fn parse(input: impl BufRead) -> Result<SyntaxTree,Error> {
    let mut output = SyntaxTree::new();

    let mut op_i;

    'outer: for line in input.lines() {
        let line = line.or(Err(Error::InvalidUTF8))?;
        //println!("---");
        // counts the current slot in the `Operand` triplet for this particular line
        op_i = 0;
        let words = line.trim().split(&[' ', ',']);
        for word in words {
            //println!("'{word}'");
            // Skip rest of line if comment
            if word.starts_with(';') { continue 'outer; }
            // Some scenarias can lead to empty words, skip these words
            if word.is_empty() { continue; }

            let operand = match word {
                "nop" => Operand::Nop,
                "stop" => Operand::Stop,
                "halt" => Operand::Halt,
                "ld" => Operand::Ld,   
                "inc" => Operand::Inc,
                "dec" => Operand::Dec,
                "add" => Operand::Add,
                "rrca" => Operand::Rrca,
                "rra" => Operand::Rra,
                "rlca" => Operand::Rlca,
                "rla" => Operand::Rla,
                "daa" => Operand::Daa,
                "scf" => Operand::Scf,
                "ccf" => Operand::Ccf,
                "cpl" => Operand::Cpl,
                "call" => Operand::Call,
                "ret" => Operand::Ret,
                "push" => Operand::Push,
                "pop" => Operand::Pop,
                "adc" => Operand::Adc,
                "sub" => Operand::Sub,
                "sbc" => Operand::Sbc,
                "xor" => Operand::Xor,
                "or" => Operand::Or,
                "and" => Operand::And,
                "cp" => Operand::Cp,
                "jr" => Operand::Jr,
                "jp" => Operand::Jp,
                "rst" => Operand::Rst,
                "ei" => Operand::Ei,
                "di" => Operand::Di,
                "reti" => Operand::Reti,
                "rlc" => Operand::Rlc,
                "rrc" => Operand::Rrc,
                "rl" => Operand::Rl,
                "rr" => Operand::Rr,
                "sla" => Operand::Sla,
                "sra" => Operand::Sra,
                "swap" => Operand::Swap,
                "srl" => Operand::Srl,
                "bit" => Operand::Bit,
                "res" => Operand::Res,
                "set" => Operand::Set,

                "a" => Operand::RegA,
                "b" => Operand::RegB,
                "c" => Operand::RegC,
                "d" => Operand::RegD,
                "e" => Operand::RegE,
                "h" => Operand::RegH,
                "l" => Operand::RegL,
                "af" => Operand::RegAf,
                "bc" => Operand::RegBc,
                "de" => Operand::RegDe,
                "hl" => Operand::RegHl,
                "sp" => Operand::RegSp,
                "(bc)" => Operand::LocBc,
                "(de)" => Operand::LocDe,
                "(hl)" => Operand::LocHl,
                "(hl+)" => Operand::LocHlP,
                "(hl-)" => Operand::LocHlM,
                "(0xff00+c)" => Operand::LocLpC,

                "nz" => Operand::ConNz,
                "z" => Operand::ConZ,
                "nc" => Operand::ConNc,
                // RegC will have to act as ConC aswell since they look 
                // identical in source
                //"c" => Operand::ConC,
                
                // TODO: remove debug tokens
                "END" => break 'outer,
                
                // This match-arm parses everything "harder" than a string, i.e.
                // literals and label definitions
                s => {
                    let mut op = Operand::Empty;

                    // Label
                    if s.ends_with(':') {
                        let end = s.len()-1;
                        let s = s[..end].to_string();
                        if !s.contains(|x: char| !x.is_ascii_alphanumeric() && x!='_') {
                            op = Operand::Label(s[..end].to_string());
                        }
                    // Last page offset addressing
                    } else if s.starts_with("(0xff00+") && s.ends_with(')') {
                        let end = s.len()-1;
                        op = Operand::LocLpI(parse_literal(&s[8..end]));
                    // sp+i8 addressing
                    } else if let Some(stripped) = s.strip_prefix("sp+") {
                        op = Operand::RegSpI(parse_literal(stripped));
                    // Immediate location
                    } else if s.starts_with('(') && s.ends_with(')') {
                        let end = s.len()-1;
                        op = Operand::LocImm(parse_literal(&s[1..end]));
                    // If we've fallen through all the way here, it must be an immediate
                    // number
                    } else if 
                        !s.contains(|x: char| !x.is_ascii_alphanumeric() && x!='_')
                    {
                        op = Operand::Imm(parse_literal(s));
                    } else {
                        return Err(Error::InvalidOperand(s.to_string()));
                    }
                    op 
                },
            };
            
            // Insert the newly parsed token into the correct slot in the `Operand`
            // triplet
            let len = output.len();
            match op_i {
                0 => { output.push([operand, Operand::Empty, Operand::Empty]); },
                1 => { output[len-1][1] = operand; },
                2 => { output[len-1][2] = operand; },
                _ => { return Err(Error::TooManyOperands) },
            }
            op_i += 1;
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
        let input: &str = "
            ; Comment
            nop
            nop
            ld c, a
            rst 0
        ";
        let exp: SyntaxTree = vec!(
            [Nop, Empty, Empty],
            [Nop, Empty, Empty],
            [Ld, RegC, RegA],
            [Rst, Imm(Value::Literal(0)), Empty],
        );
        let res = parse(input.as_bytes());
        assert!(res == exp);
    }

    /// Longer more advanced test contaning a modified snippet of the Gameboy boot ROM
    #[test]
    fn long() {
        let input: &str = "
            EntryPoint:
                ; ld sp, hStackBottom
                ld sp, 0xfffe                   ; 31 fe ff

                xor a,a                         ; af
                ; ld hl, _VRAM + SIZEOF(VRAM) - 1
                ld hl, 0x9fff                   ; 21 ff 9f
            clearVRAM:
                ld (hl-), a                     ; 32
                bit 7, h                        ; cb 7c
                ; TODO: Support `ident`'s in relative jumps
                ; jr nz, clearVRAM
                jr nz, -5                       ; 20 fb

                ; ld hl, rNR52
                ld hl, 0xff26                   ; 21 26 ff
                ; ld c, LOW(rNR11) ; CH1 length
                ld c, 0x11                      ; 0e 11
                ; Enable APU
                ; This sets (roughly) all audio registers to 0
                ; ld a, AUDENA_ON
                ld a, 0x80                      ; 3e 80
                ld (hl-), a                     ; 32
                ; ASSERT rNR52 - 1 == rNR51
                ; Set CH1 duty cycle to 25%
                ; ASSERT AUDENA_ON == AUDLEN_DUTY_50
                ld (0xff00+c), a                ; e2
                inc c                           ; 0c
                ; ASSERT LOW(rNR11) + 1 == LOW(rNR12)
                ; Set CH1 envelope
                ; ld a, (15 << 4) | AUDENV_DOWN | 3 ; Initial volume 15, decreasing sweep 3
                ld a, 0xf3                      ; 3e f3
                ld (0xff00+c), a                ; e2
                ; Route all channels to left speaker, CH2 and CH1 to right speaker
                ; Note that only channel 1 will be used!
                ld (hl-), a                     ; 32
                ; ASSERT rNR51 - 1 == rNR50
                ; Set volume on both speakers to max, disable VIN on both speakers
                ld a, 0x77                      ; 3e 77
                ld (hl), a                      ; 77

                ; ld a, %11_11_11_00
                ld a, 0xfc                      ; 3e fc
                ; ldh (rBGP], a
                ld (0xff00+0x47), a             ; e0 47
                jp EntryPoint
        ";
        let exp: SyntaxTree = vec!(
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
        let res = parse(input.as_bytes());
        println!("{res:#x?}");
        assert!(res == exp);
    }

    /// Test function of binary literals
    #[test]
    fn bin_lit() {
        let input: &str = "ld a,0b10101010";
        let exp: SyntaxTree = vec!([Ld, RegA, Imm(Value::Literal(0b10101010))]);
        let res = parse(input.as_bytes());
        assert!(res == exp);
    }
}
