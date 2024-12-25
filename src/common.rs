//! Common types used both in the parser and emitter

/// Wrapper type for `Vec` of triplets of `Operand`'s. Each statement in this flavour of
/// Gameboy assembly can be represented as 3 `Operand`'s so that is all we need.
pub type SyntaxTree = Vec<[Operand;3]>;

/// Represents an immediate value in the assembly code
#[derive(Debug,Clone,PartialEq,Eq)]
pub enum Value {
    /// Literal value hard-coded into the source
    ///
    /// This is represented as an `i32` as that is the smallest type that can contain all
    /// bossible types of immediates (`u8`, `i8`, and `u16`)
    Literal(i32),
    /// A reference (or identifier) to a label in the source
    Ident(String)
}

/// An operand, or "word" of assembly source code. This is the smallest sub-part that the
/// source is parsed into. This therefor contains all instructions, registers, branch
/// conditions, addressing modes and as well as label definition (`Operand::Label`), and
/// usage (`Operand::Ident`)
#[derive(Debug,Clone,PartialEq,Eq)]
pub enum Operand {
    /// An empty spot, meaning no operand. Having this as a variant allows us to
    /// represent each statement as a fixed triplet of Operands, insteac of having a 
    /// variable size statement type
    Empty,
    /// Label definition
    Label(String),
    
    /// No operation instruction
    Nop,
    /// Instruction. Enter CPU into a very low power mode.
    Stop,
    /// Instruction. Enter low-power mode until an interrupt occurs.
    Halt,
    /// Instruction. Copy value.
    Ld,
    /// Instruction. Increment value.
    Inc,
    /// Instruction. Decrement value.
    Dec,
    /// Instruction. Addition.
    Add,
    /// Instruction. Rotate register A right.
    Rrca,
    /// Instruction. Rotate register A right through carry flag.
    Rra,
    /// Instruction. Rotate register A left.
    Rlca,
    /// Instruction. Rotate register A left through carry flag.
    Rla,
    /// Instruction. Decimal adjust accumulator to get a correct BCD representation after
    /// an arithmetic instruction.
    Daa,
    /// Instrution. Set carry flag.
    Scf,
    /// Instruction. Clear carry flag.
    Ccf,
    /// Instruction. Complement carry flag.
    Cpl,
    /// Instruction. Call sub-routine.
    Call,
    /// Instruction. Return from sub-routine.
    Ret,
    /// Instruction. Push value to the stack.
    Push,
    /// Instruction. Pop value off the stack.
    Pop,
    /// Instruction. Add with carry
    Adc,
    /// Instruction. Subtract.
    Sub,
    /// Instruction. Subtract with carry.
    Sbc,
    /// Instruction. Exclusive or.
    Xor,
    /// Instruction. Logical or.
    Or,
    /// Instruction. Logical and.
    And,
    /// Instruction. Compare values.
    Cp,
    /// Instruction. Relative jump.
    Jr,
    /// Instruction. Absolute jump.
    Jp,
    /// Instruction. Call vector
    Rst,
    /// Instruction. Enable interrupts.
    Ei,
    /// Instruction. Disable interrupts.
    Di,
    /// Instruction. Return and enable interrupts.
    Reti,
    /// Instruction. Rotate left
    Rlc,
    /// Instruction. Rotate right
    Rrc,
    /// Instruction. Rotate left through carry flag
    Rl,
    /// Instruction. Rotate right through carry flag
    Rr,
    /// Instruction. Arithmetic shift left.
    Sla,
    /// Instruction. Arithmetic shift right.
    Sra,
    /// Instruction. Swap values.
    Swap,
    /// Instruction. Logical shift right.
    Srl,
    /// Instruction. Test bit.
    Bit,
    /// Instruction. Reset bit.
    Res,
    /// Instruction. Set bit.
    Set,
    
    /// Register a
    RegA,
    /// Register b
    RegB,
    /// Register c OR carry flag set condition. The reuse of this token does not cause
    /// collisions or other issues
    RegC,
    /// Register d
    RegD,
    /// Register e
    RegE,
    /// Register h
    RegH,
    /// Register l
    RegL,
    /// Register bc
    RegBc,
    /// Register de
    RegDe,
    /// Register af
    RegAf,
    /// Register hl
    RegHl,
    /// Register sp
    RegSp,
    /// Memory location (bc)
    LocBc,
    /// Memory location (de)
    LocDe,
    /// Memory location (hl)
    LocHl,
    /// Memory location (hl+)
    LocHlP,
    /// Memory location (hl-)
    LocHlM,
    /// Memory location (0xff00+c)
    LocLpC,
    
    /// Memory location (0xff00+u8)
    LocLpI(Value),
    /// Special addressing mode for instruction ld hl,sp+i8, which combines a load and 
    /// addition
    RegSpI(Value),
    /// Literal integer constant
    // TODO: Rename Imm
    Imm(Value),
    /// Literal memory location addressing (i.e. `(0x15)`)
    LocImm(Value),
    
    /// Non-zero condition
    ConNz,
    /// Zero condition
    ConZ,
    /// Non-carry condition
    ConNc,
}
