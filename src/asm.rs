use std::{ops, collections::HashMap};

use crate::{parse::{ParseStream, Instruction, RegImm, Address, ILInner}, lex::Register, diagnostic::Diagnostic, error, spanned_error, Errors};

const OP_ADD:  u8 = 0x0 << 4;
const OP_SUB:  u8 = 0x1 << 4;
const OP_ADC:  u8 = 0x2 << 4;
const OP_SBC:  u8 = 0x3 << 4;
const OP_NAND: u8 = 0x4 << 4;
const OP_OR:   u8 = 0x5 << 4;
const OP_CMP:  u8 = 0x6 << 4;
const OP_MV:   u8 = 0x7 << 4;
const OP_LD:   u8 = 0x8 << 4;
const OP_ST:   u8 = 0x9 << 4;
const OP_LDA:  u8 = 0xA << 4;
const OP_PUSH: u8 = 0xB << 4;
const OP_POP:  u8 = 0xC << 4;
const OP_JNZ:  u8 = 0xD << 4;
const OP_IN:   u8 = 0xE << 4;
const OP_OUT:  u8 = 0xF << 4;

#[derive(Debug, Clone, Copy)]
enum OpCode {
    Single(u8),
    Double([u8; 2]),
    Triple([u8; 3]),
}

impl OpCode {
    const fn single(op: u8, reg: Register) -> OpCode {
        OpCode::Single(op | reg as u8)
    }

    const fn double(op: u8, reg: Register, reg_imm: RegImm) -> OpCode {
        let arr = match reg_imm {
            RegImm::Immediate(imm) => [op | (1<<3) | reg as u8, imm],
            RegImm::Register(regb) => [op | (0<<3) | reg as u8, regb as u8],
        };

        OpCode::Double(arr)
    }

    const fn triple(op: u8, reg: Register, addr: u16) -> OpCode {
        OpCode::Triple([op | reg as u8, (addr >> 8) as u8, (addr & 0xFF) as u8])
    }

    const fn single_triple(op: u8, reg: Register, hl: Option<u16>) -> OpCode {
        match hl {
            Some(addr) => OpCode::triple(op, reg, addr),
            None => OpCode::single(op, reg),
        }
    }

    fn from_instruction(instruction: Instruction, labels: &HashMap<String, u16>) -> Result<OpCode, Diagnostic> {
        use Instruction as Op;

        Ok(match instruction {
            Op::Add(a, b) => OpCode::double(OP_ADD, a, b),
            Op::Sub(a, b) => OpCode::double(OP_SUB, a, b),
            Op::Adc(a, b) => OpCode::double(OP_ADC, a, b),
            Op::Sbc(a, b) => OpCode::double(OP_SBC, a, b),
            Op::Nand(a, b) => OpCode::double(OP_NAND, a, b),
            Op::Or(a, b) => OpCode::double(OP_OR, a, b),
            Op::Cmp(a, b) => OpCode::double(OP_CMP, a, b),
            Op::Mv(a, b) => OpCode::double(OP_MV, a, b),
            Op::St(reg, hl) => OpCode::single_triple(OP_ST, reg, hl),
            Op::Ld(reg, hl) => OpCode::single_triple(OP_LD, reg, hl),
            Op::Lda(addr) => OpCode::triple(OP_LDA, Register::A, addr),
            Op::Push(val) => OpCode::double(OP_PUSH, Register::A, val),
            Op::Pop(reg) => OpCode::single(OP_POP, reg),
            Op::Jnz(addr) => OpCode::triple(OP_JNZ, Register::A, match addr {
                Address::Literal(addr) => addr,
                Address::Label(label) => *labels.get(&label.name).ok_or_else(|| spanned_error!(label.span, "could not find identifier `{}`", label.name))?,
            }),
            Op::In(reg, port) => OpCode::double(OP_IN, reg, port),
            Op::Out(port, reg) => OpCode::double(OP_OUT, reg, port),
        })
    }
}

impl ops::Deref for OpCode {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            OpCode::Single(op) => std::slice::from_ref(op),
            OpCode::Double(arr) => arr.as_slice(),
            OpCode::Triple(arr) => arr.as_slice(),
        }
    }
}

pub fn assemble(stream: ParseStream) -> Result<Vec<u8>, Errors> {
    let mut labels: HashMap<String, u16> = HashMap::new();
    let mut byte: u16 = stream.org.unwrap_or(0x0000);
    let mut parent: Option<String> = None;
    let mut errors = Vec::new();

    for il in stream.stream.iter() {
        match il.inner {
            ILInner::Instruction(ref instruct) => byte += instruct.len(),
            ILInner::Label(ref label) => {
                let l = if label.starts_with('.') {
                    match parent.as_ref(){
                        Some(p) => p.to_owned() + label,
                        None => {
                            errors.push(spanned_error!(il.span.clone(), "sublabel found with no parent label"));
                            continue;
                        },
                    }
                } else {
                    parent = Some(label.clone());
                    label.clone()
                };

                if let Some(_) = labels.insert(l, byte) {
                    if label.starts_with('.') {
                        errors.push(spanned_error!(il.span.clone(), "duplicate sublabel found"));
                    } else {
                        errors.push(spanned_error!(il.span.clone(), "duplicate label found").with_help(format!("consider making this a sublabel: `.{label}`")));
                    }
                }
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    let mut output = Vec::new();

    for il in stream.stream {
        match il.inner {
            ILInner::Label(_) => {},
            ILInner::Instruction(instruct) => {
                match OpCode::from_instruction(instruct, &labels) {
                    Ok(op) => output.extend_from_slice(&op),
                    Err(d) => errors.push(d),
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(output)
    } else {
        Err(errors)
    }
}
