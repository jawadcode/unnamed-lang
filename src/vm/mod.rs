use std::{
    fmt::Display,
    io::{self, Write},
    ops::{Add, Div, Index, Mul, Sub},
};

type Constant = u8;

#[derive(Clone, Copy)]
union Value {
    num: f64,
    boole: bool,
    undefined: (),
}

#[derive(Debug, Clone)]
#[repr(u8)]
enum Op {
    PushConst(u8),
    Pop,
    Load(u8),
    StoreConst(u8, Constant),
    StoreStack(u8),

    Add,
    Sub,
    Mul,
    Div,

    LessThan,
    LessOrEq,
    GreaterThan,
    GreaterOrEq,
    Eq,
    NotEq,

    Jump(u8),
    JumpIf(u8),
    JumpElse(u8),

    PrintNum,
}

struct VM {
    constants: Vec<Value>,
    chunk: Vec<Op>,
    registers: [Value; 256],
    stack: Vec<Value>,
    ip: usize,
}

macro_rules! binop {
	($self:ident, $field:ident, $op:tt) => {
        {
            let rhs = $self.pop();
            let lhs = $self.pop();
            $self.stack.push(Value {
                $field: lhs.num $op rhs.num,
            })
        }
    };
}

impl VM {
    pub fn new(constants: &[Value], chunk: &[Op]) -> Self {
        Self {
            constants: constants.to_vec(),
            chunk: chunk.to_vec(),
            registers: [Value { undefined: () }; 256],
            stack: vec![],
            ip: 0,
        }
    }

    pub unsafe fn run(&mut self) -> Option<Value> {
        while self.ip < self.chunk.len() {
            use Op::*;
            match self.chunk[self.ip].clone() {
                PushConst(constant) => self.stack.push(self.constants[constant as usize]),
                Pop => {
                    self.pop();
                }
                Load(register) => self.stack.push(self.registers[register as usize]),
                StoreConst(register, constant) => {
                    self.registers[register as usize] = self.constants[constant as usize]
                }
                StoreStack(register) => self.registers[register as usize] = self.pop(),

                Add => binop!(self, num, +),
                Sub => binop!(self, num, -),
                Mul => binop!(self, num, *),
                Div => binop!(self, num, /),

                LessThan => binop!(self, boole, <),
                LessOrEq => binop!(self, boole, <=),
                GreaterThan => binop!(self, boole, >),
                GreaterOrEq => binop!(self, boole, >=),
                Eq => binop!(self, boole, ==),
                NotEq => binop!(self, boole, !=),

                Jump(addr) => {
                    self.ip = addr as usize;
                    continue;
                }
                JumpIf(addr) => {
                    let cond = self.pop();
                    if cond.boole {
                        self.ip = addr as usize;
                        continue;
                    }
                }
                JumpElse(addr) => {
                    let cond = self.pop();
                    if !cond.boole {
                        self.ip = addr as usize;
                        continue;
                    }
                }
                PrintNum => {
                    println!("Number: {}", self.pop().num);
                }
            }

            // dbg!(self.stack.iter().map(|v| v.num).collect::<Vec<_>>());
            self.ip += 1;
        }

        self.stack.pop()
    }

    #[inline]
    unsafe fn pop(&mut self) -> Value {
        self.stack.pop().unwrap_unchecked()
    }
}

#[test]
fn test() {
    println!();
    let constants = vec![
        Value { num: 0.0 },
        Value { num: 1.0 },
        Value { num: 100000.0 },
    ];

    use Op::*;
    let chunk = vec![
        // Loop
        StoreConst(0, 0),
        Load(0),
        PushConst(1),
        Add,
        StoreStack(0),
        // Print
        Load(0),
        PrintNum,
        // Check
        Load(0),
        PushConst(1),
        Add,
        PushConst(2),
        LessOrEq,
        JumpIf(1),
    ];

    let mut vm = VM::new(&constants, &chunk);
    println!("{:#?}", unsafe { vm.run().map(|res| res.num) });
}
