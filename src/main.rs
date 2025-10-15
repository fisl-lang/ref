use std::collections::HashMap;


type Address = usize;

#[derive(Debug)]
enum Val {
    Imm(u32),
    Addr(Address),
}

#[derive(Debug)] enum ExprKind { Add, Sub }
#[derive(Debug)] enum CompKind { Equal, Unequal, Greater, Lesser }

#[derive(Debug)]
enum Ir {
    Pull(Val),
    Push(Val),
    Call(Address),
    Return,
    Op{
        tar: Address, 
        a: Val, 
        b: Val,
        kind: ExprKind,
    },
    Let{
        tar: Address,
        val: Val,
    },
    Print(Val),
    Uncon(Address),
    Con{
        a: Val,
        b: Val,
        addr: Address,
        kind: CompKind
    }
}

#[derive(Debug)]
struct Prog {
    insts: Vec<Ir>,
    label: HashMap<String, Address>,
    entry: Option<Address>
}

impl Prog {
    fn new() -> Self {
        Prog {
            insts: vec![], 
            label: HashMap::new(), 
            entry: None
        }    
    }
    fn merge(self: &mut Self, mut other: Prog) {
        self.insts.append(&mut other.insts);
        self.label.extend(other.label);
        //notice, entry it never overwritten.
        //it can only come from the root source.
    }
}

struct Mapper {
    map: HashMap<String, u32>,
    alloc: u32
}

fn error(msg: String) -> ! {
    eprintln!("Error: {msg}\n");
    std::process::exit(1);
}

fn parse(iden: &str, mapper: &mut Mapper, alloc: bool) -> Val {
    if let Ok(num) = iden.parse::<u32>() { Val::Imm(num) }
    else {
        Val::Addr(parse_address(iden, mapper, alloc))
    }
}

fn parse_address(iden: &str, mapper: &mut Mapper, alloc: bool) -> Address {
    if let Some(addr) = mapper.map.get(iden) {
        *addr as Address
    }
    else if alloc {
        mapper.map.insert(iden.to_string(), mapper.alloc);
        let res = mapper.alloc;
        mapper.alloc += 1;
        res as Address
    }
    else {
        error(format!("Identifier '{iden}' cannot be resolved or alloced."))
    }
}


fn lookup(name: &str, prog: &Prog) -> Address  {
    if let Some(addr) = prog.label.get(name) {
        *addr
    } else {
        error(format!("Label '{name}' not defined. \n Note: Labels cannot forward reference."));
    }
}


fn compile(src: &String) -> Prog {
    let mut prog   = Prog::new();
    let mut mapper = Mapper { map: HashMap::new(), alloc: 0 };

    for raw_line in src.split("\n") {
        let line = raw_line.trim();
        if line.is_empty() { continue; }

        let parts: Vec<&str> = line.split(' ').collect();
        match parts.as_slice() {
            ["use",  name] => {
                let rel_path = format!("lib/{name}.fisl");
                let Ok(src) = std::fs::read_to_string(&rel_path) else {
                    error(format!("Module at '{rel_path}' not found."));
                };
                prog.merge(compile(&src)); 
            },
            ["label", name] => { prog.label.insert(name.to_string(), prog.insts.len() as Address); },
            ["pull", name] => { prog.insts.push( Ir::Pull(parse(name, &mut mapper, true ))); },
            ["push", name] => { prog.insts.push( Ir::Push(parse(name, &mut mapper, false))); },
            ["return"] => { prog.insts.push( Ir::Return ); },
            ["call", name] => {
                prog.insts.push( Ir::Call(lookup(*name, &prog)) )
            },
            ["let", tar, "be", a, op, b] => {
                let tar = parse_address(tar, &mut mapper, true);
                let a   = parse(a, &mut mapper, false);
                let b   = parse(b, &mut mapper, false);

                let kind = match *op {
                    "plus"  => ExprKind::Add,
                    "minus" => ExprKind::Sub,
                    _ => error(format!("Unknown operation {op}"))
                };
                prog.insts.push(Ir::Op{ tar, a, b, kind });
            },
            ["let", tar, "be", val] => {
                prog.insts.push( Ir::Let { 
                    tar: parse_address(tar, &mut mapper, true ),
                    val: parse        (val, &mut mapper, false), 
                })
            },
            ["print", x] => {
                prog.insts.push( Ir::Print(parse(x, &mut mapper, false)) );
            },
            ["goto", name] => {
                prog.insts.push( Ir::Uncon(lookup(*name, &prog)) )
            },
            ["if", ea, x, eb, "goto", dest] => {
                let a: Val = parse(ea, &mut mapper, false);
                let b: Val = parse(eb, &mut mapper, false);
                let addr: Address = lookup(*dest, &prog);
                let kind = match *x {
                    "equal"   => CompKind::Equal,
                    "unequal" => CompKind::Unequal,
                    "greater" => CompKind::Greater,
                    "lesser"  => CompKind::Lesser,
                    x         => error(format!("Invalid comperator {x}."))
                };
                prog.insts.push(Ir::Con{ a, b, addr, kind })
            },
            _ => { error(format!("Unable to parse line: {line}")) }
        };
    }

    prog.entry = prog.label.get("main").cloned();
    prog
}

type Mem = [u32; 2048];


fn eval(val: &Val, mem: &Mem) -> u32 {
    match *val {
        Val::Imm(x) => x,
        Val::Addr(x) => *mem.get(x as usize).unwrap_or(&0)
    }
}


fn run(prog: Prog) {
    let Some(mut index) = prog.entry else {
        error("Entry point 'main' not found.".to_string());
    };
    let mut mem: Mem = [0; 2048];
    let mut stack: Vec<u32> = vec![];

    loop {
        let Some(inst) = prog.insts.get(index) else { break; };
        index += 1;

        match inst {
            Ir::Push(v)            => stack.push(eval(v, &mem)),
            Ir::Pull(Val::Addr(x)) => mem[*x as usize] = stack.pop().expect("Stack underflow."),
            Ir::Pull(Val::Imm(_))  => error("Cannot pull into immediate.".to_string()),
            Ir::Return => {
                index = stack.pop().unwrap_or_else(
                    || error("Stack underflow.".to_string())
                ) as Address;
            },
            Ir::Call(addr) => {
                stack.push(index as u32);
                index = *addr;
            },
            Ir::Op { tar, a, b, kind } => {
                let ra = eval(a, &mut mem);
                let rb = eval(b, &mut mem);

                mem[*tar] = match kind {
                    ExprKind::Add => ra + rb,
                    ExprKind::Sub => ra - rb,
                }

            },
            Ir::Let { tar, val } => {
                mem[*tar as usize] = eval(val, &mut mem);
            },
            Ir::Print(x) => {
                println!("{}", eval(x, &mem));
            },
            Ir::Uncon(addr) => {
                index = *addr
            },
            Ir::Con { a, b, addr, kind } => {
                let ra = eval(a, &mut mem);
                let rb = eval(b, &mut mem);

                let branch: bool = match kind {
                    CompKind::Equal   => ra == rb,
                    CompKind::Unequal => ra != rb,
                    CompKind::Greater => ra < rb,
                    CompKind::Lesser  => ra > rb,
                };

                if branch { index = *addr; }
            },
        }
    }
}




fn main() {

    let args: Vec<_> = std::env::args().collect();
    let src = std::fs::read_to_string(&args[1]).unwrap();
    let prog = compile(&src);
    run(prog);

}
