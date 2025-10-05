use std::collections::HashMap;


#[derive(Debug)]
enum Val {
    Imm(u32),
    Addr(u32),
}


#[derive(Debug)]
enum Ir {
    Pull(Val),
    Push(Val),
    Sub(u32),
    Return,
    Op{ 
        tar: Val, 
        a: Val, 
        b: Val,
        op: char,
    },
    Let{
        tar: Val,
        val: Val,
    },
    Print(Val),
}

#[derive(Debug)]
struct Prog {
    insts: Vec<Ir>,
    label: HashMap<String, u32>,
    entry: u32
}

struct Mapper {
    map: HashMap<String, u32>,
    alloc: u32
}

fn parse(iden: &str, mapper: &mut Mapper, alloc: bool) -> Val {
    if let Ok(num) = iden.parse::<u32>() { Val::Imm(num) }
    else if let Some(addr) = mapper.map.get(iden) {
        Val::Addr(*addr)
    }
    else if alloc {
        mapper.map.insert(iden.to_string(), mapper.alloc);
        let res = mapper.alloc;
        mapper.alloc += 1;
        Val::Addr(res)
    }
    else {
        eprint!("Identifier '{iden}' cannot be resolved or alloced.");
        std::process::exit(1);
    }

}


fn compile(src: &String) -> Prog {
    let mut prog   = Prog { insts: vec![], label: HashMap::new(), entry: 0 };
    let mut mapper = Mapper { map: HashMap::new(), alloc: 0 };

    for raw_line in src.split("\n") {
        let line = raw_line.trim();
        if line.is_empty() { continue; }

        let parts: Vec<&str> = line.split(' ').collect();
        match parts.as_slice() {
            ["lab",  name] => { prog.label.insert(name.to_string(), prog.insts.len() as u32); },
            ["pull", name] => { prog.insts.push( Ir::Pull(parse(name, &mut mapper, true ))); },
            ["push", name] => { prog.insts.push( Ir::Push(parse(name, &mut mapper, false))); },
            ["ret"] => { prog.insts.push( Ir::Return ); },
            ["sub", name] => {
                if let Some(addr) = prog.label.get(*name) {
                    prog.insts.push( Ir::Sub(*addr) )
                } else {
                    eprint!("Label '{name}' not defined. \n Note: Labels cannot forward reference. ");
                    std::process::exit(1);
                }
            },
            ["let", tar, "=", a, op, b] => {
                prog.insts.push( Ir::Op { 
                    tar: parse(tar,  &mut mapper, true),
                    a: parse(a, &mut mapper, false), 
                    b: parse(b, &mut mapper, false), 
                    op: op.chars().next().unwrap(), 
                })
            },
            ["let", tar, "=", val] => {
                prog.insts.push( Ir::Let { 
                    tar: parse(tar,  &mut mapper, true),
                    val: parse(val, &mut mapper, false), 
                })
            },
            ["print", x] => {
                prog.insts.push( Ir::Print(parse(x, &mut mapper, false)) );
            },
            x => { dbg!(x); }
        };
    }

    let Some(x) = prog.label.get("main") else {
        eprint!("Entry point 'main' not found.\n");
        std::process::exit(1);
    };
    prog.entry = *x;

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
    let mut index: u32 = prog.entry;
    let mut mem: Mem = [0; 2048];
    let mut stack: Vec<u32> = vec![];

    loop {
        let Some(inst) = prog.insts.get(index as usize) else { break; };
        index += 1;

        match inst {
            Ir::Push(v)            => stack.push(eval(v, &mem)),
            Ir::Pull(Val::Addr(x)) => mem[*x as usize] = stack.pop().expect("Stack underflow."),
            Ir::Pull(Val::Imm(_))  => {
                eprintln!("Cannot pull into immediate.");
                std::process::exit(1);
            },
            Ir::Return => {
                index = stack.pop().expect("Stack underflow.");
            },
            Ir::Sub(addr) => {
                stack.push(index);
                index = *addr;
            },
            Ir::Op { tar, a, b, op } => {
                let ra = eval(a, &mut mem);
                let rb = eval(b, &mut mem);
                let Val::Addr(addr) = tar else {
                    eprintln!("Cannot write to immediate.");
                    std::process::exit(1);
                };
                mem[*addr as usize] = match op {
                    '+' => ra + rb,
                    '-' => ra - rb,
                    _   => {
                        eprintln!("Operator '{op}' is not valid.");
                        std::process::exit(1);
                    },
                }
            },
            Ir::Let { tar, val } => {
                let Val::Addr(addr) = tar else {
                    eprintln!("Cannot write to immediate.");
                    std::process::exit(1);
                };
                mem[*addr as usize] = eval(val, &mut mem);
            },
            Ir::Print(x) => {
                println!("{}", eval(x, &mem));
            }
        }
    }
}




fn main() {

    let args: Vec<_> = std::env::args().collect();
    let src = std::fs::read_to_string(&args[1]).unwrap();
    let prog = compile(&src);
    run(prog);

}
