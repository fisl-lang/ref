use std::collections::HashMap;


#[derive(Debug)]
enum Val {
    Imm(u32),
    Addr(u32)
}


#[derive(Debug)]
enum Ir {
    Pull(Val),
    Push(Val),
    Sub(u32),
    Return,
    Let{ 
        tar: Val, 
        a: Val, 
        b: Val,
        op: char,
    },
    Print(Val),
}

#[derive(Debug)]
struct Prog {
    insts: Vec<Ir>,
    label: HashMap<String, u32>,
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
    let mut prog   = Prog { insts: vec![], label: HashMap::new() };
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
                    eprint!("Label '{name}' not defined. \n Note: Labels cannot back-reference. ");
                    std::process::exit(1);
                }
            },
            ["let", tar, "=", a, op, b] => {
                prog.insts.push( Ir::Let { 
                    tar: parse(tar,  &mut mapper, true),
                    a: parse(a, &mut mapper, false), 
                    b: parse(b, &mut mapper, false), 
                    op: op.chars().next().unwrap(), 
                })
            },
            ["print", x] => {
                prog.insts.push( Ir::Print(parse(x, &mut mapper, false)) );
            },
            x => { dbg!(x); }
        };
    }


    prog
}



fn main() {

    let args: Vec<_> = std::env::args().collect();
    let src = std::fs::read_to_string(&args[1]).unwrap();
    let prog = compile(&src);

    dbg!(prog);
}
