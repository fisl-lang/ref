import sys

with open('build') as f:
    src = f.read()

prog = [
    x.split(' ')
    for x in
    src.split('\n')
    if x.strip()
]
pc = 0 

acc = 0
aux = 0

mem = [0 for _ in range(1000)]
stack = []

while pc < len(prog):
    inst, *tail = prog[pc]
    pc += 1

    #tail owo
    attr = int(tail[0]) if tail[0] else None

    match inst:
        case 'ldi': acc = attr
        case 'lda': acc = mem[attr]
        case 'sta': mem[attr] = acc
        case 'tfr': aux = acc
        case 'add': acc += aux
        case 'sub': acc -= aux
        case 'prt': print(acc)

        case 'ceq': acc = int(acc == aux)
        case 'cue': acc = int(acc != aux)
        case 'clt': acc = int(acc <  aux)
        case 'cgt': acc = int(acc >  aux)

        case 'cgo': pc = attr if acc != 0 else pc
        case 'ugo': pc = attr

        case 'pha': stack.append(acc)
        case 'pla': acc = stack.pop()

        case 'ucl':
            stack.append(pc)
            pc = attr

        case 'ccl':
            if acc != 0:
                stack.append(pc)
                pc = attr

        case 'ret':
            pc = stack.pop()

        case 'ldp': acc = mem[mem[attr]]
        case 'stp': mem[mem[attr]] = acc

        case x:
            print(f"Invalid instruction: {x}")
            sys.exit(0)






