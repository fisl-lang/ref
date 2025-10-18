

with open('build') as f:
    src = f.read()

prog = [
    x.split(' ')
    for x in
    src.split('\n')
    if x.strip()
]
print(prog)
pc = 0 

acc = 0
aux = 0

mem = [0 for _ in range(1000)]

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






