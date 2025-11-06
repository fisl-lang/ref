#!/bin/python3

import sys
import argparse
import itertools

parser = argparse.ArgumentParser(epilog=":3")
parser.add_argument("path", help='bytecode path file.', default='build', nargs='?')
parser.add_argument("--core", help='core dump upon exit. dot for stdout.')
args = parser.parse_args()


with open(args.path) as f:
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
imr = 0 #indirect memory register

mem = [0 for _ in range(2048)]
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

        case 'tmi': imr = acc
        case 'lmi': acc = mem[imr]
        case 'smi': mem[imr] = acc

        case 'out': print(chr(acc), end='')
        case 'inp': acc = ord(sys.stdin.read(1))

        case x:
            print(f"Invalid instruction: {x}")
            sys.exit(0)


if args.core is not None:
    (
        sys.stdout 
        if args.core == '.' 
        else open(args.core, 'w')
    ).write("\n".join(
        f"{hex(addr):<4}: {content}"
        for addr, content
        in enumerate(mem)
    ))







