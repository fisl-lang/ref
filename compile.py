#!/bin/python3

import sys
import os




def lex(line):
    stream = []

    buffer = ''
    string = False
    for char in line: 
        if char == '#': return []
        if char == '"': string = not string

        if char in (' ', '\n') and not string:
            if buffer:
                stream.append(buffer)
            buffer = ''
        elif char != '"':
            buffer += char

    return stream



out = ""
addr = 1 #reserve initial entry jump
def emit(opcode, attr=None):
    global out, addr
    out += f"{opcode} {attr if attr is not None else ''}\n"
    addr += 1

labels = {}
var = {}
consts = {}
alloc = 0

line_no = 1
def error(msg):
    print(f'Error on line {line_no}: {msg}')
    sys.exit(0)



def read(x):
    global var
    if x.isdigit():
        emit('ldi', int(x))
    elif x in consts:
        emit('ldi', consts[x])
    elif x in var:
        emit('lda', var[x])
    else:
        error(f'Unable to read from: {x}')

def ensure(x):
    global alloc
    global var
    if x not in var:
        var[x] = alloc
        alloc += 1

def write(x):
    ensure(x)
    emit('sta', var[x])


lib = '/data/dev/fisl-lang/lib'


def compile(path):
    with open(path, 'r') as f:
        lines = f.readlines()

    global line_no, alloc, consts
    for line_index, line in enumerate(lines):
        line_no = line_index + 1

        parts = lex(line)
        if not parts: continue

        print(parts)

        match parts:
            #preprocessed
            case ['use', name]:
                path = os.path.join(lib, name + ".fisl")
                compile(path)

            case ['label', name]: labels[name] = addr
            case ['constant', name, 'be', number]:
                if not number.isdigit():
                    error(f"Constant should be number, but is {number}")

                consts[name] = int(number)

            case ['let', tar, 'be', src]:
                read(src)
                write(tar)
            case ['let', tar, 'be', a, op, b]:
                read(b)
                emit('tfr')
                read(a)

                match op:
                    case 'plus':  emit('add')
                    case 'minus': emit('sub')
                    case x: error(f"Unknown operation: {x}")

                write(tar)

            case ['let', tar, 'beknown']:
                ensure(tar)

            case ['print', tar]:
                read(tar)
                emit('prt', 0)

            case ['pull', tar]:
                emit('pla')
                write(tar)
            case ['push', tar]:
                read(tar)
                emit('pha')
            case ['return']:
                emit('ret')
            case [inst, name] if inst in ('call', 'goto'): 
                if name in labels:
                    match inst:
                        case 'call': emit('ucl', labels[name])
                        case 'goto': emit('ugo', labels[name])
                else:
                    error(f"Label '{name}' not defined.\nNote: Labels cannot be forward referenced.")

            case ['if', a, comp, b, inst, name] if inst in ('call', 'goto'):
                read(b)
                emit('tfr')
                read(a)

                match comp:
                    case 'equal':   emit('ceq')
                    case 'unequal': emit('cue')
                    case 'lesser':  emit('clt')
                    case 'greater': emit('cgt')

                if name in labels:
                    match inst:
                        case 'call': emit('ccl', labels[name])
                        case 'goto': emit('cgo', labels[name])
                else:
                    error(f"Label '{name}' not defined.\nNote: Labels cannot be forward referenced.")

            case ['read', tar, 'from', ptr]:
                read(ptr)
                emit('tmi')
                emit('lmi')
                write(tar)
            case ['read', tar, 'from', ptr, 'at', offset]:
                read(ptr)
                emit('tfr')
                read(offset)
                emit('add')
                emit('tmi')
                emit('lmi')
                write(tar)

            case ['write', src, 'into', ptr]:
                read(ptr)
                emit('tmi')
                read(src)
                emit('smi')
            case ['write', src, 'into', ptr, 'at', offset]:
                read(ptr)
                emit('tfr')
                read(offset)
                emit('add')
                emit('tmi')
                read(src)
                emit('smi')



            case ['allocate', count, 'words', 'for', tar]:
                if not count.isdigit():
                    error("Allocation count must be number.")

                ptr = alloc
                alloc += int(count)

                emit('ldi', ptr)
                write(tar)

            case ['string', string, 'into', tar]:
                ptr = alloc
                alloc += len(string) + 1

                emit('ldi', ptr)
                write(tar)

                for i, char in enumerate(string + '\0'):
                    emit('ldi', ord(char))
                    emit('sta', ptr + i)

            case ['output', 'newline']:
                emit('ldi', ord('\n'))
                emit('out')

            case ['output', char]:
                read(char)
                emit('out')

            case ['input', char]:
                emit('inp')
                write(char)


            
            case _:
                error(f"Unable to compile: {line}")




compile(sys.argv[1])

entry = 'main'
if entry not in labels:
    error(f"Unable to locate entry point: {entry}")

build = f"ugo {labels[entry]}\n" + out

print(build)
print(labels)

with open('build', 'w') as f:
    f.write(build)

