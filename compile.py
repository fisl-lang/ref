
import sys





def lex(line):
    stream = []

    buffer = ''
    string = False
    for char in line: 
        if char == '#': break
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
alloc = 0

def error(msg):
    print(f'Error: {msg}')
    sys.exit(0)



def read(x):
    global var
    if x.isdigit():
        emit('ldi', int(x))
    elif x in var:
        emit('lda', var[x])
    else:
        error(f'Unable to read from: {x}')

def write(x):
    global alloc
    global var
    if x not in var:
        var[x] = alloc
        alloc += 1
    emit('sta', var[x])



def compile(path):
    with open(path, 'r') as f:
        lines = f.readlines()

    for line in lines:
        if not line.strip(): continue

        parts = lex(line)
        print(parts)

        match parts:
            case ['label', name]: labels[name] = addr
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

            case ['print', tar]:
                read(tar)
                emit('prt', 0)

            case ['if', a, comp, b, 'goto', name]:
                read(b)
                emit('tfr')
                read(a)

                match comp:
                    case 'equal':   emit('ceq')
                    case 'unequal': emit('cue')
                    case 'lesser':  emit('clt')
                    case 'greater': emit('cgt')

                if name in labels:
                    emit('cgo', labels[name])
                else:
                    error("Label '{name}' not defined.\nNote: Labels cannot be forward referenced.")

                
            
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

