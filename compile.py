
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



out = []
def emit(opcode, attr=None):
    out.append((opcode, attr))

labels = {}
vars = {}
alloc = 0

def error(msg):
    print(f'Error: {msg}')
    sys.exit(0)



def read(x):
    if x.isdigit():
        emit('ldi', int(x))
    elif x in vars:
        emit('lda', vars[x])
    else:
        error(f'Unable to read from: {x}')

def write(x):
    global alloc
    if x not in vars:
        vars[x] = alloc
        alloc += 1
    emit('sta', vars[x])



def compile(path):
    with open(path, 'r') as f:
        lines = f.readlines()

    for line in lines:
        if not line.strip(): continue

        parts = lex(line)
        print(parts)

        match parts:
            case ['label', name]: labels[name] = len(out)
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

print(out)

