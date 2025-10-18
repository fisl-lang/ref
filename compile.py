
import sys



out = []
def emit(opcode, attr):
    out.append((opcode, attr))


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





def compile(path):
    with open(path, 'r') as f:
        lines = f.readlines()

    for line in lines:
        if not line.strip(): continue

        parts = lex(line)

        print(parts)

    



compile(sys.argv[1])

