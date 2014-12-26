import sys
import compiler_base as CB

def compile(inp,out):
  opc=""
  while True:
    c=inp.read(1)
    if not c: break

    if c in "01": CB.write_bits(out,c)
    elif c.isspace():
      if not opc: continue
      if not opc.upper() in CB.opcodes:
        sys.stderr.write("Error: Unknown opcode '%s'\n" % opc.upper())
        sys.exit(-1)
      CB.write_bits(out,CB.opcodes[opc.upper()])
      opc=""
    else: opc += c
  CB.flush_bits(out)

if __name__ == "__main__":
  CB.main(compile)
