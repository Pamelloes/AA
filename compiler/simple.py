import compiler_base as CB

def compile(inp,out):
  opc=""
  while True:
    c=inp.read(1)
    if not c: break

    if c in "01": CB.write_bits(out,c)
    elif c.isspace():
      if not opc: continue
      CB.write_bits(out,CB.opcodes[opc.upper()])
      opc=""
    else: opc += c
  CB.flush_bits(out)

if __name__ == "__main__":
  CB.main(compile)
