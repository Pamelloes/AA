import argparse
import os

bts=""
def write_bits(out,bit):
  global bts
  bts = bts+bit
  while len(bts)>8:
    out.write(chr(int(bts[0:8],2)))
    bts=bts[8:]
def flush_bits(out):
  write_bits(out,"")

  global bts
  if len(bts) == 0: return
  while len(bts) < 8: bts+="0"
  out.write(chr(int(bts[0:8],2)))

opcodes={
  'ES':'0',
  'CS':'1',

  'AN' :'0',
  'RN' :'1',
  'EN' :'0',
  'CN' :'1',
  'ERN':'00',
  'PN' :'01',

  'LS':'0',
  'LT':'0',
  'LI':'10',
  'LF':'11',
  'FS':'1',
  'NS':'0',
  'AS':'00',
  'RS':'01',
  'ET':'1',
  'MS':'1',

  'OP':'0000',
  'OM':'0001',
  'OT':'0010',
  'OD':'0011',
  'OE':'0100',
  'OU':'0101',

  'BN' :'0110',
  'BO' :'01110',
  'BX' :'01111',
  'BA' :'1000',
  'BE' :'1001',
  'BL' :'1010',
  'BLE':'1011',
  'BG' :'1100',
  'BGE':'1101',

  'TN' :'1110',
  'TO' :'1111000',
  'TX' :'1111001',
  'TA' :'1111010',
  'TL' :'1111011',
  'TRA':'1111100',
  'TRL':'1111101',
  'TTL':'1111110',
  'TTR':'1111111',

  'RT':'00',
  'ST':'01',
  'IF':'10',
  'EN':'0',
  'EL':'1',
  'WH':'11'
}

def compile(inp,out):
  opc=""
  while True:
    c=inp.read(1)
    if not c: break

    if c in "01": write_bits(out,c)
    elif c.isspace():
      if not opc: continue
      write_bits(out,opcodes[opc.upper()])
      opc=""
    else: opc += c
  flush_bits(out)

def precompile(input,output,cfunc):
  print "Compiling '%s' to '%s'..." % (input,output)
  with open(input,'r') as i:
    with open(output,'w') as o:
      cfunc(i,o)
  print "Successfully compiled '%s'!" % (input)

def main(cfunc):
  parser = argparse.ArgumentParser(description="Compiles Advanced Assembly code"
    + " into an interpretable  binary format.")
  parser.add_argument('-o','--output',metavar="output",dest="output",
    help="Where the compiled file will be saved. If no value is specified, this"
    + " will be inferred.")
  parser.add_argument('file',help="The file to be compiled.")
  args=parser.parse_args()
  if args.output: precompile(args.file,args.output,cfunc)
  elif "." in args.file:
    precompile(args.file,os.path.splitext(args.file)[0],cfunc)
  else: precompile(args.file,args.file+"_compiled",cfunc)
