import argparse
import os

def compile(inp,out):
  pass

def precompile(input,output,cfunc):
  print "Compiling '%s' to '%s'..." % (input,output)
  with open(input,'r') as i:
    with open(output,'w') as o:
      cfunc(i,o)
  print "Successfully compiled '%s'!" % (input)

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Compiles Advanced Assembly code"
    + " into an interpretable  binary format.")
  parser.add_argument('-o','--output',metavar="output",dest="output",
    help="Where the compiled file will be saved. If no value is specified, this"
    + " will be inferred.")
  parser.add_argument('file',help="The file to be compiled.")
  args=parser.parse_args()
  f = compile
  if args.output: precompile(args.file,args.output,f)
  elif "." in args.file: precompile(args.file,os.path.splitext(args.file)[0],f)
  else: precompile(args.file,args.file+"_compiled",f)
