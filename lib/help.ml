let usage =
  {|Usage: calyxium [command] [options] <files>

Commands:
  format <file|dir>     Format a file or all files in a directory.
  check <file>          Typecheck the input without compiling or running.
  new <project>         Create a new project scaffold with basic layout.
  repl                  Launch an interactive REPL for experimenting with Calyxium code.

Options:
  --help                Display this information.
  --version             Show the interpreter version.
  --emit-bytecode       Compile and emit the bytecode of the input file.
                        You can optionally specify the output file or path.
  --run-bytecode        Execute a bytecode file with the `.cxc` extension.
  --no-run              Compile without executing the output.
|}
