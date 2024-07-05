# SimpleSat
A little toy SAT solver, don't use it for anything serious.

It implements the DPLL algorithm at least asymptotically efficiently by keeping track
of every clause's contents as well as occurences of every variable.  
Supports file input in the DIMACS CNF format.

## Running the solver
Inside `cabal repl` run something like `runPrintResults 1 ``onFile`` "input_file.cnf"`
or `runPrintResults 1 ``onDir``  "dir_with_cnf_files"`.

## Tests
No tests included for now, during development I used SAT inputs from https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html

## Documentation
You can build the haddocks with `cabal haddock --haddock-internal` or, if you want nice links to imports, 
`cabal haddock --enable-documentation --haddock-internal`.
