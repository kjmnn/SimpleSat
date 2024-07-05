# SimpleSat
A little toy SAT solver, don't use it for anything serious.

It implements the DPLL algorithm at least asymptotically efficiently by keeping track
of every clause's contents as well as occurences of every variable.  
Supports file input in the DIMACS CNF format.

## Running the solver
Inside `cabal repl` run something like `runPrintResults 1 ``onFile`` "input_file.cnf"`
or `runPrintResults 1 ``onDir``  "dir_with_cnf_files"`.

## Tests
For now I've put some input files in folders named `satisfiable_small`, `satisfiable` and `unsatisfiable`,
so you can just run `runTest expectSat ``onDir`` "satisfiable"` and 
`runTest expectUnsat ``onDir`` "unsatisfiable"` (inside `cabal repl` of course.)  
(all inputs sourced from https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html)

## Documentation
You can build the haddocks with `cabal haddock --haddock-internal` or, if you want nice links to imports, 
`cabal haddock --enable-documentation --haddock-internal`.