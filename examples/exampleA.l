import defs.l

print Tracing (\x.K I(x x)) \y.K I(y y) = 
printT four
printLn
traceNF four

printLn Tracing OMEGA (Maximum 10 steps):
traceNFMax 10 OMEGA
printLn Printing Normal form of OMEGA (should not finish):
printNF OMEGA
