let I = \x.x
let K = \x y.x
let four = (\x.K I(x x)) \y.K I(y y)

print Tracing (\x.K I(x x)) \y.K I(y y) = 
printT four
printLn
traceNF four

let omega = \x.x x
let OMEGA = omega omega
printLn Tracing OMEGA (Maximum 10 steps):
traceNFMax 10 OMEGA
printLn Printing Normal form of OMEGA (should not finish):
printNF OMEGA
