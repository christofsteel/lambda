let I = \x.x
let K = \x y.x
let four = (\x.K I(x x)) \y.K I(y y)

let omega = \x.x x
let OMEGA = omega omega
