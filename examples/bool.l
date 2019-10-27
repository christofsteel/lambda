let TRUE = \x y.x
let FALSE = \x y.y
let if = \m n o.m n o

addRev TRUE
addRev FALSE

let and = \x y.if x (if y TRUE FALSE) FALSE
let or = \x y.if x TRUE (if y TRUE FALSE)
let not = \x.if x FALSE TRUE

let isZero = \x.x(\y.FALSE) TRUE
