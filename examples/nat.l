import bool.l

let plus = \m n f x.m f(n f x)
let succ = \n f x.f (n f x)
let mult = \m n f x.m (n f) x
let exp = \m n.n m
let pred = \n f x.n(\g h.h (g f)) (\u.x) (\u.u)
let minus = \m n.(n pred) m

let le = \x y.isZero (minus x y)
let ge = \x y.isZero (minus y x)
let eq = \x y. and (le x y) (ge x y)
let lt = \x y.isZero (minus x (pred y))
