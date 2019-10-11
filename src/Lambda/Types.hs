module Lambda.Types (
    Term (L, A, V),
    Variable,
    Prog,
    Command (..),
    explShow',
    minShow'
    ) where
import Lambda.Types.Prog(Prog, Command (..))
import Lambda.Types.Term
