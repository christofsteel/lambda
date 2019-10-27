module Lambda.Types
  ( ATerm(L, A, V)
  , Term
  , Variable
  , Prog
  , Command(..)
  , lshow
  , reverseNatTerm
  )
where
import           Lambda.Types.Prog              ( Prog
                                                , Command(..)
                                                )
import           Lambda.Types.Term
