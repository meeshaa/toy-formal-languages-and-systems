module TFLS.Basic where

----------------------
-- The language datatype
----------------------

{- Any type can be treated as an alphabet. A formal language on a type `A` is a
type of well-formed formulas over `A`, in other words, a subtype of a free
monoid over `A`. A free monoid over a type `A` is precisely `List A`. A subtype
can be specified as a function to the two-term type such as `Bool` (That is, an
automata). We will be conserned only with languages on `Char`. -}

type Language = String -> Bool
