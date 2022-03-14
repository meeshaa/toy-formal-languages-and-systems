module TFLS.Semantics where

import TFLS.Languages

----------------------
-- Semantics for languages
----------------------
-- Or various things to do with a language
--That is, functions `Language -> ...`

-- example: deductive apparatus on a language, producing a formal system

-- Context-free languages

-- A parser `Language -> ParseTree` for context-free languages.

-- class contextfree 
-- ^^^^^^^^^^^^^^^^^ because grammar gives context-free languages a structure
-- that is important for semantics, and contextFree function forgets it
