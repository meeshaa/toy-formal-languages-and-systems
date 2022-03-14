module TFLS.Languages where

import TFLS.Basic
import TFLS.LanguageConstructions

----------------------
-- Some particular languages
----------------------

-- Language from an excercise 1.1 from the Geoffrey Hunter's "Metalogic"
metalogic1dot1 :: Language
metalogic1dot1 s = startsWith '△' s && subAlphabet "△□" s

-- A formal language for propositional logic
prop :: Language
prop = contextFree propGrammar

propGrammar :: CFG
propGrammar = Grammar
  [('F',"P"),('F',"¬F"),('F',"(F→F)"),('P',"p"),('P',"P'")] 'F'

-- Minimal arithmetic
