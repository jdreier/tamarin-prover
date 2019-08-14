{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternGuards #-}
-- |
-- Copyright   : (c) 2019 Charlie Jacomme and Robert Künnemann
-- License     : GPL v3 (see LICENSE)
--
-- Maintainer  : Robert Künnemann <robert@kunnemann.de>
-- Portability : GHC only
--
-- Translation from Sapic processes to Proverif

module Export (
    prettyProVerifTheory

) where

import         Term.Builtin.Signature
import         Term.SubtermRule

import         Text.StringTemplate
import         Theory
import         Text.PrettyPrint.Highlight



import qualified Data.Set as S
import qualified Data.Label as L

import qualified Data.ByteString.Char8 as BC


template = newSTMP $ unlines [
  "$headers;separator='\n'$",
  "",
  "$reduc;separator='\n'$",
  "",
  "$queries;separator='\n'$",
  "",
  "process",
  "    $process$",
  ""
  ] :: StringTemplate String

-- Proverif Headers need to be ordered, and declared only once. We order them by type, and will update a set of headers.
data ProverifHeader =
  Sym String
  | Fun String
  | Eq String
  -- | Type String -- will  be used to define types
  deriving (Ord, Show, Eq)

-- We declare some base headers. Notably, we need a dedicated attacker channel.
base_headers = S.fromList [
  Sym ("free attacker_channel:channel.")
  ]
  
-- The corresponding headers for each Tamarin builtin. If the functions of the builtin are inside the signature, we add the corresponding headers to the output.
builtins = map (\(x,y) -> (x, S.fromList y)) [
  (hashFunSig, [Fun "fun hash(bitstring):bitstring."] ),
  (signatureFunSig, [
      Fun "fun sign(bitstring,bitstring):bitstring.",
      Fun "fun pk(bitstring):bitstring.",
      Eq "reduc forall m:bitstring,sk:bitstring; verify(sign(m,sk),m,pk(sk)) = true."
      ]
  ),
  (S.fromList [ expSym, oneSym], [
      Sym "const g:bitstring.",
      Fun "fun exp(bitstring,bitstring):bitstring",
      Eq "equation forall a:bitstring,b:bitstring; exp( exp(g,a),b) = exp(exp(g,b),a)."
      ]
  ),
  (asymEncFunSig, [
      Fun "fun senc(bitstring,bitstring):bitstring.",
      Eq "reduc forall m:bitstring,sk:bitstring; sdec(senc(m,sk),sk) = m."]
  ),
  (pairFunSig,  [Eq "reduc forall a:bitstring,b:bitstring; fst((a,b))=a.",
  Eq  "reduc forall a:bitstring,b:bitstring; snd((a,b))=b."]
  )
  ]

  
prettyProVerifTheory :: HighlightDocument d => OpenTheory -> d
prettyProVerifTheory thy = text result
  where result = toString tmp
        tmp = setManyAttrib hd template
        hd = attribHeaders $ S.toList (base_headers `S.union` (loadHeaders thy))


-- Load the proverif headers from the OpenTheory
loadHeaders :: OpenTheory -> S.Set ProverifHeader
loadHeaders thy =
  (S.map  headerOfFunSym funSymsNoBuiltin) `S.union` funSymsBuiltins -- `S.union` (S.map headerOfRule sigRules)
  where sig = (L.get sigpMaudeSig (L.get thySignature thy))
        -- generating headers for function symbols
        sigFunSyms = stFunSyms sig
        funSymsBuiltins = ((foldl (\x (y,z) -> if S.isSubsetOf y sigFunSyms then  x `S.union` z else x  )) S.empty builtins)
        funSymsNoBuiltin = sigFunSyms S.\\ ((foldl (\x (y,z) -> x `S.union` y  )) S.empty builtins)
        headerOfFunSym (f,(k,Public)) = Fun (make_str (f,k) ++ ".")
        headerOfFunSym (f,(k,Private)) = Fun ((make_str (f,k))  ++ " [private].")
        make_str (f,k) = "fun " ++ BC.unpack f ++ "(" ++ (make_args k) ++ "):bitstring"
        make_args 0 = ""
        make_args 1 = "bitstring"
        make_args n = "bitstring,"++(make_args (n-1))
        -- generating headers for equations, TODO, needs term printer for proverif
        -- sigRules = stRules sig        
        -- headerOfRule r = Eq (Text.PrettyPrint.Highlight.render (prettyCtxtStRule r))
        
attribHeaders :: [ProverifHeader] -> [(String, String)]
attribHeaders hd =
  eq ++ fun ++ sym
  where (eq,fun,sym) = splitHeaders hd
        splitHeaders [] = ([],[],[])
        splitHeaders (x:xs)
          | Sym s <- x = (e1,f1,("headers",s):s1)
          | Fun s <- x =  (e1,("headers",s):f1,s1)
          | Eq s <- x =  (("headers", s):e1,f1,s1)
          where (e1,f1,s1) = splitHeaders xs
          
