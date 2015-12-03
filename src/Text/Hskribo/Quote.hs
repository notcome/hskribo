{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.Hskribo.Quote where

import Control.Monad
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Blaze.Html5 ((!))

import Text.Hskribo.AST
import Text.Hskribo.Parser

(<!>) = (!)

hskribo :: QuasiQuoter
hskribo = QuasiQuoter
  { quoteExp = \input -> do
      loc <- location
      ast <- case pHskribo (show loc) input of
        Left parseError -> fail parseError
        Right parseRes  -> return parseRes
      toHaskell ast
  , quotePat  = fail "hskribo in pattern not allowed."
  , quoteType = fail "hskribo in type not allowed."
  , quoteDec  = fail "hskribo in dec not allowed." }

nameToValue :: String -> Q Exp
nameToValue name = do
  let name' = parseName name
  valueName <- lookupValueName name'
  case valueName of
    Just x  -> varE x
    Nothing -> errNotValidValueName name
  where
    parseName [] = []
    parseName ('-':x:xs) = parseName (toUpper x : xs)
    parseName (x:xs)     = x : parseName xs

joinSpaced :: String -> String -> String
joinSpaced lhs rhs = lhs ++ " " ++ rhs

joinLined :: String -> String -> String
joinLined lhs rhs = lhs ++ rhs

toHaskell :: SExpr -> ExpQ
toHaskell (SInt   num) = [|num|]
toHaskell (SFloat num) = [|num|]
toHaskell (SStr   str) = [|str|]
toHaskell (SName name)
  | '[':']':name' <- name = nameToValue name'
  | ':'    :name' <- name = nameToValue name'
  | otherwise             = nameToValue name

toHaskell (SText  xs)  = foldl combine zero xs
  where
    zero = [|""|]
    combine lhs rhs
      | SLine next <- rhs = appFXs [|joinLined|]  [lhs, toHaskell next]
      | otherwise         = appFXs [|joinSpaced|] [lhs, toHaskell rhs]

toHaskell (SList (f:xs))
  | SName _ <- f = slistToHaskell f xs
  | otherwise    = errNotValidValueName $ show f

slistToHaskell :: SExpr -> [SExpr] -> ExpQ
slistToHaskell f xs = let
  fValue        = toHaskell f
  (attrs, args) = takeAttrs xs
  fValue'       = combineAttrs fValue attrs
  args'         = map toHaskell args
  in if isFromList f
     then appE fValue' $ listE args'
     else appFXs fValue' args'
  where
    takeAttrs (x1:x2:xs)
      | isAttribute x1 = let
        (attrs, args) = takeAttrs xs
        in ((x1, x2) : attrs, args)
      | otherwise = ([], x1:x2:xs)
    takeAttrs xs  = ([], xs)

    combineAttrs = foldl (\f (a, b) -> let
      a' = toHaskell a
      b' = toHaskell b
      in uInfixE a' [|(<!>)|] b')

isFromList :: SExpr -> Bool
isFromList (SName ('[':']':_)) = True
isFromList _                   = False

isAttribute :: SExpr -> Bool
isAttribute (SName (':':_)) = True
isAttribute _               = False

appFXs :: ExpQ -> [ExpQ] -> ExpQ
appFXs f xs = foldl appE f xs

errNotValidValueName :: String -> Q a
errNotValidValueName x = fail $ x ++ " is not a valid name."
