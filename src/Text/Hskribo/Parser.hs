{-# LANGUAGE FlexibleContexts #-}

module Text.Hskribo.Parser where

import           Control.Monad         (void)
import qualified Text.Megaparsec       as MP
import qualified Text.Megaparsec.Lexer as ML
import           Text.Megaparsec       ((<|>))
import           Text.Megaparsec.Prim  (MonadParsec(..))

import Text.Hskribo.AST

import Debug.Trace

pHskribo :: String -> String -> Either String SExpr
pHskribo name input = let
  parser = aSkipSpace *> alex pList
  in case MP.parse parser name input of
    Left  err -> Left $ show err
    Right ast -> Right ast

pExpr :: MonadParsec s m Char => m SExpr
pExpr = alex $ pList <|> pText <|> pAtom

pList :: MonadParsec s m Char => m SExpr
pList = fmap SList $ asym "(" *> MP.some pExpr <* asym ")"

pAtom :: MonadParsec s m Char => m SExpr
pAtom =   fmap SInt   ML.integer
      <|> fmap SFloat ML.float
      <|> fmap SStr   pString
      <|> fmap SName  pName
  where
    pString = MP.char '"' *> MP.manyTill ML.charLiteral (MP.char '"')
    pName   = MP.some (MP.noneOf "()[]{}<># \t\n\r")

aSkipSpace :: MonadParsec s m Char => m ()
aSkipSpace = ML.space aSpace
                      (ML.skipLineComment  "##")
                      (ML.skipBlockComment "#=" "=#")
  where
    aSpace    = void MP.eol <|> void (MP.tab <|> MP.char ' ')

alex :: MonadParsec s m Char => m a -> m a
alex = ML.lexeme aSkipSpace

asym :: MonadParsec s m Char => String -> m String
asym = ML.symbol aSkipSpace

pText :: MonadParsec s m Char => m SExpr
-- NOTE: ML.lexeme spc p = p <* spc
pText = fmap SText $ (pText_ '[' ']') <|> (pText_ '{' '}') <|> (pText_ '<' '>')
  where
    pText_ b e  = bsym [b] *> MP.many (pTextFrag e) <* asym [e]

    pTextFrag e = blex $ pNewLine e <|> pScheme <|> pTextLit e
    pNewLine  e = do
      _    <- MP.some $ blex MP.eol
      next <- MP.optional $ pTextFrag e
      case next of
        Just x  -> return $ SLine x
        Nothing -> return $ SLine $ SStr ""
    pScheme     = MP.try (MP.char ',') *> pList
    pTextLit  e = SStr <$> MP.some char where
      char = MP.notFollowedBy (MP.oneOf $ e:" \t\n\r") *> ML.charLiteral

bSkipSpace :: MonadParsec s m Char => m ()
bSkipSpace = do _ <- MP.many $ MP.tab <|> MP.char ' '; return ()

blex :: MonadParsec s m Char => m a -> m a
blex = ML.lexeme bSkipSpace

bsym :: MonadParsec s m Char => String -> m String
bsym = ML.symbol bSkipSpace
