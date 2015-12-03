module Text.Hskribo.AST where

data SExpr = SInt   Integer
           | SFloat Double
           | SStr   String  -- string     "foo bar"
           | SName  String  -- identifier foo-bar
           | SLine  SExpr
           | SText  [SExpr] -- text       [foo bar]
           | SList  [SExpr]
           deriving (Eq, Show)
