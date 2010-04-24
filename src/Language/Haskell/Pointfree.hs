module Language.Haskell.Pointfree (
    pointfree, Expr(..), TopLevel(..)
) where

import Language.Haskell.Pointfree.Common (
        mapTopLevel,mapTopLevel',
        Expr(..),TopLevel(..)
    )
import Language.Haskell.Pointfree.Optimize (optimize)
import Language.Haskell.Pointfree.Parser (parsePF)
import Language.Haskell.Pointfree.Transform (transform)

pointfree :: String -> Either String TopLevel
pointfree expr = case parsePF expr of
    Right d -> Right $ last $ mapTopLevel' optimize $ mapTopLevel transform d
    Left err -> Left err
