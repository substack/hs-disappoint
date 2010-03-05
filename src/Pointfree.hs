module Language.Haskell.Pointfree where

import Language.Haskell.Exts
import Language.Pointfree.Parser

import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&))

transform :: Exp -> Exp
transform expr = undefined

type ImportQ = (String,Maybe String)

data PModule = PModule {
    pmName :: String,
    pmImports :: [ImportQ],
    pmDecls :: [Decl]
} deriving (Eq,Show)

loadModule :: FilePath -> IO PModule
loadModule src = do
    result <- parseFile src
    case result of
        err@ParseFailed{} -> fail $ show err
        ParseOk m -> return $ fromModule m
    
fromModule :: Module -> PModule
fromModule (Module loc name pragmas mWarn mExports imports decls) =
    PModule { pmImports = imports'', pmName = nameOf name, pmDecls = decls }
        where
            nameOf (ModuleName n) = n
            imports' = map f imports :: [ImportQ] where
                f = nameOf . importModule &&& (Just nameOf <*>) . importAs
            imports'' = if elem "Prelude" $ map fst imports'
                then imports'
                else ("Prelude",Nothing) : imports'