module Language.Haskell.Pointfree (
    pointfree
) where

import Language.Haskell.Pointfree.Common (mapTopLevel,mapTopLevel')
import Language.Haskell.Pointfree.Optimize (optimize)
import Language.Haskell.Pointfree.Parser (parsePF)
import Language.Haskell.Pointfree.Transform (transform)
import Language.Haskell.Pointfree.PrettyPrinter

import Language.Haskell.Exts

import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&),first,second)
import Data.Maybe (mapMaybe)
import Data.String.Utils (replace)

type ImportQ = (String,Maybe String)

data PModule = PModule {
    pmName :: String,
    pmImports :: [ImportQ],
    pmDecls :: [Decl],
    pmIdents :: [(String,Decl)]
} deriving (Eq,Show)

loadModule :: FilePath -> IO PModule
loadModule src = do
    result <- parseFile src
    case result of
        err@ParseFailed{} -> fail $ show err
        ParseOk m -> return $ fromModule m
    
fromModule :: Module -> PModule
fromModule (Module loc name pragmas mWarn mExports imports decls) =
    PModule {
        pmImports = imports'',
        pmName = nameOf name,
        pmDecls = decls,
        pmIdents = idents
    } where
        nameOf (ModuleName n) = n
        imports' = map f imports :: [ImportQ] where
            f = nameOf . importModule &&& (Just nameOf <*>) . importAs
        imports'' = if elem "Prelude" $ map fst imports'
            then imports'
            else ("Prelude",Nothing) : imports'
        
        idents = mapMaybe rhs decls
        rhs bind@(PatBind _ (PVar (Ident ident)) _ _ _) = Just (ident,bind)
        rhs bind@(FunBind matches) = head $ map f matches where
            f (Match _ (Ident ident) _ _ _ _) = Just (ident,bind)
        rhs _ = Nothing

-- | Convert the expression into a simplified form understood by parsePF.
-- | Namely: convert patterns and guards to cases and wheres to lets.
convert :: Exp -> Exp
convert (Let (BDecls decls) expr) = Let (BDecls $ concatMap f decls) expr
    where
        f :: Decl -> [Decl]
        --f (FunBind matches) = FunBind $ map g matches
        f bind = [bind]
        
        g :: Match -> (Match,[Decl])
        g (Match loc name pats mType rhs (BDecls iDecls)) =
            (Match loc name pats mType rhs (BDecls []), iDecls)
        g m = (m,[])
convert expr = expr

makeOneLiner :: String -> String
makeOneLiner = replace "\n    " ";"

ok (ParseOk x) = x
r (Right x) = x

pointfree :: Exp -> Either String Exp
pointfree expr = 
    case parsePF $ prettyPrint expr of
        Right d ->
            case parse pf of
                err@ParseFailed{} -> Left $ show err
                ParseOk ast -> Right ast
            where
                pf = show $ last $ mapTopLevel' optimize
                    $ mapTopLevel transform d
        Left err -> Left err
