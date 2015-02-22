module Testing where

import System.Directory
import Smtlib.Parsers.CommandsParsers
import Smtlib.Syntax.Syntax
import Text.ParserCombinators.Parsec
import Data.List
import Data.Functor


isSMTLib2File :: String -> Bool
isSMTLib2File = isInfixOf ".smt2"

containsError :: Either ParseError Source -> Bool
containsError (Left _) = True
containsError (Right _) = False


filterFiles = filter isSMTLib2File

filterResults = filter containsError


parseFile :: FilePath -> IO (Either ParseError Source)
parseFile x = parse parseSource "debug.txt" <$> readFile x



getDirectoryContentsPath x = do 
	cnts <- getDirectoryContents x
	return (fmap ((x++"\\")++) cnts)

(<#>) :: Monad m => m ([a]) -> (a -> m b) -> m [b]
(<#>) mlst f = do 
	lst <- mlst
	mapM f lst


(<##>) :: Monad m => m([a]) -> ([a] -> [a])  -> m([a])
(<##>) rlst f = do
	lst <- rlst
	return (f  lst)


main = files <#> parseFile <##> filterResults
	where files = (filterFiles <$> (getLine >>= getDirectoryContentsPath))