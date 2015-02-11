module Example where

import Smtlib.Parsers.CommandsParsers
import Smtlib.Syntax.Syntax
import Text.ParserCombinators.Parsec

parseSmtLibFile :: IO ()
parseSmtLibFile = getLine >>= readFile >>= pt.parse parseSource ""

pt :: Either ParseError Source -> IO ()
pt (Left err) = print err
pt (Right x)  = pt' x

pt' :: Source -> IO ()
pt' = foldr (\ x -> (>>) (print x >> putStr "\n")) (return ())
