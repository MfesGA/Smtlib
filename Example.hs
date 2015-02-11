module SmtLib.Example where

import SmtLib.Parsers.CommandsParsers
import           Text.ParserCombinators.Parsec as Pc
import SmtLib.Syntax.Syntax

parseSmtLibFile :: IO ()
parseSmtLibFile =  getLine >>= readFile >>= pt.parse parseSource ""


pt :: Either ParseError Source -> IO ()
pt (Left err) = print err
pt (Right x) = pt' x

pt' :: Source -> IO()
pt' = foldr (\ x -> (>>) (print x >> putStr "\n")) (return ())
