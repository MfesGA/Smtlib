module Example where

import Smtlib.Parsers.CommandsParsers
import Smtlib.Syntax.Syntax
import Smtlib.Syntax.ShowSL
import Text.ParserCombinators.Parsec

parseSmtLibFile :: IO ()
parseSmtLibFile = getLine >>= readFile >>= pt.parse parseSource ""


parseT = parse parseSetOption ""



pt :: Either ParseError Source -> IO ()
pt (Left err) = print err
pt (Right x)  = pt' x

pt' :: Source -> IO ()
pt' = foldr (\ x -> (>>) ((print.showSL) x)) (return ())
