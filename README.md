# HsmtlibParser
Parser for smt-lib Command responses

A library with SMTLib2 syntax and parsers to parsee SMTLib2 commands and their responses.

How to parse a smtlib2 file:
```Haskell
import Smtlib.Parsers.CommandsParsers
import Smtlib.Syntax.Syntax
import Text.ParserCombinators.Parsec

parseFile :: FilePath -> IO (Either ParseError Source)
parseFile x = parse parseSource "" <$> readFile x
```
