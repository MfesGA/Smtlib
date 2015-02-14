module Smtlib.Syntax.ShowSL where

import Data.List 
import Smtlib.Syntax.Syntax


joinA ::(ShowSL a) => [a] -> String 
joinA = (intercalate " ").(fmap showSL)

class ShowSL a where
  showSL :: a -> String

instance ShowSL Command where
  showSL (SetLogic s) = "(set-logic " ++ s ++ ")"
  showSL (SetOption opt) = "(set-option " ++ showSL opt ++ ")"
  showSL (SetInfo info) = "(set-info " ++ showSL info ++ ")"
  showSL (DeclareSort str val) =  "(declare-sort " ++ str ++ 
    " " ++ show val ++ ")"
  showSL (DefineSort str strs sort) = "(define-sort " ++ str ++ 
    " ( " ++ (joinA strs) ++ " ) " ++ showSL sort ++ ") "
  showSL (DeclareFun  str sorts sort) = "(declare-sort  " ++ str ++ 
    " ( "  ++ joinA sorts ++ " )  " ++ showSL sort ++ ") "
  showSL (DefineFun str srvs sort term) = "( define-sort"   ++ str ++ 
    " (  " ++ joinA srvs ++ " ) " ++ showSL sort ++ " " ++ showSL term ++ " )"
  showSL (Push n) = "(push " ++ show n ++ " )"
  showSL (Pop n) = "(pop " ++show n ++ " )"
  showSL (Assert term) = "(asert " ++ showSL term ++ ") "
  showSL CheckSat = "(check-sat)" 
  showSL GetAssertions = "(get-assertions)"
  showSL GetProof = "(get-proof)"
  showSL GetUnsatCore = "(get-unsat-core)"
  showSL (GetValue terms) = "( (" ++ joinA terms ++ ") )"
  showSL GetAssignment =  "(get-assignment)"
  showSL (GetOption opt) = "(get-option " ++ opt ++ ")"
  showSL (GetInfo info) = "(get-info " ++ showSL info ++ " )"
  showSL Exit = "(exit)"


instance ShowSL Option where
  showSL (PrintSucess b) = ":print-sucess " ++ show b
  showSL (ExpandDefinitions b) = ":expand-definitions " ++ show b
  showSL (InteractiveMode b) = ":interactive-mode " ++ show b
  showSL (ProduceProofs b) = ":produce-proofs " ++ show b
  showSL (ProduceUnsatCores b) = ":produce-unsat-cores " ++  show b
  showSL (ProduceModels b) = ":produce-models " ++ show b
  showSL (ProduceAssignments b) = ":produce-assignments " ++ show b
  showSL (RegularOuputChannel s) = ":regular-output-channel " ++ s
  showSL (DiagnosticOutputChannel s) = ":diagnostic-output-channel " ++ s
  showSL (RandomSeed n) = ":random-seed " ++ n
  showSL (Verbosity n) = ":verbosity'" ++ n
  showSL (OptionAttr attr) = showSL attr