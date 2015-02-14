module Smtlib.Syntax.ShowSL where

import           Data.List
import           Smtlib.Syntax.Syntax


joinA ::(ShowSL a) => [a] -> String
joinA = unwords.fmap showSL

joinNs :: [Int] -> String
joinNs = unwords.fmap show

class ShowSL a where
  showSL :: a -> String

instance ShowSL Command where
  showSL (SetLogic s) = "(set-logic " ++ s ++ ")"
  showSL (SetOption opt) = "(set-option " ++ showSL opt ++ ")"
  showSL (SetInfo info) = "(set-info " ++ showSL info ++ ")"
  showSL (DeclareSort str val) =  "(declare-sort " ++ str ++
    " " ++ show val ++ ")"
  showSL (DefineSort str strs sort) = "(define-sort " ++ str ++
    " ( " ++ joinA strs ++ " ) " ++ showSL sort ++ ") "
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

instance ShowSL InfoFlags where
  showSL ErrorBehavior = ":error-behavior"
  showSL Name = ":name"
  showSL Authors = ":authors"
  showSL Version = ":version"
  showSL Status = ":status"
  showSL ReasonUnknown = ":reason-unknown"
  showSL AllStatistics = ":all-statistics"
  showSL (InfoFlags s) = s


instance ShowSL Term where
  showSL (TermSpecConstant sc) = showSL sc
  showSL (TermQualIdeintifier qi) = showSL qi
  showSL (TermQualIdeintifierT qi term) =
    "( " ++ showSL qi ++ " " ++ showSL term ++ " )"
  showSL (TermLet vb term) =
    "( let ( " ++ joinA vb ++ " ) " ++ showSL term ++ " )"
  showSL (TermForall svs term) =
    "(forall ( " ++ joinA svs ++ " ) " ++ showSL term ++ " )"
  showSL (TermExists svs term) =
    "(exists ( " ++ joinA svs ++ " ) " ++ showSL term ++ " )"
  showSL (TermAnnot term atts) =
    "( ! " ++ showSL term ++ " " ++ joinA atts ++ " )"

instance ShowSL VarBinding where
  showSL (VB str term) = "( "++ show str ++ " " ++ showSL term ++ " )"

instance ShowSL SortedVar where
  showSL (SV str sort)  = "( " ++ show str ++ " " ++ showSL sort ++ " )"

instance ShowSL QualIdentifier where
  showSL (QIdentifier iden ) = showSL iden
  showSL (QIdentifierAs iden sort) =
    " ( as " ++ showSl iden ++ " " ++ showSL sort ++ " )"

instance ShowSL AttrValue where
  showSL (AttrValueConstant spc) = showSL spc
  showSL (AttrValueSymbol str) = str
  showSL (AttrValueSexpr sexprs) = "( " ++ joinA sexprs  ++ " )"

instance ShowSL Attribute where
  showSL (Attribute str ) = str
  showSL (AttributeVal str attrVal ) = str ++ " " ++ showSL attrVal

instance ShowSL Identifier where
  showSL (ISymbol str) = str
  showSL (I_Symbol str ns ) = "( _ " ++ str ++ " " ++ joinNs ns  ++ " )"

instance ShowSL Sort where
  showSL (SortId iden) = showSL id
  showSL (SortIdentifiers iden sorts) =
    " ( " ++ show iden ++ " " ++ joinA sorts ++ " )"

instance ShowSL SpecConstant where
  showSL (SpecConstantNumeral n) = show n
  showSL (SpecConstantDecimal str) = str
  showSL (SpecConstantHexadecimal str) = str
  showSL (SpecConstantBinary str)  = str
  showSL (SpecConstantString str) = str

instance ShowSL Sexpr where
  showSL (SexprSpecConstant sc) = showSL sc
  showSL (SexprSymbol str) = str
  showSL (SexprKeyword str) = str
  showSL (SexprSxp srps) = "( " ++ joinA srps ++ " )"
