module Juvix.Backends.Michelson.Emit (
  emit,
  emitUT,
  emitType,
  emitFinal
) where

import qualified Data.Text                      as T
import           Data.Typeable
import           Foundation                     hiding (Either (..))
import qualified Prelude                        as P

import           Juvix.Backends.Michelson.Types

emit ∷ ∀ a b . (Typeable a, Typeable b) ⇒ Expr a b → T.Text
emit expr =
  case expr of

    Drop     → "DROP"
    Dup      → "DUP"
    Swap     → "SWAP"
    Const (v ∷ t) ->
      case eqT ∷ Maybe (t :~: ()) of
        Just Refl → "UNIT"
        _ ->
          case eqT ∷ Maybe (t :~: Integer) of
            Just Refl → T.concat ["PUSH int ", T.pack (P.show v)]
            _         → "e"

    ConsPair → "PAIR"
    Car      → "CAR"
    Cdr      → "CDR"

    Left        → "LEFT"
    Right       → "RIGHT"
    IfLeft a b  → T.concat ["IF_LEFT {", emit a, "} {", emit b, "}"]



    Seq a b  → T.concat ["{", emit a, "; ", emit b, "}"]
    If a b   → T.concat ["IF {", emit a, "} {", emit b, "}"]
    Dip a    → T.concat ["DIP {", emit a, "}"]
    Nop      → "NOP"

    _        → "e"

emitUT ∷ ExprUT → T.Text
emitUT expr =
  case expr of

    DropUT       → "DROP"
    DupUT        → "DUP"
    SwapUT       → "SWAP"
    ConstUT UnitUT        → "UNIT"
    ConstUT (BoolUT b)    → T.concat ["PUSH bool ", if b then "True" else "False"]
    ConstUT (IntegerUT i) → T.concat ["PUSH int ", T.pack (P.show i)]
    ConstUT (TezUT i)     → T.concat ["PUSH tez \"", T.pack (P.show i), "\""]
    ConstUT (StringUT s)  → T.concat ["\"", s, "\""]

    ConsPairUT   → "PAIR"
    CarUT        → "CAR"
    CdrUT        → "CDR"

    ConsSomeUT   → "SOME"
    ConsNoneUT   → "NONE"
    IfNoneUT a b → T.concat ["IF_NONE {", emitUT a, "} {", emitUT b, "}"]

    LeftUT       → "LEFT"
    RightUT      → "RIGHT"
    IfLeftUT a b → T.concat ["IF_LEFT {", emitUT a, "} {", emitUT b, "}"]

    ConsListUT   → "CONS"
    NilUT        → "NIL"
    IfConsUT a b → T.concat ["IF_CONS {", emitUT a, "} {", emitUT b, "}"]
    ListMapUT    → "MAP"
    ListReduceUT → "REDUCE"

    EmptySetUT   → "EMPTY_SET"
    SetMapUT     → "MAP"
    SetReduceUT  → "REDUCE"
    SetMemUT     → "MEM"
    SetUpdateUT  → "UPDATE"
    SetSizeUT    → "SIZE"

    EmptyMapUT   → "EMPTY_MAP"
    MapMapUT     → "MAP"
    MapReduceUT  → "REDUCE"
    MapMemUT     → "MEM"
    MapGetUT     → "GET"
    MapUpdateUT  → "UPDATE"
    MapSizeUT    → "SIZE"

    ConcatUT     → "CONCAT"

    AddSecondsToTimestampUT → "ADD"
    AddTimestampToSecondsUT → "ADD"

    AddTezUT     → "ADD"
    SubTezUT     → "SUB"
    MulTezNatUT  → "MUL"
    MulNatTezUT  → "MUL"
    EdivTezNatUT → "DIV"
    EdivTezUT    → "DIV"

    OrUT         → "OR"
    AndUT        → "AND"
    XorUT        → "XOR"
    NotUT        → "NOT"

    NegNatUT     → "NEG"
    NegIntUT     → "NEG"
    AbsNatUT     → "ABS"
    AbsIntUT     → "ABS"
    IntNatUT     → "CAST"
    AddIntIntUT  → "ADD"
    AddIntNatUT  → "ADD"
    AddNatIntUT  → "ADD"
    AddNatNatUT  → "ADD"
    SubIntUT     → "SUB"
    MulIntIntUT  → "MUL"
    MulIntNatUT  → "MUL"
    MulNatIntUT  → "MUL"
    MulNatNatUT  → "MUL"
    EdivIntIntUT → "DIV"
    EdivIntNatUT → "DIV"
    EdivNatIntUT → "DIV"
    EdivNatNatUT → "DIV"
    LslNatUT     → "LSL"
    LsrNatUT     → "LSR"
    OrNatUT      → "OR"
    AndNatUT     → "ADD"
    XorNatUT     → "XOR"
    NotNatUT     → "NOT"
    NotIntUT     → "NOT"

    SeqUT a b    → T.concat ["{", emitUT a, "; ", emitUT b, "}"]
    IfUT a b     → T.concat ["IF {", emitUT a, "} {", emitUT b, "}"]
    LoopUT a     → T.concat ["LOOP {", emitUT a, "}"]
    DipUT a      → T.concat ["DIP {", emitUT a, "}"]
    ExecUT       → "EXEC"
    LambdaUT a   → T.concat ["LAMBDA {", emitUT a, "}"]
    FailUT       → "FAIL"
    NopUT        → "NOP"

    CompareUT    → "COMPARE"
    EqUT         → "EQ"
    NeqUT        → "NEQ"
    LtUT         → "LT"
    GtUT         → "GT"
    LeUT         → "LE"
    GeUT         → "GE"

    {-
    CmpLtUT      → "CMPLT"
    CmpLeUT      → "CMPLE"
    CmpEqUT      → "CMPEQ"
    CmpGeUT      → "CMPGE"
    CmpGtUT      → "CMPGT"
    -}

    ManagerUT        → "MANAGER"
    TransferTokensUT → "TRANSFER_TOKENS"
    CreateAccountUT  → "CREATE_ACCOUNT"
    DefaultAccountUT → "DEFAULT_ACCOUNT"
    CreateContractUT → "CREATE_CONTRACT"
    NowUT            → "NOW"
    BalanceUT        → "BALANCE"
    CheckSignatureUT → "CHECK_SIGNATURE"
    HUT              → "H"
    HashKeyUT        → "HASH"
    StepsToQuotaUT   → "STEPS_TO_QUOTA"
    SourceUT         → "SOURCE"
    AmountUT         → "AMOUNT"

    AnnUT e t        → T.concat ["(", emitUT e, " ∷ ", emitType t, ")"]

emitFinal ∷ ∀ a b . (Typeable a, Typeable b) ⇒ Expr a b → T.Text
emitFinal expr =
  let code =
        case expr of
          Nop → ""
          _   → emit expr in
  T.concat ["{", code, "}"]

emitType ∷ Type → T.Text
emitType UnitT         = "unit"
emitType KeyT          = "key"
emitType HashT         = "hash"
emitType IntT          = "int"
emitType TezT          = "tez"
emitType BoolT         = "bool"
emitType StringT       = "string"
emitType (EitherT a b) = T.concat ["(or ", emitType a, " ", emitType b, ")"]
emitType (PairT a b)   = T.concat ["(pair ", emitType a, " ", emitType b, ")"]
emitType (OptionT a)   = T.concat ["(option ", emitType a, ")"]
emitType (ListT a)     = T.concat ["(list ", emitType a, ")"]
emitType (LamT _ _)    = ""
