
val add : int -> int -> int

val eqb : int -> int -> bool

module Nat :
 sig
 end

val eqb0 : char list -> char list -> bool

val append : char list -> char list -> char list

type withUTF16 = { unicode : int; utf16 : int }

type pos = { index : withUTF16; line : int; column : withUTF16 }

type spanInFile = { start_pos : pos; end_pos : pos }

type span = { file_name : char list; range : spanInFile }

type cST =
| Symbol of char list * span
| Tuple of cST list * span
| ListLiteral of cST list * span
| Block of cST list * cST list * span
| StringLiteral of char list * span
| IntegerLiteral of char list * span
| SeqOf of cST list * span
| LetCST of char list * cST * cST * span
| IfCST of cST * cST * cST * span
| DefCST of char list * char list list * cST list * cST * cST * span
| EnumCST of char list * char list list * cST list * span
| RecordCST of char list * char list list * cST list * span
| Error of char list * span

val zero_utf16 : withUTF16

val zero_pos : pos

val empty_span : span

val combine_span : span -> span -> span

type metaId = int

type ('a, 'partial) metaState =
| Unsolved
| Constrained of 'partial
| Solved of 'a

type effectRef =
| BuiltinEffect of char list
| UserEffect of char list

type effectSet = effectRef list

type aST =
| AstRef of char list
| AstTuple of aST list
| AstStringLit of char list
| AstIntLit of int
| AstBlock of aST list * aST
| AstApp of aST * aST list
| AstLam of char list * aST * aST
| AstPi of char list * aST * aST * effectSet
| AstDo of aST * aST list
| AstHandle of aST * effectRef * (char list * aST) list
| AstLet of char list * aST * aST
| AstIf of aST * aST * aST
| AstDef of char list * char list list * (char list * aST) list * aST * aST
| AstEnum of char list * char list list * (char list * aST list) list
| AstRecord of char list * char list list * (char list * aST) list
| AstMeta of metaId
| AstError of char list

type solverState = metaId -> (effectSet, effectSet) metaState

val empty_state : solverState

val update_state :
  metaId -> (effectSet, effectSet) metaState -> solverState -> solverState

val add_effect_constraint : metaId -> effectRef -> solverState -> solverState

type token =
| TokSymbol of char list * span
| TokString of char list * span
| TokInt of char list * span
| TokLParen of span
| TokRParen of span
| TokLBrace of span
| TokRBrace of span
| TokLBracket of span
| TokRBracket of span

val token_span : token -> span

val is_rparen : token -> bool

val is_rbracket : token -> bool

val is_rbrace : token -> bool

type 'a parseResult =
| ParseOk of 'a * token list
| ParseErr of char list

val parse_cst : int -> token list -> cST parseResult

val parse_sequence :
  int -> token list -> (token -> bool) -> (cST list * span) parseResult

val tokenize : int -> char list -> pos -> token list

type typeEnv = (char list * aST) list

val lookup_type : char list -> typeEnv -> aST option

type 'a tyResult =
| TyOk of 'a
| TyErr of char list

val eq_ast : aST -> aST -> bool

val typeUniverse : aST

val intType : aST

val stringType : aST

val infer_check : typeEnv -> aST -> aST option -> aST tyResult

type elabState = { next_meta : int; solver_st : solverState }

val init_elab_state : elabState

type 'a elabResult =
| ElabOk of 'a * elabState
| ElabErr of char list * elabState

type 'a elabM = elabState -> 'a elabResult

val ret : 'a1 -> 'a1 elabM

val bind : 'a1 elabM -> ('a1 -> 'a2 elabM) -> 'a2 elabM

val throw : char list -> 'a1 elabM

val fresh_meta : aST elabM

val get_solver : solverState elabM

val put_solver : solverState -> unit elabM

val constrain_effect : metaId -> effectRef -> unit elabM

val unify : aST -> aST -> unit elabM

val elaborate : typeEnv -> cST -> aST option -> (aST * aST) elabM

type typeScriptAST =
| TsNumberLiteral of char list
| TsStringLiteral of char list
| TsBooleanLiteral of bool
| TsIdentifier of char list
| TsPropertyAccess of typeScriptAST * char list
| TsCall of typeScriptAST * typeScriptAST list
| TsArrow of char list list * typeScriptAST
| TsBlock of typeScriptAST list * typeScriptAST
| TsArray of typeScriptAST list
| TsAwait of typeScriptAST
| TsRaw of char list

val concat_strings : char list -> char list list -> char list

val stringify_ts : typeScriptAST -> char list

type goAST =
| GoIntLiteral of char list
| GoStringLiteral of char list
| GoBoolLiteral of bool
| GoIdentifier of char list
| GoSelector of goAST * char list
| GoCall of goAST * goAST list
| GoFuncLiteral of char list list * goAST
| GoBlock of goAST list * goAST
| GoArray of goAST list
| GoRaw of char list

val concat_strings0 : char list -> char list list -> char list

val stringify_go : goAST -> char list

val nat_to_string : int -> char list

val emit_ts : aST -> typeScriptAST

val emit_go : aST -> goAST

val ts_to_chester : typeScriptAST -> aST

val go_to_chester : goAST -> aST
