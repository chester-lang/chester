
val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val app : 'a1 list -> 'a1 list -> 'a1 list

val add : int -> int -> int

val eqb : int -> int -> bool

val eqb0 : bool -> bool -> bool

module Nat :
 sig
 end

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Pos :
 sig
  val succ : positive -> positive

  val of_succ_nat : int -> positive
 end

module N :
 sig
  val of_nat : int -> n
 end

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : positive -> char

val ascii_of_N : n -> char

val ascii_of_nat : int -> char

val eqb1 : char list -> char list -> bool

val append : char list -> char list -> char list

type withUTF16 = { unicode : int; utf16 : int }

type pos = { index : withUTF16; line : int; column : withUTF16 }

type spanInFile = { start_pos : pos; end_pos : pos }

type span = { file_name : char list; range : spanInFile }

type patternCST =
| PatWildcardCST of span
| PatVarCST of char list * span
| PatConstructorCST of char list * char list list * span

type cST =
| Symbol of char list * span
| Tuple of cST list * span
| ListLiteral of cST list * span
| Block of cST list * cST * span
| StringLiteral of char list * span
| IntegerLiteral of char list * span
| BoolLiteral of bool * span
| SeqOf of cST list * span
| CommentCST of char list * span
| LetCST of char list * cST * cST * span
| IfCST of cST * cST * cST * span
| DefCST of char list * char list list * (char list * cST) list * cST * 
   cST * span
| LamCST of char list * cST option * cST * span
| AppCST of cST * cST list * span
| EnumCST of char list * char list list * cST list * span
| MatchCST of cST * (patternCST * cST) list * span
| RecordCST of char list * char list list * cST list * span
| FieldAccessCST of cST * char list * span
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

type patternAST =
| PatWildcard
| PatVar of char list
| PatConstructor of char list * char list list

type aST =
| AstRef of char list
| AstTuple of aST list
| AstStringLit of char list
| AstIntLit of int
| AstBoolLit of bool
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
| AstMatch of aST * (patternAST * aST) list
| AstRecord of char list * char list list * (char list * aST) list
| AstFieldAccess of aST * char list
| AstMeta of metaId
| AstError of char list

type solverState = { type_metas : (metaId -> (aST, aST) metaState);
                     effect_metas : (metaId -> (effectSet, effectSet)
                                    metaState) }

val empty_state : solverState

val update_type_state :
  metaId -> (aST, aST) metaState -> solverState -> solverState

type token =
| TokSymbol of char list * span
| TokString of char list * span
| TokInt of char list * span
| TokComment of char list * span
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

val boolType : aST

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

val zonk : int -> aST -> aST elabM

val unify : int -> aST -> aST -> unit elabM

val elaborate : typeEnv -> cST -> aST option -> (aST * aST) elabM

type typeScriptAST =
| TsNumberLiteral of char list
| TsStringLiteral of char list
| TsBooleanLiteral of bool
| TsIdentifier of char list
| TsPropertyAccess of typeScriptAST * char list
| TsIndexAccess of typeScriptAST * typeScriptAST
| TsCall of typeScriptAST * typeScriptAST list
| TsArrow of char list list * typeScriptAST
| TsBlock of typeScriptAST list * typeScriptAST
| TsArray of typeScriptAST list
| TsAwait of typeScriptAST
| TsLet of char list * typeScriptAST
| TsIf of typeScriptAST * typeScriptAST * typeScriptAST
| TsFunctionDecl of char list * char list list * typeScriptAST
| TsInterface of char list
| TsIIFE of typeScriptAST
| TsThrow of char list
| TsEmpty

val concat_strings : char list -> char list list -> char list

val stringify_ts : typeScriptAST -> char list

type goAST =
| GoIntLiteral of char list
| GoStringLiteral of char list
| GoBoolLiteral of bool
| GoIdentifier of char list
| GoSelector of goAST * char list
| GoIndex of goAST * goAST
| GoCall of goAST * goAST list
| GoFuncLiteral of char list list * goAST
| GoBlock of goAST list * goAST
| GoArray of goAST list
| GoLet of char list * goAST
| GoIf of goAST * goAST * goAST
| GoFuncDecl of char list * char list list * goAST
| GoStruct of char list
| GoTypeAssert of goAST * char list
| GoPanic of char list
| GoEmpty

val concat_strings0 : char list -> char list list -> char list

val stringify_go : goAST -> char list

val nat_to_string : int -> char list

val emit_ts : aST -> typeScriptAST

val emit_go : aST -> goAST

val gen_spaces : int -> char list

val join_strings : char list -> char list list -> char list

val format_cst : int -> int -> cST -> char list

val ts_to_chester : typeScriptAST -> aST

val go_to_chester : goAST -> aST
