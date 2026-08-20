
(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

(** val add : int -> int -> int **)

let rec add = (+)

(** val eqb : int -> int -> bool **)

let rec eqb n0 m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> true)
      (fun _ -> false)
      m)
    (fun n' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> false)
      (fun m' -> eqb n' m')
      m)
    n0

(** val eqb0 : bool -> bool -> bool **)

let eqb0 b1 b2 =
  if b1 then b2 else if b2 then false else true

module Nat =
 struct
 end

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val of_succ_nat : int -> positive **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> XH)
      (fun x -> succ (of_succ_nat x))
      n0
 end

module N =
 struct
  (** val of_nat : int -> n **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> N0)
      (fun n' -> Npos (Pos.of_succ_nat n'))
      n0
 end

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: l0 -> (f a) :: (map f l0)

(** val zero : char **)

let zero = '\000'

(** val one : char **)

let one = '\001'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)

(** val ascii_of_pos : positive -> char **)

let ascii_of_pos =
  let rec loop n0 p =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> zero)
      (fun n' ->
      match p with
      | XI p' -> shift true (loop n' p')
      | XO p' -> shift false (loop n' p')
      | XH -> one)
      n0
  in loop (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
       (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
       0))))))))

(** val ascii_of_N : n -> char **)

let ascii_of_N = function
| N0 -> zero
| Npos p -> ascii_of_pos p

(** val ascii_of_nat : int -> char **)

let ascii_of_nat a =
  ascii_of_N (N.of_nat a)

(** val eqb1 : char list -> char list -> bool **)

let rec eqb1 s1 s2 =
  match s1 with
  | [] -> (match s2 with
           | [] -> true
           | _::_ -> false)
  | c1::s1' ->
    (match s2 with
     | [] -> false
     | c2::s2' -> if (=) c1 c2 then eqb1 s1' s2' else false)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

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

(** val zero_utf16 : withUTF16 **)

let zero_utf16 =
  { unicode = 0; utf16 = 0 }

(** val zero_pos : pos **)

let zero_pos =
  { index = zero_utf16; line = 0; column = zero_utf16 }

(** val empty_span : span **)

let empty_span =
  { file_name = []; range = { start_pos = zero_pos; end_pos = zero_pos } }

(** val combine_span : span -> span -> span **)

let combine_span s1 s2 =
  { file_name = s1.file_name; range = { start_pos = s1.range.start_pos;
    end_pos = s2.range.end_pos } }

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

(** val empty_state : solverState **)

let empty_state =
  { type_metas = (fun _ -> Unsolved); effect_metas = (fun _ -> Unsolved) }

(** val update_type_state :
    metaId -> (aST, aST) metaState -> solverState -> solverState **)

let update_type_state id new_state st =
  { type_metas = (fun x -> if (=) x id then new_state else st.type_metas x);
    effect_metas = st.effect_metas }

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

(** val token_span : token -> span **)

let token_span = function
| TokSymbol (_, s) -> s
| TokString (_, s) -> s
| TokInt (_, s) -> s
| TokComment (_, s) -> s
| TokLParen s -> s
| TokRParen s -> s
| TokLBrace s -> s
| TokRBrace s -> s
| TokLBracket s -> s
| TokRBracket s -> s

(** val is_rparen : token -> bool **)

let is_rparen = function
| TokRParen _ -> true
| _ -> false

(** val is_rbracket : token -> bool **)

let is_rbracket = function
| TokRBracket _ -> true
| _ -> false

(** val is_rbrace : token -> bool **)

let is_rbrace = function
| TokRBrace _ -> true
| _ -> false

type 'a parseResult =
| ParseOk of 'a * token list
| ParseErr of char list

(** val parse_cst : int -> token list -> cST parseResult **)

let rec parse_cst fuel tokens =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> ParseErr
    ('O'::('u'::('t'::(' '::('o'::('f'::(' '::('f'::('u'::('e'::('l'::[]))))))))))))
    (fun fuel' ->
    match tokens with
    | [] ->
      ParseErr
        ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('e'::('n'::('d'::(' '::('o'::('f'::(' '::('i'::('n'::('p'::('u'::('t'::[])))))))))))))))))))))))
    | t :: rest ->
      (match t with
       | TokSymbol (name, s) -> ParseOk ((Symbol (name, s)), rest)
       | TokString (val0, s) -> ParseOk ((StringLiteral (val0, s)), rest)
       | TokInt (val0, s) -> ParseOk ((IntegerLiteral (val0, s)), rest)
       | TokComment (text, s) -> ParseOk ((CommentCST (text, s)), rest)
       | TokLParen s_start ->
         (match parse_sequence fuel' rest is_rparen with
          | ParseOk (p, rest') ->
            let (elements, s_end) = p in
            ParseOk ((Tuple (elements, (combine_span s_start s_end))), rest')
          | ParseErr e -> ParseErr e)
       | TokLBrace s_start ->
         (match parse_sequence fuel' rest is_rbrace with
          | ParseOk (p, rest') ->
            let (elements, s_end) = p in
            ParseOk ((Block (elements, (Tuple ([],
            (combine_span s_start s_end))), (combine_span s_start s_end))),
            rest')
          | ParseErr e -> ParseErr e)
       | TokLBracket s_start ->
         (match parse_sequence fuel' rest is_rbracket with
          | ParseOk (p, rest') ->
            let (elements, s_end) = p in
            ParseOk ((ListLiteral (elements, (combine_span s_start s_end))),
            rest')
          | ParseErr e -> ParseErr e)
       | _ ->
         ParseErr
           ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('o'::('k'::('e'::('n'::[]))))))))))))))))))
    fuel

(** val parse_sequence :
    int -> token list -> (token -> bool) -> (cST list * span) parseResult **)

and parse_sequence fuel tokens end_token_type =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> ParseErr
    ('O'::('u'::('t'::(' '::('o'::('f'::(' '::('f'::('u'::('e'::('l'::[]))))))))))))
    (fun fuel' ->
    match tokens with
    | [] ->
      ParseErr
        ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('e'::('n'::('d'::(' '::('o'::('f'::(' '::('i'::('n'::('p'::('u'::('t'::(','::(' '::('m'::('i'::('s'::('s'::('i'::('n'::('g'::(' '::('c'::('l'::('o'::('s'::('i'::('n'::('g'::(' '::('t'::('o'::('k'::('e'::('n'::[]))))))))))))))))))))))))))))))))))))))))))))))
    | t :: rest ->
      if end_token_type t
      then ParseOk (([], (token_span t)), rest)
      else (match parse_cst fuel' tokens with
            | ParseOk (first_cst, rest') ->
              (match parse_sequence fuel' rest' end_token_type with
               | ParseOk (p, rest'') ->
                 let (rest_csts, end_s) = p in
                 ParseOk (((first_cst :: rest_csts), end_s), rest'')
               | ParseErr e -> ParseErr e)
            | ParseErr e -> ParseErr e))
    fuel

(** val tokenize : int -> char list -> pos -> token list **)

let rec tokenize fuel input current_pos =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun fuel' ->
    match input with
    | [] -> []
    | c::rest ->
      if (=) c '{'
      then (TokLBrace empty_span) :: (tokenize fuel' rest current_pos)
      else if (=) c '}'
           then (TokRBrace empty_span) :: (tokenize fuel' rest current_pos)
           else tokenize fuel' rest current_pos)
    fuel

type typeEnv = (char list * aST) list

(** val lookup_type : char list -> typeEnv -> aST option **)

let rec lookup_type name = function
| [] -> None
| p :: rest ->
  let (k, v) = p in if eqb1 name k then Some v else lookup_type name rest

type 'a tyResult =
| TyOk of 'a
| TyErr of char list

(** val eq_ast : aST -> aST -> bool **)

let rec eq_ast t1 t2 =
  match t1 with
  | AstRef n1 -> (match t2 with
                  | AstRef n2 -> eqb1 n1 n2
                  | _ -> false)
  | AstStringLit s1 ->
    (match t2 with
     | AstStringLit s2 -> eqb1 s1 s2
     | _ -> false)
  | AstIntLit n1 -> (match t2 with
                     | AstIntLit n2 -> eqb n1 n2
                     | _ -> false)
  | AstBoolLit b1 -> (match t2 with
                      | AstBoolLit b2 -> eqb0 b1 b2
                      | _ -> false)
  | AstPi (n1, ty1, ret1, _) ->
    (match t2 with
     | AstPi (n2, ty2, ret2, _) ->
       (&&) ((&&) (eqb1 n1 n2) (eq_ast ty1 ty2)) (eq_ast ret1 ret2)
     | _ -> false)
  | AstMeta m1 -> (match t2 with
                   | AstMeta m2 -> eqb m1 m2
                   | _ -> false)
  | _ -> false

(** val typeUniverse : aST **)

let typeUniverse =
  AstRef ('T'::('y'::('p'::('e'::[]))))

(** val intType : aST **)

let intType =
  AstRef ('I'::('n'::('t'::[])))

(** val stringType : aST **)

let stringType =
  AstRef ('S'::('t'::('r'::('i'::('n'::('g'::[]))))))

(** val boolType : aST **)

let boolType =
  AstRef ('B'::('o'::('o'::('l'::[]))))

(** val infer_check : typeEnv -> aST -> aST option -> aST tyResult **)

let rec infer_check env expr expected =
  match expr with
  | AstRef name ->
    (match lookup_type name env with
     | Some ty ->
       (match expected with
        | Some expTy ->
          if eq_ast ty expTy
          then TyOk ty
          else TyErr
                 ('T'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))
        | None -> TyOk ty)
     | None ->
       TyErr
         (append
           ('U'::('n'::('b'::('o'::('u'::('n'::('d'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(':'::(' '::[]))))))))))))))))))
           name))
  | AstStringLit _ ->
    (match expected with
     | Some expTy ->
       if eq_ast stringType expTy
       then TyOk stringType
       else TyErr
              ('T'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))
     | None -> TyOk stringType)
  | AstIntLit _ ->
    (match expected with
     | Some expTy ->
       if eq_ast intType expTy
       then TyOk intType
       else TyErr
              ('T'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))
     | None -> TyOk intType)
  | AstBoolLit _ ->
    (match expected with
     | Some expTy ->
       if eq_ast boolType expTy
       then TyOk boolType
       else TyErr
              ('T'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))
     | None -> TyOk boolType)
  | AstApp (func, args) ->
    (match infer_check env func None with
     | TyOk a ->
       (match a with
        | AstPi (_, argTy, retTy, _) ->
          (match args with
           | [] ->
             TyErr
               ('C'::('a'::('n'::('n'::('o'::('t'::(' '::('a'::('p'::('p'::('l'::('y'::(' '::('t'::('o'::(' '::('z'::('e'::('r'::('o'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[]))))))))))))))))))))))))))))))
           | arg :: _ ->
             (match infer_check env arg (Some argTy) with
              | TyOk _ ->
                (match expected with
                 | Some expTy ->
                   if eq_ast retTy expTy
                   then TyOk retTy
                   else TyErr
                          ('T'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))
                 | None -> TyOk retTy)
              | TyErr e -> TyErr e))
        | _ ->
          TyErr
            ('C'::('a'::('n'::('n'::('o'::('t'::(' '::('a'::('p'::('p'::('l'::('y'::(' '::('t'::('o'::(' '::('n'::('o'::('n'::('-'::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))
     | TyErr e -> TyErr e)
  | AstLam (argName, argTy, body) ->
    (match expected with
     | Some a ->
       (match a with
        | AstPi (_, expArgTy, expRetTy, _) ->
          if eq_ast argTy expArgTy
          then (match infer_check ((argName, argTy) :: env) body (Some
                        expRetTy) with
                | TyOk _ -> TyOk (AstPi (argName, argTy, expRetTy, []))
                | TyErr e -> TyErr e)
          else TyErr
                 ('L'::('a'::('m'::('b'::('d'::('a'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::(' '::('t'::('y'::('p'::('e'::(' '::('d'::('o'::('e'::('s'::(' '::('n'::('o'::('t'::(' '::('m'::('a'::('t'::('c'::('h'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('P'::('i'::(' '::('t'::('y'::('p'::('e'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          TyErr
            ('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('P'::('i'::(' '::('t'::('y'::('p'::('e'::(' '::('f'::('o'::('r'::(' '::('l'::('a'::('m'::('b'::('d'::('a'::[]))))))))))))))))))))))))))))
     | None ->
       (match infer_check ((argName, argTy) :: env) body None with
        | TyOk bodyTy -> TyOk (AstPi (argName, argTy, bodyTy, []))
        | TyErr e -> TyErr e))
  | AstPi (argName, argTy, retTy, _) ->
    (match infer_check env argTy (Some typeUniverse) with
     | TyOk _ ->
       (match infer_check ((argName, argTy) :: env) retTy (Some typeUniverse) with
        | TyOk _ ->
          (match expected with
           | Some expTy ->
             if eq_ast typeUniverse expTy
             then TyOk typeUniverse
             else TyErr
                    ('T'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))
           | None -> TyOk typeUniverse)
        | TyErr e -> TyErr e)
     | TyErr e -> TyErr e)
  | AstLet (name, value, body) ->
    (match infer_check env value None with
     | TyOk valTy -> infer_check ((name, valTy) :: env) body expected
     | TyErr e -> TyErr e)
  | AstIf (cond, thenB, elseB) ->
    (match infer_check env cond (Some boolType) with
     | TyOk _ ->
       (match infer_check env thenB expected with
        | TyOk thenTy ->
          (match infer_check env elseB (Some thenTy) with
           | TyOk _ -> TyOk thenTy
           | TyErr e -> TyErr e)
        | TyErr e -> TyErr e)
     | TyErr e -> TyErr e)
  | AstDef (_, _, params, ret_ty, body) ->
    let build_env =
      let rec build_env ps e =
        match ps with
        | [] -> e
        | p :: rest ->
          let (pname, pty) = p in build_env rest ((pname, pty) :: e)
      in build_env
    in
    let body_env = build_env params env in
    (match infer_check body_env body (Some ret_ty) with
     | TyOk _ ->
       let build_pi =
         let rec build_pi = function
         | [] -> ret_ty
         | p :: rest ->
           let (pname, pty) = p in AstPi (pname, pty, (build_pi rest), [])
         in build_pi
       in
       TyOk (build_pi params)
     | TyErr e -> TyErr e)
  | AstEnum (_, _, _) -> TyOk (AstRef ('U'::('n'::('i'::('t'::[])))))
  | AstMatch (expr0, cases) ->
    (match infer_check env expr0 None with
     | TyOk _ ->
       let rec check_cases = function
       | [] ->
         TyErr
           ('E'::('m'::('p'::('t'::('y'::(' '::('m'::('a'::('t'::('c'::('h'::[])))))))))))
       | p :: rest ->
         let (_, body) = p in
         (match rest with
          | [] -> infer_check env body expected
          | _ :: _ ->
            (match infer_check env body expected with
             | TyOk ty_body ->
               (match check_cases rest with
                | TyOk ty_rest ->
                  if eq_ast ty_body ty_rest
                  then TyOk ty_body
                  else TyErr
                         ('M'::('a'::('t'::('c'::('h'::(' '::('b'::('r'::('a'::('n'::('c'::('h'::('e'::('s'::(' '::('h'::('a'::('v'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::('i'::('n'::('g'::(' '::('t'::('y'::('p'::('e'::('s'::[])))))))))))))))))))))))))))))))))))))
                | TyErr s -> TyErr s)
             | TyErr s -> TyErr s))
       in check_cases cases
     | TyErr s -> TyErr s)
  | AstRecord (_, _, _) -> TyOk (AstRef ('U'::('n'::('i'::('t'::[])))))
  | AstFieldAccess (expr0, _) ->
    (match infer_check env expr0 None with
     | TyOk _ -> TyOk (AstRef ('U'::('n'::('i'::('t'::[])))))
     | TyErr s -> TyErr s)
  | AstMeta _ ->
    TyErr
      ('C'::('o'::('r'::('e'::(' '::('C'::('h'::('e'::('c'::('k'::('e'::('r'::(':'::(' '::('E'::('n'::('c'::('o'::('u'::('n'::('t'::('e'::('r'::('e'::('d'::(' '::('u'::('n'::('s'::('o'::('l'::('v'::('e'::('d'::(' '::('m'::('e'::('t'::('a'::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::[])))))))))))))))))))))))))))))))))))))))))))))))
  | _ ->
    TyErr
      ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('A'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('c'::('h'::('e'::('c'::('k'::('e'::('r'::[]))))))))))))))))))))))))))))))))

type elabState = { next_meta : int; solver_st : solverState }

(** val init_elab_state : elabState **)

let init_elab_state =
  { next_meta = 0; solver_st = empty_state }

type 'a elabResult =
| ElabOk of 'a * elabState
| ElabErr of char list * elabState

type 'a elabM = elabState -> 'a elabResult

(** val ret : 'a1 -> 'a1 elabM **)

let ret a s =
  ElabOk (a, s)

(** val bind : 'a1 elabM -> ('a1 -> 'a2 elabM) -> 'a2 elabM **)

let bind m f s =
  match m s with
  | ElabOk (a, s') -> f a s'
  | ElabErr (e, s') -> ElabErr (e, s')

(** val throw : char list -> 'a1 elabM **)

let throw e s =
  ElabErr (e, s)

(** val fresh_meta : aST elabM **)

let fresh_meta s =
  let id = s.next_meta in
  let s' = { next_meta = (add id (Stdlib.Int.succ 0)); solver_st =
    s.solver_st }
  in
  ElabOk ((AstMeta id), s')

(** val get_solver : solverState elabM **)

let get_solver s =
  ElabOk (s.solver_st, s)

(** val put_solver : solverState -> unit elabM **)

let put_solver st s =
  ElabOk ((), { next_meta = s.next_meta; solver_st = st })

(** val zonk : int -> aST -> aST elabM **)

let rec zonk fuel ty =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> ret ty)
    (fun f ->
    match ty with
    | AstPi (arg, t, ret_ty, effs) ->
      bind (zonk f t) (fun t' ->
        bind (zonk f ret_ty) (fun ret_ty' ->
          ret (AstPi (arg, t', ret_ty', effs))))
    | AstMatch (expr, cases) ->
      bind (zonk f expr) (fun expr' ->
        let map_cases =
          let rec map_cases = function
          | [] -> ret []
          | p :: rest ->
            let (pat, body) = p in
            bind (zonk f body) (fun body' ->
              bind (map_cases rest) (fun rest' -> ret ((pat, body') :: rest')))
          in map_cases
        in
        bind (map_cases cases) (fun cases' -> ret (AstMatch (expr', cases'))))
    | AstFieldAccess (expr, field) ->
      bind (zonk f expr) (fun expr' -> ret (AstFieldAccess (expr', field)))
    | AstMeta m ->
      bind get_solver (fun st ->
        match st.type_metas m with
        | Solved t -> zonk f t
        | _ -> ret (AstMeta m))
    | _ -> ret ty)
    fuel

(** val unify : int -> aST -> aST -> unit elabM **)

let rec unify fuel t1 t2 =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    throw
      ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('o'::('u'::('t'::(' '::('o'::('f'::(' '::('f'::('u'::('e'::('l'::[]))))))))))))))))))))))))
    (fun f ->
    bind (zonk f t1) (fun t1' ->
      bind (zonk f t2) (fun t2' ->
        match t1' with
        | AstRef n1 ->
          (match t2' with
           | AstRef n2 ->
             if eqb1 n1 n2
             then ret ()
             else throw
                    ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(':'::(' '::('n'::('a'::('m'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))
           | AstMeta m ->
             bind get_solver (fun st ->
               put_solver (update_type_state m (Solved t1') st))
           | _ ->
             throw
               ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))
        | AstPi (_, ty1, ret1, _) ->
          (match t2' with
           | AstPi (_, ty2, ret2, _) ->
             bind (unify f ty1 ty2) (fun _ -> unify f ret1 ret2)
           | AstMeta m ->
             bind get_solver (fun st ->
               put_solver (update_type_state m (Solved t1') st))
           | _ ->
             throw
               ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))
        | AstMeta m ->
          (match t2' with
           | AstMeta m0 ->
             if (=) m m0
             then ret ()
             else bind get_solver (fun st ->
                    put_solver (update_type_state m (Solved (AstMeta m0)) st))
           | _ ->
             bind get_solver (fun st ->
               put_solver (update_type_state m (Solved t2') st)))
        | _ ->
          (match t2' with
           | AstMeta m ->
             bind get_solver (fun st ->
               put_solver (update_type_state m (Solved t1') st))
           | _ ->
             throw
               ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::[])))))))))))))))))))))))))))))))))))))))
    fuel

(** val elaborate : typeEnv -> cST -> aST option -> (aST * aST) elabM **)

let rec elaborate env expr expected =
  match expr with
  | Symbol (name, _) ->
    (match lookup_type name env with
     | Some ty ->
       (match expected with
        | Some expTy ->
          bind
            (unify (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ
              0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
              ty expTy)
            (fun _ -> ret ((AstRef name), ty))
        | None -> ret ((AstRef name), ty))
     | None ->
       throw
         (append
           ('U'::('n'::('b'::('o'::('u'::('n'::('d'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(':'::(' '::[]))))))))))))))))))
           name))
  | Block (stmts, ret_expr, _) ->
    let map_elabs =
      let rec map_elabs = function
      | [] -> ret []
      | x :: xs ->
        bind (elaborate env x None) (fun res ->
          let (a, _) = res in
          bind (map_elabs xs) (fun rest -> ret (a :: rest)))
      in map_elabs
    in
    bind (map_elabs stmts) (fun stmtsAst ->
      bind (elaborate env ret_expr None) (fun retAst ->
        ret ((AstBlock (stmtsAst, (fst retAst))), (snd retAst))))
  | StringLiteral (s, _) ->
    bind
      (match expected with
       | Some exp ->
         unify (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ
           0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           stringType exp
       | None -> ret ())
      (fun _ -> ret ((AstStringLit s), stringType))
  | IntegerLiteral (_, _) ->
    bind
      (match expected with
       | Some exp ->
         unify (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ
           0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           intType exp
       | None -> ret ())
      (fun _ ->
      ret ((AstIntLit (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        0))))))))))))))))))))))))))))))))))))))))))), intType))
  | BoolLiteral (b, _) ->
    bind
      (match expected with
       | Some exp ->
         unify (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
           (Stdlib.Int.succ
           0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           boolType exp
       | None -> ret ())
      (fun _ -> ret ((AstBoolLit b), boolType))
  | SeqOf (_, _) ->
    throw
      ('S'::('e'::('q'::('O'::('f'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('o'::('r'::[])))))))))))))))))))))))))))))))))))
  | LetCST (name, value, body, _) ->
    bind (elaborate env value None) (fun valueAst ->
      bind (elaborate ((name, (snd valueAst)) :: env) body expected)
        (fun bodyAst ->
        ret ((AstLet (name, (fst valueAst), (fst bodyAst))), (snd bodyAst))))
  | IfCST (cond, thenB, elseB, _) ->
    bind (elaborate env cond None) (fun condAst ->
      bind (elaborate env thenB expected) (fun thenAst ->
        bind (elaborate env elseB expected) (fun elseAst ->
          ret ((AstIf ((fst condAst), (fst thenAst), (fst elseAst))),
            (snd thenAst)))))
  | DefCST (name, type_params, params, ret_ty, body, _) ->
    let map_params =
      let rec map_params = function
      | [] -> ret []
      | p :: rest ->
        let (pname, pty) = p in
        bind (elaborate env pty (Some typeUniverse)) (fun tyAst ->
          bind (map_params rest) (fun restAst ->
            ret ((pname, (fst tyAst)) :: restAst)))
      in map_params
    in
    bind (map_params params) (fun paramsAst ->
      let build_env =
        let rec build_env ps env0 =
          match ps with
          | [] -> env0
          | p :: rest ->
            let (pname, pty) = p in build_env rest ((pname, pty) :: env0)
        in build_env
      in
      let body_env = build_env paramsAst env in
      bind (elaborate env ret_ty (Some typeUniverse)) (fun retAst ->
        bind (elaborate body_env body (Some (fst retAst))) (fun bodyAst ->
          ret ((AstDef (name, type_params, paramsAst, (fst retAst),
            (fst bodyAst))), (AstRef ('U'::('n'::('i'::('t'::[])))))))))
  | LamCST (arg_name, opt_arg_ty, body, _) ->
    bind
      (match opt_arg_ty with
       | Some ty -> elaborate env ty (Some typeUniverse)
       | None -> bind fresh_meta (fun m -> ret (m, typeUniverse)))
      (fun argTyAst ->
      bind (elaborate ((arg_name, (fst argTyAst)) :: env) body None)
        (fun bodyAst ->
        let arrTy = AstPi (arg_name, (fst argTyAst), (snd bodyAst), []) in
        ret ((AstLam (arg_name, (fst argTyAst), (fst bodyAst))), arrTy)))
  | AppCST (func, args, _) ->
    bind (elaborate env func None) (fun funcAst ->
      let check_args =
        let rec check_args fs = function
        | [] -> ret ([], fs)
        | a :: rest ->
          (match fs with
           | AstPi (_, arg_ty, ret_ty, _) ->
             bind (elaborate env a (Some arg_ty)) (fun aAst ->
               bind (check_args ret_ty rest) (fun restAst ->
                 ret (((fst aAst) :: (fst restAst)), (snd restAst))))
           | AstMeta _ ->
             bind fresh_meta (fun argTyM ->
               bind fresh_meta (fun retTyM ->
                 bind
                   (unify (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ
                     0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                     fs (AstPi (('x'::[]), argTyM, retTyM, [])))
                   (fun _ ->
                   bind (elaborate env a (Some argTyM)) (fun aAst ->
                     bind (check_args retTyM rest) (fun restAst ->
                       ret (((fst aAst) :: (fst restAst)), (snd restAst)))))))
           | _ ->
             throw
               ('C'::('a'::('n'::('n'::('o'::('t'::(' '::('a'::('p'::('p'::('l'::('y'::(' '::('n'::('o'::('n'::('-'::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))
        in check_args
      in
      bind (check_args (snd funcAst) args) (fun argsRes ->
        bind
          (match expected with
           | Some exp ->
             unify (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ
               0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
               (snd argsRes) exp
           | None -> ret ())
          (fun _ ->
          ret ((AstApp ((fst funcAst), (fst argsRes))), (snd argsRes)))))
  | EnumCST (name, type_params, _, _) ->
    ret ((AstEnum (name, type_params, [])), (AstRef
      ('U'::('n'::('i'::('t'::[]))))))
  | MatchCST (expr0, cases, _) ->
    bind (elaborate env expr0 None) (fun exprAst ->
      let elab_cases = fun cs ->
        match cs with
        | [] ->
          throw
            ('E'::('m'::('p'::('t'::('y'::(' '::('m'::('a'::('t'::('c'::('h'::(' '::('n'::('o'::('t'::(' '::('a'::('l'::('l'::('o'::('w'::('e'::('d'::[])))))))))))))))))))))))
        | p :: _ ->
          let (pat, body) = p in
          (match pat with
           | PatWildcardCST _ ->
             bind (elaborate env body expected) (fun bodyAst ->
               ret (((PatWildcard, (fst bodyAst)) :: []), (snd bodyAst)))
           | PatVarCST (v, _) ->
             bind fresh_meta (fun m ->
               bind (elaborate ((v, m) :: env) body expected) (fun bodyAst ->
                 ret ((((PatVar v), (fst bodyAst)) :: []), (snd bodyAst))))
           | PatConstructorCST (name, vars, _) ->
             let add_vars =
               let rec add_vars vs e =
                 match vs with
                 | [] -> ret e
                 | v :: rest_vs ->
                   bind fresh_meta (fun m -> add_vars rest_vs ((v, m) :: e))
               in add_vars
             in
             bind (add_vars vars env) (fun case_env ->
               bind (elaborate case_env body expected) (fun bodyAst ->
                 ret ((((PatConstructor (name, vars)), (fst bodyAst)) :: []),
                   (snd bodyAst)))))
      in
      let process_cases =
        let rec process_cases = function
        | [] ->
          throw
            ('E'::('m'::('p'::('t'::('y'::(' '::('m'::('a'::('t'::('c'::('h'::[])))))))))))
        | single :: rest ->
          let (pat, body) = single in
          (match rest with
           | [] -> elab_cases (single :: [])
           | _ :: _ ->
             bind (elab_cases ((pat, body) :: [])) (fun res_first ->
               bind (process_cases rest) (fun res_rest ->
                 bind
                   (unify (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ
                     0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                     (snd res_first) (snd res_rest))
                   (fun _ ->
                   ret ((app (fst res_first) (fst res_rest)), (snd res_first))))))
        in process_cases
      in
      bind (process_cases cases) (fun casesRes ->
        ret ((AstMatch ((fst exprAst), (fst casesRes))), (snd casesRes))))
  | RecordCST (name, type_params, _, _) ->
    ret ((AstRecord (name, type_params, [])), (AstRef
      ('U'::('n'::('i'::('t'::[]))))))
  | FieldAccessCST (expr0, field, _) ->
    bind (elaborate env expr0 None) (fun exprAst ->
      ret ((AstFieldAccess ((fst exprAst), field)), (AstRef
        ('T'::('y'::('p'::('e'::[])))))))
  | _ ->
    throw
      ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))

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

(** val concat_strings : char list -> char list list -> char list **)

let rec concat_strings sep = function
| [] -> []
| x :: xs ->
  (match xs with
   | [] -> x
   | _ :: _ -> append x (append sep (concat_strings sep xs)))

(** val stringify_ts : typeScriptAST -> char list **)

let rec stringify_ts expr =
  let map_ts =
    let rec map_ts = function
    | [] -> []
    | x :: xs -> (stringify_ts x) :: (map_ts xs)
    in map_ts
  in
  (match expr with
   | TsNumberLiteral n0 -> n0
   | TsStringLiteral s -> append ('"'::[]) (append s ('"'::[]))
   | TsBooleanLiteral b ->
     if b
     then 't'::('r'::('u'::('e'::[])))
     else 'f'::('a'::('l'::('s'::('e'::[]))))
   | TsIdentifier name -> name
   | TsPropertyAccess (obj, prop) ->
     append (stringify_ts obj) (append ('.'::[]) prop)
   | TsCall (callee, args) ->
     append (stringify_ts callee)
       (append ('('::[])
         (append (concat_strings (','::(' '::[])) (map_ts args)) (')'::[])))
   | TsArrow (params, body) ->
     append ('('::[])
       (append (concat_strings (','::(' '::[])) params)
         (append (')'::(' '::('='::('>'::(' '::[]))))) (stringify_ts body)))
   | TsBlock (stmts, ret0) ->
     append ('{'::(' '::[]))
       (append (concat_strings (';'::(' '::[])) (map_ts stmts))
         (append
           (';'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))))
           (append (stringify_ts ret0) (';'::(' '::('}'::[]))))))
   | TsArray elements ->
     append ('['::[])
       (append (concat_strings (','::(' '::[])) (map_ts elements)) (']'::[]))
   | TsAwait e ->
     append ('a'::('w'::('a'::('i'::('t'::(' '::[])))))) (stringify_ts e)
   | TsRaw s -> s)

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

(** val concat_strings0 : char list -> char list list -> char list **)

let rec concat_strings0 sep = function
| [] -> []
| x :: xs ->
  (match xs with
   | [] -> x
   | _ :: _ -> append x (append sep (concat_strings0 sep xs)))

(** val stringify_go : goAST -> char list **)

let rec stringify_go expr =
  let map_go =
    let rec map_go = function
    | [] -> []
    | x :: xs -> (stringify_go x) :: (map_go xs)
    in map_go
  in
  (match expr with
   | GoIntLiteral n0 -> n0
   | GoStringLiteral s -> append ('"'::[]) (append s ('"'::[]))
   | GoBoolLiteral b ->
     if b
     then 't'::('r'::('u'::('e'::[])))
     else 'f'::('a'::('l'::('s'::('e'::[]))))
   | GoIdentifier name -> name
   | GoSelector (obj, prop) ->
     append (stringify_go obj) (append ('.'::[]) prop)
   | GoCall (callee, args) ->
     append (stringify_go callee)
       (append ('('::[])
         (append (concat_strings0 (','::(' '::[])) (map_go args)) (')'::[])))
   | GoFuncLiteral (params, body) ->
     append ('f'::('u'::('n'::('c'::('('::[])))))
       (append
         (concat_strings0
           (' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(','::(' '::[]))))))))))))))
           params)
         (append
           (' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(')'::(' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(' '::('{'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))))))))))))))))))))))))))))))
           (append (stringify_go body) (' '::('}'::[])))))
   | GoBlock (stmts, ret0) ->
     append
       ('f'::('u'::('n'::('c'::('('::(')'::(' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(' '::('{'::(' '::[])))))))))))))))))))))
       (append (concat_strings0 (';'::(' '::[])) (map_go stmts))
         (append
           (';'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))))
           (append (stringify_go ret0) (' '::('}'::('('::(')'::[])))))))
   | GoArray elements ->
     append
       ('['::(']'::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::('{'::[]))))))))))))))
       (append (concat_strings0 (','::(' '::[])) (map_go elements)) ('}'::[]))
   | GoRaw s -> s)

(** val nat_to_string : int -> char list **)

let nat_to_string _ =
  '<'::('n'::('a'::('t'::('>'::[]))))

(** val emit_ts : aST -> typeScriptAST **)

let rec emit_ts expr =
  let map_ts =
    let rec map_ts = function
    | [] -> []
    | x :: xs -> (emit_ts x) :: (map_ts xs)
    in map_ts
  in
  (match expr with
   | AstRef name -> TsIdentifier name
   | AstTuple elems -> TsArray (map_ts elems)
   | AstStringLit s -> TsStringLiteral s
   | AstIntLit n0 -> TsNumberLiteral (nat_to_string n0)
   | AstBoolLit b -> TsBooleanLiteral b
   | AstBlock (stmts, ret0) -> TsBlock ((map_ts stmts), (emit_ts ret0))
   | AstApp (func, args) -> TsCall ((emit_ts func), (map_ts args))
   | AstLam (argName, _, body) -> TsArrow ((argName :: []), (emit_ts body))
   | AstPi (argName, _, _, _) ->
     TsRaw
       (append ('('::[])
         (append argName
           (':'::(' '::('a'::('n'::('y'::(')'::(' '::('='::('>'::(' '::('a'::('n'::('y'::[])))))))))))))))
   | AstDo (op, args) -> TsAwait (TsCall ((emit_ts op), (map_ts args)))
   | AstHandle (_, _, _) ->
     TsRaw
       ('/'::('*'::(' '::('h'::('a'::('n'::('d'::('l'::('e'::(' '::('*'::('/'::[]))))))))))))
   | AstLet (name, value, body) ->
     TsBlock (((TsRaw
       (append ('c'::('o'::('n'::('s'::('t'::(' '::[]))))))
         (append name
           (append (' '::('='::(' '::[]))) (stringify_ts (emit_ts value)))))) :: []),
       (emit_ts body))
   | AstIf (cond, thenB, elseB) ->
     TsRaw
       (append ('('::[])
         (append (stringify_ts (emit_ts cond))
           (append (' '::('?'::(' '::[])))
             (append (stringify_ts (emit_ts thenB))
               (append (' '::(':'::(' '::[])))
                 (append (stringify_ts (emit_ts elseB)) (')'::[])))))))
   | AstDef (name, _, params, _, body) ->
     let get_param_names =
       let rec get_param_names = function
       | [] -> []
       | p :: rest -> let (n0, _) = p in n0 :: (get_param_names rest)
       in get_param_names
     in
     TsRaw
     (append
       ('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::[])))))))))
       (append name
         (append ('('::[])
           (append
             (concat_strings0 (','::(' '::[])) (get_param_names params))
             (append
               (')'::(' '::('{'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))))))
               (append (stringify_ts (emit_ts body)) (';'::(' '::('}'::[])))))))))
   | AstEnum (name, _, _) ->
     TsRaw
       (append ('t'::('y'::('p'::('e'::(' '::[])))))
         (append name
           (' '::('='::(' '::('a'::('n'::('y'::(';'::(' '::('/'::('*'::(' '::('s'::('i'::('m'::('p'::('l'::('i'::('f'::('i'::('e'::('d'::(' '::('e'::('n'::('u'::('m'::(' '::('*'::('/'::[])))))))))))))))))))))))))))))))
   | AstMatch (expr0, cases) ->
     let emit_cases =
       let rec emit_cases = function
       | [] ->
         't'::('h'::('r'::('o'::('w'::(' '::('n'::('e'::('w'::(' '::('E'::('r'::('r'::('o'::('r'::('('::('\''::('N'::('o'::('n'::('-'::('e'::('x'::('h'::('a'::('u'::('s'::('t'::('i'::('v'::('e'::(' '::('m'::('a'::('t'::('c'::('h'::('\''::(')'::(';'::[])))))))))))))))))))))))))))))))))))))))
       | p :: rest ->
         let (pat, body) = p in
         (match pat with
          | PatWildcard ->
            append ('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))
              (append (stringify_ts (emit_ts body)) (';'::[]))
          | PatVar v ->
            append ('c'::('o'::('n'::('s'::('t'::(' '::[]))))))
              (append v
                (append
                  (' '::('='::(' '::('_'::('m'::('a'::('t'::('c'::('h'::('_'::('v'::('a'::('l'::(';'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[]))))))))))))))))))))))
                  (append (stringify_ts (emit_ts body)) (';'::[]))))
          | PatConstructor (cname, vars) ->
            append
              ('i'::('f'::(' '::('('::('_'::('m'::('a'::('t'::('c'::('h'::('_'::('v'::('a'::('l'::('.'::('_'::('t'::('a'::('g'::(' '::('='::('='::('='::(' '::('\''::[])))))))))))))))))))))))))
              (append cname
                (append ('\''::(')'::(' '::('{'::(' '::[])))))
                  (append
                    (let rec bind_vars vs idx =
                       match vs with
                       | [] -> []
                       | v :: v_rest ->
                         append ('c'::('o'::('n'::('s'::('t'::(' '::[]))))))
                           (append v
                             (append
                               (' '::('='::(' '::('_'::('m'::('a'::('t'::('c'::('h'::('_'::('v'::('a'::('l'::('.'::('a'::('r'::('g'::('s'::('['::[])))))))))))))))))))
                               (append (nat_to_string idx)
                                 (append (']'::(';'::(' '::[])))
                                   (bind_vars v_rest (Stdlib.Int.succ idx))))))
                     in bind_vars vars 0)
                    (append
                      ('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))
                      (append (stringify_ts (emit_ts body))
                        (append (';'::(' '::('}'::(' '::[]))))
                          (emit_cases rest))))))))
       in emit_cases
     in
     TsRaw
     (append
       ('('::('('::(')'::(' '::('='::('>'::(' '::('{'::(' '::('c'::('o'::('n'::('s'::('t'::(' '::('_'::('m'::('a'::('t'::('c'::('h'::('_'::('v'::('a'::('l'::(' '::('='::(' '::[]))))))))))))))))))))))))))))
       (append (stringify_ts (emit_ts expr0))
         (append (';'::(' '::[]))
           (append (emit_cases cases) (' '::('}'::(')'::('('::(')'::[])))))))))
   | AstRecord (name, _, _) ->
     TsRaw
       (append
         ('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::(' '::[]))))))))))
         (append name
           (' '::('{'::(' '::('['::('k'::('e'::('y'::(':'::(' '::('s'::('t'::('r'::('i'::('n'::('g'::(']'::(':'::(' '::('a'::('n'::('y'::(' '::('}'::[])))))))))))))))))))))))))
   | AstFieldAccess (expr0, field) ->
     TsRaw (append (stringify_ts (emit_ts expr0)) (append ('.'::[]) field))
   | AstMeta id ->
     TsRaw
       (append
         ('/'::('*'::(' '::('?'::('m'::('e'::('t'::('a'::('_'::[])))))))))
         (append (nat_to_string id) (' '::('*'::('/'::[])))))
   | AstError e ->
     TsRaw
       (append
         ('/'::('*'::(' '::('E'::('R'::('R'::('O'::('R'::(':'::(' '::[]))))))))))
         (append e (' '::('*'::('/'::[]))))))

(** val emit_go : aST -> goAST **)

let rec emit_go expr =
  let map_go =
    let rec map_go = function
    | [] -> []
    | x :: xs -> (emit_go x) :: (map_go xs)
    in map_go
  in
  (match expr with
   | AstRef name -> GoIdentifier name
   | AstTuple elems -> GoArray (map_go elems)
   | AstStringLit s -> GoStringLiteral s
   | AstIntLit n0 -> GoIntLiteral (nat_to_string n0)
   | AstBoolLit b -> GoBoolLiteral b
   | AstBlock (stmts, ret0) -> GoBlock ((map_go stmts), (emit_go ret0))
   | AstApp (func, args) -> GoCall ((emit_go func), (map_go args))
   | AstLam (argName, _, body) ->
     GoFuncLiteral ((argName :: []), (emit_go body))
   | AstPi (argName, _, _, _) ->
     GoRaw
       (append ('f'::('u'::('n'::('c'::('('::[])))))
         (append argName
           (' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(')'::(' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::[])))))))))))))))))))))))))))
   | AstDo (op, args) -> GoCall ((emit_go op), (map_go args))
   | AstHandle (_, _, _) ->
     GoRaw
       ('/'::('*'::(' '::('h'::('a'::('n'::('d'::('l'::('e'::(' '::('*'::('/'::[]))))))))))))
   | AstLet (name, value, body) ->
     GoBlock (((GoRaw
       (append name
         (append (' '::(':'::('='::(' '::[]))))
           (stringify_go (emit_go value))))) :: []),
       (emit_go body))
   | AstIf (cond, thenB, elseB) ->
     GoRaw
       (append
         ('f'::('u'::('n'::('c'::('('::(')'::(' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(' '::('{'::(' '::('i'::('f'::(' '::[]))))))))))))))))))))))))
         (append (stringify_go (emit_go cond))
           (append
             (' '::('{'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[]))))))))))
             (append (stringify_go (emit_go thenB))
               (append
                 (' '::('}'::(' '::('e'::('l'::('s'::('e'::(' '::('{'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))))))))))))
                 (append (stringify_go (emit_go elseB))
                   (' '::('}'::(' '::('}'::('('::(')'::[]))))))))))))
   | AstDef (name, _, params, _, body) ->
     let get_param_names =
       let rec get_param_names = function
       | [] -> []
       | p :: rest -> let (n0, _) = p in n0 :: (get_param_names rest)
       in get_param_names
     in
     GoRaw
     (append ('f'::('u'::('n'::('c'::(' '::[])))))
       (append name
         (append ('('::[])
           (append
             (concat_strings0
               (' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(','::(' '::[]))))))))))))))
               (get_param_names params))
             (append
               (' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(')'::(' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(' '::('{'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))))))))))))))))))))))))))))))
               (append (stringify_go (emit_go body)) (' '::('}'::[]))))))))
   | AstEnum (name, _, _) ->
     GoRaw
       (append ('t'::('y'::('p'::('e'::(' '::[])))))
         (append name
           (' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(' '::('/'::('*'::(' '::('s'::('i'::('m'::('p'::('l'::('i'::('f'::('i'::('e'::('d'::(' '::('e'::('n'::('u'::('m'::(' '::('*'::('/'::[]))))))))))))))))))))))))))))))))))))
   | AstMatch (expr0, cases) ->
     let emit_cases =
       let rec emit_cases = function
       | [] ->
         'p'::('a'::('n'::('i'::('c'::('('::('"'::('N'::('o'::('n'::('-'::('e'::('x'::('h'::('a'::('u'::('s'::('t'::('i'::('v'::('e'::(' '::('m'::('a'::('t'::('c'::('h'::('"'::(')'::[]))))))))))))))))))))))))))))
       | p :: rest ->
         let (pat, body) = p in
         (match pat with
          | PatWildcard ->
            append ('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))
              (stringify_go (emit_go body))
          | PatVar v ->
            append v
              (append
                (' '::(':'::('='::(' '::('_'::('m'::('a'::('t'::('c'::('h'::('_'::('v'::('a'::('l'::(';'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))))))))))))))))))
                (stringify_go (emit_go body)))
          | PatConstructor (cname, vars) ->
            append
              ('i'::('f'::(' '::('_'::('t'::('a'::('g'::(','::(' '::('_'::('o'::('k'::(' '::(':'::('='::(' '::('_'::('m'::('a'::('t'::('c'::('h'::('_'::('v'::('a'::('l'::('.'::('('::('m'::('a'::('p'::('['::('s'::('t'::('r'::('i'::('n'::('g'::(']'::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(')'::(';'::(' '::('_'::('o'::('k'::(' '::('&'::('&'::(' '::('_'::('t'::('a'::('g'::('['::('"'::('_'::('t'::('a'::('g'::('"'::(']'::(' '::('='::('='::(' '::('"'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
              (append cname
                (append ('"'::(' '::('{'::(' '::[]))))
                  (append
                    (let rec bind_vars vs idx =
                       match vs with
                       | [] -> []
                       | v :: v_rest ->
                         append v
                           (append
                             (' '::(':'::('='::(' '::('_'::('t'::('a'::('g'::('['::('"'::('a'::('r'::('g'::('s'::('"'::(']'::('.'::('('::('['::(']'::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(')'::('['::[])))))))))))))))))))))))))))))))))
                             (append (nat_to_string idx)
                               (append (']'::(';'::(' '::[])))
                                 (bind_vars v_rest (Stdlib.Int.succ idx)))))
                     in bind_vars vars 0)
                    (append
                      ('r'::('e'::('t'::('u'::('r'::('n'::(' '::[])))))))
                      (append (stringify_go (emit_go body))
                        (append (' '::('}'::(';'::(' '::[]))))
                          (emit_cases rest))))))))
       in emit_cases
     in
     GoRaw
     (append
       ('f'::('u'::('n'::('c'::('('::(')'::(' '::('i'::('n'::('t'::('e'::('r'::('f'::('a'::('c'::('e'::('{'::('}'::(' '::('{'::(' '::('_'::('m'::('a'::('t'::('c'::('h'::('_'::('v'::('a'::('l'::(' '::(':'::('='::(' '::[])))))))))))))))))))))))))))))))))))
       (append (stringify_go (emit_go expr0))
         (append (';'::(' '::[]))
           (append (emit_cases cases) (' '::('}'::('('::(')'::[]))))))))
   | AstRecord (name, _, _) ->
     GoRaw
       (append ('t'::('y'::('p'::('e'::(' '::[])))))
         (append name
           (' '::('s'::('t'::('r'::('u'::('c'::('t'::('{'::('}'::[])))))))))))
   | AstFieldAccess (expr0, field) ->
     GoRaw (append (stringify_go (emit_go expr0)) (append ('.'::[]) field))
   | AstMeta id ->
     GoRaw
       (append
         ('/'::('*'::(' '::('?'::('m'::('e'::('t'::('a'::('_'::[])))))))))
         (append (nat_to_string id) (' '::('*'::('/'::[])))))
   | AstError e ->
     GoRaw
       (append
         ('/'::('*'::(' '::('E'::('R'::('R'::('O'::('R'::(':'::(' '::[]))))))))))
         (append e (' '::('*'::('/'::[]))))))

(** val gen_spaces : int -> char list **)

let rec gen_spaces n0 =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n' -> append (' '::[]) (gen_spaces n'))
    n0

(** val join_strings : char list -> char list list -> char list **)

let rec join_strings sep = function
| [] -> []
| x :: xs ->
  (match xs with
   | [] -> x
   | _ :: _ -> append x (append sep (join_strings sep xs)))

(** val format_cst : int -> int -> cST -> char list **)

let rec format_cst fuel indent expr =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    '/'::('*'::(' '::('E'::('R'::('R'::('O'::('R'::(':'::(' '::('f'::('o'::('r'::('m'::('a'::('t'::('t'::('e'::('r'::(' '::('o'::('u'::('t'::(' '::('o'::('f'::(' '::('f'::('u'::('e'::('l'::(' '::('*'::('/'::[]))))))))))))))))))))))))))))))))))
    (fun f ->
    match expr with
    | Symbol (name, _) -> name
    | Tuple (elements, _) ->
      append ('('::[])
        (append
          (join_strings (','::(' '::[])) (map (format_cst f indent) elements))
          (')'::[]))
    | ListLiteral (elements, _) ->
      append ('['::[])
        (append
          (join_strings (','::(' '::[])) (map (format_cst f indent) elements))
          (']'::[]))
    | Block (elements, tail, _) ->
      let next_indent = add indent (Stdlib.Int.succ (Stdlib.Int.succ 0)) in
      let nl_indent =
        (ascii_of_nat (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))::
        (gen_spaces next_indent)
      in
      let format_stmts =
        let rec format_stmts = function
        | [] -> []
        | s :: rest ->
          (match s with
           | CommentCST (text, _) ->
             append ('/'::('/'::(' '::[])))
               (append text (append nl_indent (format_stmts rest)))
           | _ ->
             append (format_cst f next_indent s)
               (append (';'::[]) (append nl_indent (format_stmts rest))))
        in format_stmts
      in
      let formatted_elems = format_stmts elements in
      let formatted_tail = format_cst f next_indent tail in
      append ('{'::[])
        (append nl_indent
          (append formatted_elems
            (append formatted_tail
              (append
                ((ascii_of_nat (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))::(gen_spaces
                                                                    indent))
                ('}'::[])))))
    | StringLiteral (val0, _) -> append ('"'::[]) (append val0 ('"'::[]))
    | IntegerLiteral (val0, _) -> val0
    | BoolLiteral (b, _) ->
      if b
      then 't'::('r'::('u'::('e'::[])))
      else 'f'::('a'::('l'::('s'::('e'::[]))))
    | SeqOf (elements, _) ->
      let formatted_elems = map (format_cst f indent) elements in
      let nl_indent =
        (ascii_of_nat (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))::
        (gen_spaces indent)
      in
      join_strings nl_indent formatted_elems
    | CommentCST (text, _) -> append ('/'::('/'::(' '::[]))) text
    | LetCST (name, val0, body, _) ->
      append ('l'::('e'::('t'::(' '::[]))))
        (append name
          (append (' '::('='::(' '::[])))
            (append (format_cst f indent val0)
              (append (';'::[])
                (append
                  ((ascii_of_nat (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))::
                  (gen_spaces indent)) (format_cst f indent body))))))
    | IfCST (cond, thenB, elseB, _) ->
      append ('i'::('f'::(' '::[])))
        (append (format_cst f indent cond)
          (append (' '::('t'::('h'::('e'::('n'::(' '::[]))))))
            (append (format_cst f indent thenB)
              (append (' '::('e'::('l'::('s'::('e'::(' '::[]))))))
                (format_cst f indent elseB)))))
    | DefCST (name, _, params, ret_ty, body, _) ->
      let format_params =
        let rec format_params = function
        | [] -> []
        | p :: rest ->
          let (n0, t) = p in
          (match rest with
           | [] -> append n0 (append (':'::(' '::[])) (format_cst f indent t))
           | _ :: _ ->
             append n0
               (append (':'::(' '::[]))
                 (append (format_cst f indent t)
                   (append (','::(' '::[])) (format_params rest)))))
        in format_params
      in
      append ('d'::('e'::('f'::(' '::[]))))
        (append name
          (append ('('::[])
            (append (format_params params)
              (append (')'::(':'::(' '::[])))
                (append (format_cst f indent ret_ty)
                  (append (' '::('='::(' '::[]))) (format_cst f indent body)))))))
    | LamCST (arg_name, opt_arg_ty, body, _) ->
      let arg_str =
        match opt_arg_ty with
        | Some t ->
          append ('('::[])
            (append arg_name
              (append (':'::(' '::[]))
                (append (format_cst f indent t) (')'::[]))))
        | None -> arg_name
      in
      append ('\\'::[])
        (append arg_str
          (append (' '::('='::('>'::(' '::[])))) (format_cst f indent body)))
    | AppCST (func, args, _) ->
      append (format_cst f indent func)
        (append ('('::[])
          (append
            (join_strings (','::(' '::[])) (map (format_cst f indent) args))
            (')'::[])))
    | EnumCST (name, _, _, _) ->
      append ('e'::('n'::('u'::('m'::(' '::[])))))
        (append name
          (' '::('{'::(' '::('.'::('.'::('.'::(' '::('}'::[])))))))))
    | MatchCST (expr0, cases, _) ->
      let format_cases =
        let rec format_cases = function
        | [] -> []
        | p :: rest ->
          let (pat, body) = p in
          let pat_str =
            match pat with
            | PatWildcardCST _ -> '_'::[]
            | PatVarCST (v, _) -> v
            | PatConstructorCST (cname, vars, _) ->
              (match vars with
               | [] -> cname
               | _ :: _ ->
                 append cname
                   (append ('('::[])
                     (append (join_strings (','::(' '::[])) vars) (')'::[]))))
          in
          append ('c'::('a'::('s'::('e'::(' '::[])))))
            (append pat_str
              (append (' '::('='::('>'::(' '::[]))))
                (append
                  (format_cst f
                    (add indent (Stdlib.Int.succ (Stdlib.Int.succ 0))) body)
                  (append (';'::[])
                    (match rest with
                     | [] -> []
                     | _ :: _ ->
                       append
                         ((ascii_of_nat (Stdlib.Int.succ (Stdlib.Int.succ
                            (Stdlib.Int.succ (Stdlib.Int.succ
                            (Stdlib.Int.succ (Stdlib.Int.succ
                            (Stdlib.Int.succ (Stdlib.Int.succ
                            (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))::
                         (gen_spaces
                           (add indent (Stdlib.Int.succ (Stdlib.Int.succ 0)))))
                         (format_cases rest))))))
        in format_cases
      in
      append ('m'::('a'::('t'::('c'::('h'::(' '::[]))))))
        (append (format_cst f indent expr0)
          (append (' '::('{'::[]))
            (append
              ((ascii_of_nat (Stdlib.Int.succ (Stdlib.Int.succ
                 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                 (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))::(gen_spaces
                                                                   (add
                                                                    indent
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0)))))
              (append (format_cases cases)
                (append
                  ((ascii_of_nat (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))::
                  (gen_spaces indent)) ('}'::[]))))))
    | RecordCST (name, _, _, _) ->
      append ('r'::('e'::('c'::('o'::('r'::('d'::(' '::[])))))))
        (append name
          (' '::('{'::(' '::('.'::('.'::('.'::(' '::('}'::[])))))))))
    | FieldAccessCST (expr0, field, _) ->
      append (format_cst f indent expr0) (append ('.'::[]) field)
    | Error (msg, _) ->
      append
        ('/'::('*'::(' '::('E'::('R'::('R'::('O'::('R'::(':'::(' '::[]))))))))))
        (append msg (' '::('*'::('/'::[])))))
    fuel

(** val ts_to_chester : typeScriptAST -> aST **)

let rec ts_to_chester = function
| TsIdentifier name ->
  if eqb1 name ('n'::('u'::('m'::('b'::('e'::('r'::[]))))))
  then AstRef ('I'::('n'::('t'::[])))
  else if eqb1 name ('s'::('t'::('r'::('i'::('n'::('g'::[]))))))
       then AstRef ('S'::('t'::('r'::('i'::('n'::('g'::[]))))))
       else if eqb1 name ('b'::('o'::('o'::('l'::('e'::('a'::('n'::[])))))))
            then AstRef ('B'::('o'::('o'::('l'::[]))))
            else AstRef name
| TsArrow (params, ret0) ->
  let ret_ty = ts_to_chester ret0 in
  let rec build_pi = function
  | [] -> ret_ty
  | arg :: rest ->
    AstPi (arg, (AstRef ('A'::('n'::('y'::[])))), (build_pi rest), [])
  in build_pi params
| _ -> AstRef ('A'::('n'::('y'::[])))

(** val go_to_chester : goAST -> aST **)

let rec go_to_chester = function
| GoIdentifier name ->
  if eqb1 name ('i'::('n'::('t'::[])))
  then AstRef ('I'::('n'::('t'::[])))
  else if eqb1 name ('s'::('t'::('r'::('i'::('n'::('g'::[]))))))
       then AstRef ('S'::('t'::('r'::('i'::('n'::('g'::[]))))))
       else if eqb1 name ('b'::('o'::('o'::('l'::[]))))
            then AstRef ('B'::('o'::('o'::('l'::[]))))
            else AstRef name
| GoFuncLiteral (params, ret0) ->
  let ret_ty = go_to_chester ret0 in
  let rec build_pi = function
  | [] -> ret_ty
  | arg :: rest ->
    AstPi (arg, (AstRef ('A'::('n'::('y'::[])))), (build_pi rest), [])
  in build_pi params
| _ -> AstRef ('A'::('n'::('y'::[])))
