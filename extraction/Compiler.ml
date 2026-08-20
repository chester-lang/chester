
(** val add : int -> int -> int **)

let rec add = (+)

(** val eqb : int -> int -> bool **)

let rec eqb n m =
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
    n

module Nat =
 struct
 end

(** val eqb0 : char list -> char list -> bool **)

let rec eqb0 s1 s2 =
  match s1 with
  | [] -> (match s2 with
           | [] -> true
           | _::_ -> false)
  | c1::s1' ->
    (match s2 with
     | [] -> false
     | c2::s2' -> if (=) c1 c2 then eqb0 s1' s2' else false)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

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

(** val empty_state : solverState **)

let empty_state _ =
  Unsolved

(** val update_state :
    metaId -> (effectSet, effectSet) metaState -> solverState -> solverState **)

let update_state id new_state st x =
  if (=) x id then new_state else st x

(** val add_effect_constraint :
    metaId -> effectRef -> solverState -> solverState **)

let add_effect_constraint id eff st =
  match st id with
  | Unsolved -> update_state id (Constrained (eff :: [])) st
  | Constrained effs -> update_state id (Constrained (eff :: effs)) st
  | Solved _ -> st

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

(** val token_span : token -> span **)

let token_span = function
| TokSymbol (_, s) -> s
| TokString (_, s) -> s
| TokInt (_, s) -> s
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
            ParseOk ((Block (elements, [], (combine_span s_start s_end))),
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
  let (k, v) = p in if eqb0 name k then Some v else lookup_type name rest

type 'a tyResult =
| TyOk of 'a
| TyErr of char list

(** val eq_ast : aST -> aST -> bool **)

let rec eq_ast t1 t2 =
  match t1 with
  | AstRef n1 -> (match t2 with
                  | AstRef n2 -> eqb0 n1 n2
                  | _ -> false)
  | AstStringLit s1 ->
    (match t2 with
     | AstStringLit s2 -> eqb0 s1 s2
     | _ -> false)
  | AstIntLit n1 -> (match t2 with
                     | AstIntLit n2 -> eqb n1 n2
                     | _ -> false)
  | AstPi (n1, ty1, ret1, _) ->
    (match t2 with
     | AstPi (n2, ty2, ret2, _) ->
       (&&) ((&&) (eqb0 n1 n2) (eq_ast ty1 ty2)) (eq_ast ret1 ret2)
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
  | AstLet (_, _, _) ->
    TyErr
      ('L'::('e'::('t'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('c'::('h'::('e'::('c'::('k'::('e'::('r'::[]))))))))))))))))))))))))))))))
  | AstIf (_, _, _) ->
    TyErr
      ('I'::('f'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('c'::('h'::('e'::('c'::('k'::('e'::('r'::[])))))))))))))))))))))))))))))
  | AstDef (_, _, _, _, _) ->
    TyErr
      ('D'::('e'::('f'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('c'::('h'::('e'::('c'::('k'::('e'::('r'::[]))))))))))))))))))))))))))))))
  | AstEnum (_, _, _) ->
    TyErr
      ('E'::('n'::('u'::('m'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('c'::('h'::('e'::('c'::('k'::('e'::('r'::[])))))))))))))))))))))))))))))))
  | AstRecord (_, _, _) ->
    TyErr
      ('R'::('e'::('c'::('o'::('r'::('d'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('c'::('h'::('e'::('c'::('k'::('e'::('r'::[])))))))))))))))))))))))))))))))))
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

(** val constrain_effect : metaId -> effectRef -> unit elabM **)

let constrain_effect id eff =
  bind get_solver (fun st -> put_solver (add_effect_constraint id eff st))

(** val unify : aST -> aST -> unit elabM **)

let rec unify t1 t2 =
  match t1 with
  | AstRef n1 ->
    (match t2 with
     | AstRef n2 ->
       if eqb0 n1 n2
       then ret ()
       else throw
              ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(':'::(' '::('n'::('a'::('m'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))
     | _ ->
       throw
         ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))
  | AstMeta m1 ->
    (match t2 with
     | AstMeta m2 ->
       if (=) m1 m2
       then ret ()
       else throw
              ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('o'::('f'::(' '::('t'::('w'::('o'::(' '::('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::(' '::('m'::('e'::('t'::('a'::('s'::(' '::('n'::('o'::('t'::(' '::('f'::('u'::('l'::('l'::('y'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       throw
         ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))
  | _ ->
    throw
      ('U'::('n'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::[])))))))))))))))))))))))))))))))))))

(** val elaborate : typeEnv -> cST -> aST option -> (aST * aST) elabM **)

let rec elaborate env expr expected =
  match expr with
  | Symbol (name, _) ->
    (match lookup_type name env with
     | Some ty ->
       (match expected with
        | Some expTy ->
          bind (unify ty expTy) (fun _ -> ret ((AstRef name), ty))
        | None -> ret ((AstRef name), ty))
     | None ->
       throw
         (append
           ('U'::('n'::('b'::('o'::('u'::('n'::('d'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(':'::(' '::[]))))))))))))))))))
           name))
  | StringLiteral (s, _) -> ret ((AstStringLit s), stringType)
  | SeqOf (l, _) ->
    (match l with
     | [] ->
       throw
         ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
     | c :: l0 ->
       (match c with
        | Symbol (s0, _) ->
          (match s0 with
           | [] ->
             throw
               ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
           | a::s2 ->
             (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
               (fun b b0 b1 b2 b3 b4 b5 b6 ->
               if b
               then if b0
                    then throw
                           ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                    else if b1
                         then throw
                                ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                         else if b2
                              then if b3
                                   then throw
                                          ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                   else if b4
                                        then if b5
                                             then if b6
                                                  then throw
                                                         ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                  else (match s2 with
                                                        | [] ->
                                                          throw
                                                            ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                        | a0::s3 ->
                                                          (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                            (fun b7 b8 b9 b10 b11 b12 b13 b14 ->
                                                            if b7
                                                            then if b8
                                                                 then 
                                                                   if b9
                                                                   then 
                                                                    if b10
                                                                    then 
                                                                    if b11
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b12
                                                                    then 
                                                                    if b13
                                                                    then 
                                                                    if b14
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    (match s3 with
                                                                    | [] ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    | a1::s4 ->
                                                                    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                                    (fun b15 b16 b17 b18 b19 b20 b21 b22 ->
                                                                    if b15
                                                                    then 
                                                                    if b16
                                                                    then 
                                                                    if b17
                                                                    then 
                                                                    if b18
                                                                    then 
                                                                    if b19
                                                                    then 
                                                                    if b20
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b21
                                                                    then 
                                                                    if b22
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    (match s4 with
                                                                    | [] ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    | a2::s5 ->
                                                                    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                                    (fun b23 b24 b25 b26 b27 b28 b29 b30 ->
                                                                    if b23
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b24
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b25
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b26
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b27
                                                                    then 
                                                                    if b28
                                                                    then 
                                                                    if b29
                                                                    then 
                                                                    if b30
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    (match s5 with
                                                                    | [] ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    | a3::s6 ->
                                                                    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                                    (fun b31 b32 b33 b34 b35 b36 b37 b38 ->
                                                                    if b31
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b32
                                                                    then 
                                                                    if b33
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b34
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b35
                                                                    then 
                                                                    if b36
                                                                    then 
                                                                    if b37
                                                                    then 
                                                                    if b38
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    (match s6 with
                                                                    | [] ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    | a4::s7 ->
                                                                    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                                    (fun b39 b40 b41 b42 b43 b44 b45 b46 ->
                                                                    if b39
                                                                    then 
                                                                    if b40
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b41
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b42
                                                                    then 
                                                                    if b43
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b44
                                                                    then 
                                                                    if b45
                                                                    then 
                                                                    if b46
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    (match s7 with
                                                                    | [] ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    | a5::s8 ->
                                                                    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                                    (fun b47 b48 b49 b50 b51 b52 b53 b54 ->
                                                                    if b47
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b48
                                                                    then 
                                                                    if b49
                                                                    then 
                                                                    if b50
                                                                    then 
                                                                    if b51
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b52
                                                                    then 
                                                                    if b53
                                                                    then 
                                                                    if b54
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    (match s8 with
                                                                    | [] ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    | a6::s9 ->
                                                                    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                                    (fun b55 b56 b57 b58 b59 b60 b61 b62 ->
                                                                    if b55
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b56
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b57
                                                                    then 
                                                                    if b58
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if b59
                                                                    then 
                                                                    if b60
                                                                    then 
                                                                    if b61
                                                                    then 
                                                                    if b62
                                                                    then 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    (match s9 with
                                                                    | [] ->
                                                                    (match l0 with
                                                                    | [] ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    | arg :: l1 ->
                                                                    (match l1 with
                                                                    | [] ->
                                                                    bind
                                                                    (elaborate
                                                                    env arg
                                                                    (Some
                                                                    stringType))
                                                                    (fun res ->
                                                                    let (
                                                                    ast_arg, _) =
                                                                    res
                                                                    in
                                                                    bind
                                                                    fresh_meta
                                                                    (fun eff_meta_ast ->
                                                                    match eff_meta_ast with
                                                                    | AstMeta m ->
                                                                    bind
                                                                    (constrain_effect
                                                                    m
                                                                    (BuiltinEffect
                                                                    ('i'::('o'::[]))))
                                                                    (fun _ ->
                                                                    ret
                                                                    ((AstApp
                                                                    ((AstRef
                                                                    ('i'::('o'::('_'::('p'::('r'::('i'::('n'::('t'::[]))))))))),
                                                                    (ast_arg :: []))),
                                                                    intType))
                                                                    | _ ->
                                                                    throw
                                                                    ('I'::('n'::('t'::('e'::('r'::('n'::('a'::('l'::(' '::('e'::('r'::('r'::('o'::('r'::[]))))))))))))))))
                                                                    | _ :: _ ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))
                                                                    | _::_ ->
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
                                                                    a6)
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
                                                                    a5)
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
                                                                    a4)
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
                                                                    a3)
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
                                                                    a2)
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
                                                                    a1)
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                    else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                   else 
                                                                    throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                                 else 
                                                                   throw
                                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                                            else throw
                                                                   ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
                                                            a0)
                                             else throw
                                                    ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                        else throw
                                               ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                              else throw
                                     ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
               else throw
                      ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
               a)
        | _ ->
          throw
            ('U'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('C'::('S'::('T'::(' '::('n'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))
  | LetCST (_, _, _, _) ->
    throw
      ('L'::('e'::('t'::('C'::('S'::('T'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('o'::('r'::[]))))))))))))))))))))))))))))))))))))
  | IfCST (_, _, _, _) ->
    throw
      ('I'::('f'::('C'::('S'::('T'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('o'::('r'::[])))))))))))))))))))))))))))))))))))
  | DefCST (_, _, _, _, _, _) ->
    throw
      ('D'::('e'::('f'::('C'::('S'::('T'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('o'::('r'::[]))))))))))))))))))))))))))))))))))))
  | EnumCST (_, _, _, _) ->
    throw
      ('E'::('n'::('u'::('m'::('C'::('S'::('T'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('o'::('r'::[])))))))))))))))))))))))))))))))))))))
  | RecordCST (_, _, _, _) ->
    throw
      ('R'::('e'::('c'::('o'::('r'::('d'::('C'::('S'::('T'::(' '::('n'::('o'::('t'::(' '::('i'::('m'::('p'::('l'::('e'::('m'::('e'::('n'::('t'::('e'::('d'::(' '::('i'::('n'::(' '::('e'::('l'::('a'::('b'::('o'::('r'::('a'::('t'::('o'::('r'::[])))))))))))))))))))))))))))))))))))))))
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
   | TsNumberLiteral n -> n
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
   | GoIntLiteral n -> n
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
   | AstIntLit n -> TsNumberLiteral (nat_to_string n)
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
   | AstLet (name, _, _) ->
     TsRaw
       (append ('/'::('*'::(' '::('l'::('e'::('t'::(' '::[])))))))
         (append name (' '::('*'::('/'::[])))))
   | AstIf (_, _, _) ->
     TsRaw ('/'::('*'::(' '::('i'::('f'::(' '::('*'::('/'::[]))))))))
   | AstDef (name, _, _, _, _) ->
     TsRaw
       (append ('/'::('*'::(' '::('d'::('e'::('f'::(' '::[])))))))
         (append name (' '::('*'::('/'::[])))))
   | AstEnum (name, _, _) ->
     TsRaw
       (append ('/'::('*'::(' '::('e'::('n'::('u'::('m'::(' '::[]))))))))
         (append name (' '::('*'::('/'::[])))))
   | AstRecord (name, _, _) ->
     TsRaw
       (append
         ('/'::('*'::(' '::('r'::('e'::('c'::('o'::('r'::('d'::(' '::[]))))))))))
         (append name (' '::('*'::('/'::[])))))
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
   | AstIntLit n -> GoIntLiteral (nat_to_string n)
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
   | AstLet (name, _, _) ->
     GoRaw
       (append ('/'::('*'::(' '::('l'::('e'::('t'::(' '::[])))))))
         (append name (' '::('*'::('/'::[])))))
   | AstIf (_, _, _) ->
     GoRaw ('/'::('*'::(' '::('i'::('f'::(' '::('*'::('/'::[]))))))))
   | AstDef (name, _, _, _, _) ->
     GoRaw
       (append ('/'::('*'::(' '::('d'::('e'::('f'::(' '::[])))))))
         (append name (' '::('*'::('/'::[])))))
   | AstEnum (name, _, _) ->
     GoRaw
       (append ('/'::('*'::(' '::('e'::('n'::('u'::('m'::(' '::[]))))))))
         (append name (' '::('*'::('/'::[])))))
   | AstRecord (name, _, _) ->
     GoRaw
       (append
         ('/'::('*'::(' '::('r'::('e'::('c'::('o'::('r'::('d'::(' '::[]))))))))))
         (append name (' '::('*'::('/'::[])))))
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

(** val ts_to_chester : typeScriptAST -> aST **)

let rec ts_to_chester = function
| TsIdentifier name ->
  if eqb0 name ('n'::('u'::('m'::('b'::('e'::('r'::[]))))))
  then AstRef ('I'::('n'::('t'::[])))
  else if eqb0 name ('s'::('t'::('r'::('i'::('n'::('g'::[]))))))
       then AstRef ('S'::('t'::('r'::('i'::('n'::('g'::[]))))))
       else if eqb0 name ('b'::('o'::('o'::('l'::('e'::('a'::('n'::[])))))))
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
  if eqb0 name ('i'::('n'::('t'::[])))
  then AstRef ('I'::('n'::('t'::[])))
  else if eqb0 name ('s'::('t'::('r'::('i'::('n'::('g'::[]))))))
       then AstRef ('S'::('t'::('r'::('i'::('n'::('g'::[]))))))
       else if eqb0 name ('b'::('o'::('o'::('l'::[]))))
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
