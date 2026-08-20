From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.
Require Import Chester.CST.

(* We define a simple Token type for our universal syntax *)
Inductive Token : Type :=
  | TokSymbol : string -> Span -> Token
  | TokString : string -> Span -> Token
  | TokInt : string -> Span -> Token
  | TokComment : string -> Span -> Token
  | TokLParen : Span -> Token
  | TokRParen : Span -> Token
  | TokLBrace : Span -> Token
  | TokRBrace : Span -> Token
  | TokLBracket : Span -> Token
  | TokRBracket : Span -> Token.

(* Extract span from a token *)
Definition token_span (t : Token) : Span :=
  match t with
  | TokSymbol _ s => s
  | TokString _ s => s
  | TokInt _ s => s
  | TokComment _ s => s
  | TokLParen s => s
  | TokRParen s => s
  | TokLBrace s => s
  | TokRBrace s => s
  | TokLBracket s => s
  | TokRBracket s => s
  end.

(* Boolean checks for closing tokens *)
Definition is_rparen (t : Token) : bool :=
  match t with TokRParen _ => true | _ => false end.

Definition is_rbracket (t : Token) : bool :=
  match t with TokRBracket _ => true | _ => false end.

Definition is_rbrace (t : Token) : bool :=
  match t with TokRBrace _ => true | _ => false end.

(* Result of a parsing step: either an error or a parsed value plus remaining tokens *)
Inductive ParseResult (A : Type) : Type :=
  | ParseOk : A -> list Token -> ParseResult A
  | ParseErr : string -> ParseResult A.

Arguments ParseOk {A}.
Arguments ParseErr {A}.

(* A simplified recursive-descent parser using fuel for termination. *)
Section ParserLogic.
  
  (* We use fuel to convince Coq of termination since parsing consumes tokens but proving structural decrease mutually can be tedious. *)
  Fixpoint parse_cst (fuel : nat) (tokens : list Token) : ParseResult CST :=
    match fuel with
    | 0 => ParseErr "Out of fuel"
    | S fuel' =>
        match tokens with
        | [] => ParseErr "Unexpected end of input"
        | t :: rest =>
            match t with
            | TokSymbol name s => ParseOk (Symbol name s) rest
            | TokString val s => ParseOk (StringLiteral val s) rest
            | TokInt val s => ParseOk (IntegerLiteral val s) rest
            | TokComment text s => ParseOk (CommentCST text s) rest
            
            | TokLParen s_start =>
                (* Parse a Tuple *)
                match parse_sequence fuel' rest is_rparen with
                | ParseOk (elements, s_end) rest' =>
                    ParseOk (Tuple elements (combine_span s_start s_end)) rest'
                | ParseErr e => ParseErr e
                end
                
            | TokLBracket s_start =>
                (* Parse a ListLiteral *)
                match parse_sequence fuel' rest is_rbracket with
                | ParseOk (elements, s_end) rest' =>
                    ParseOk (ListLiteral elements (combine_span s_start s_end)) rest'
                | ParseErr e => ParseErr e
                end
                
            | TokLBrace s_start =>
                (* Parse a Block (simplified without tail separation for this example) *)
                match parse_sequence fuel' rest is_rbrace with
                | ParseOk (elements, s_end) rest' =>
                    ParseOk (Block elements (Tuple [] (combine_span s_start s_end)) (combine_span s_start s_end)) rest'
                | ParseErr e => ParseErr e
                end
                
            | _ => ParseErr "Unexpected token"
            end
        end
    end
    
  (* Parses a sequence of CSTs until it hits the end_token *)
  with parse_sequence (fuel : nat) (tokens : list Token) (end_token_type : Token -> bool) : ParseResult (list CST * Span) :=
    match fuel with
    | 0 => ParseErr "Out of fuel"
    | S fuel' =>
        match tokens with
        | [] => ParseErr "Unexpected end of input, missing closing token"
        | t :: rest =>
            if end_token_type t then
              ParseOk ([], token_span t) rest
            else
              match parse_cst fuel' tokens with
              | ParseOk first_cst rest' =>
                  match parse_sequence fuel' rest' end_token_type with
                  | ParseOk (rest_csts, end_s) rest'' =>
                      ParseOk (first_cst :: rest_csts, end_s) rest''
                  | ParseErr e => ParseErr e
                  end
              | ParseErr e => ParseErr e
              end
        end
    end.
    
End ParserLogic.

(* Wrapper that supplies some maximum fuel based on token list length *)
Definition parse (tokens : list Token) : ParseResult CST :=
  parse_cst (length tokens * 2 + 10) tokens.
