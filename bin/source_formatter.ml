type token =
  | Word of string
  | String_lit of string
  | Comment of string * bool
  | Sym of string
  | Eof

let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false

let is_word_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let tokenize source =
  let len = String.length source in
  let rec loop i line_has_code acc =
    if i >= len then List.rev (Eof :: acc)
    else
      match source.[i] with
      | c when is_space c ->
          loop (i + 1)
            (if c = '\n' || c = '\r' then false else line_has_code)
            acc
      | '/' when i + 1 < len && source.[i + 1] = '/' ->
          let start = i in
          let rec consume j =
            if j >= len || source.[j] = '\n' then j else consume (j + 1)
          in
          let stop = consume i in
          loop stop false
            (Comment (String.sub source start (stop - start), line_has_code)
            :: acc)
      | '"' ->
          let start = i in
          let rec consume escaped j =
            if j >= len then j
            else if escaped then consume false (j + 1)
            else if source.[j] = '\\' then consume true (j + 1)
            else if source.[j] = '"' then j + 1
            else consume false (j + 1)
          in
          let stop = consume false (i + 1) in
          loop stop true
            (String_lit (String.sub source start (stop - start)) :: acc)
      | c when is_word_char c ->
          let start = i in
          let rec consume j =
            if j < len && is_word_char source.[j] then consume (j + 1) else j
          in
          let stop = consume (i + 1) in
          loop stop true (Word (String.sub source start (stop - start)) :: acc)
      | '=' when i + 1 < len && source.[i + 1] = '>' ->
          loop (i + 2) true (Sym "=>" :: acc)
      | '-' when i + 1 < len && source.[i + 1] = '>' ->
          loop (i + 2) true (Sym "->" :: acc)
      | '=' when i + 1 < len && source.[i + 1] = '=' ->
          loop (i + 2) true (Sym "==" :: acc)
      | '!' when i + 1 < len && source.[i + 1] = '=' ->
          loop (i + 2) true (Sym "!=" :: acc)
      | '<' when i + 1 < len && source.[i + 1] = '=' ->
          loop (i + 2) true (Sym "<=" :: acc)
      | '>' when i + 1 < len && source.[i + 1] = '=' ->
          loop (i + 2) true (Sym ">=" :: acc)
      | '&' when i + 1 < len && source.[i + 1] = '&' ->
          loop (i + 2) true (Sym "&&" :: acc)
      | '|' when i + 1 < len && source.[i + 1] = '|' ->
          loop (i + 2) true (Sym "||" :: acc)
      | c -> loop (i + 1) true (Sym (String.make 1 c) :: acc)
  in
  loop 0 false []

let token_text = function
  | Word text | String_lit text | Comment (text, _) | Sym text -> text
  | Eof -> ""

let adjacent prev curr =
  match (prev, curr) with
  | _, Sym ("," | ";" | ")" | "]" | "." | ":") -> true
  | Sym ("(" | "[" | "." | "\\"), _ -> true
  | Word "case", Sym "(" -> false
  | (Word _ | String_lit _ | Sym (")" | "]")), Sym ("(" | "[") -> true
  | _ -> false

let starts_with_blank_line tokens =
  match tokens with
  | Word ("enum" | "effect" | "macro" | "extension") :: _ -> true
  | Comment (_, false) :: _ -> true
  | Comment _ :: Word ("enum" | "effect" | "macro" | "extension") :: _ -> true
  | _ -> false

let pop_to_matching brace_stack =
  match brace_stack with _ :: rest -> rest | [] -> []

let format_source source =
  let tokens = tokenize source in
  let buf = Buffer.create (String.length source) in
  let indent = ref 0 in
  let line_start = ref true in
  let line_tokens = ref [] in
  let prev = ref None in
  let brace_stack = ref [] in
  let blank_pending = ref false in
  let wrote_any_line = ref false in
  let spaces n = String.make (max 0 n) ' ' in
  let write_indent () =
    if !line_start then begin
      if !blank_pending && !wrote_any_line then Buffer.add_char buf '\n';
      blank_pending := false;
      Buffer.add_string buf (spaces (!indent * 2));
      line_start := false
    end
  in
  let newline () =
    if not !line_start then begin
      Buffer.add_char buf '\n';
      line_start := true;
      line_tokens := [];
      prev := None;
      wrote_any_line := true
    end
  in
  let add_token tok =
    write_indent ();
    (match !prev with
    | Some p when not (adjacent p tok) -> Buffer.add_char buf ' '
    | _ -> ());
    Buffer.add_string buf (token_text tok);
    line_tokens := !line_tokens @ [ tok ];
    prev := Some tok
  in
  let set_blank_pending rest =
    blank_pending := !indent = 0 && starts_with_blank_line rest
  in
  let rec loop = function
    | [] | [ Eof ] -> ()
    | Eof :: _ -> ()
    | (Comment _ as tok) :: rest ->
        add_token tok;
        newline ();
        loop rest
    | (Sym "{" as tok) :: rest ->
        add_token tok;
        brace_stack := !line_tokens :: !brace_stack;
        incr indent;
        newline ();
        loop rest
    | (Sym "}" as tok) :: rest -> (
        if not !line_start then newline ();
        indent := max 0 (!indent - 1);
        add_token tok;
        brace_stack := pop_to_matching !brace_stack;
        match rest with
        | Sym ";" :: rest' ->
            add_token (Sym ";");
            newline ();
            set_blank_pending rest';
            loop rest'
        | Word "else" :: rest' ->
            add_token (Word "else");
            loop rest'
        | (Sym (")" | "]" | ",") as next) :: rest' ->
            add_token next;
            loop rest'
        | _ ->
            newline ();
            set_blank_pending rest;
            loop rest)
    | (Sym ";" as tok) :: (Comment (_, true) as comment) :: rest ->
        add_token tok;
        add_token comment;
        newline ();
        set_blank_pending rest;
        loop rest
    | (Sym ";" as tok) :: rest ->
        add_token tok;
        newline ();
        set_blank_pending rest;
        loop rest
    | tok :: rest ->
        add_token tok;
        loop rest
  in
  loop tokens;
  if not !line_start then newline ();
  Buffer.contents buf
