open Compiler_lib.Compiler

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_alphanum c = is_alpha c || is_digit c

let char_list_of_string s =
  let rec aux i acc = if i < 0 then acc else aux (i - 1) (s.[i] :: acc) in
  aux (String.length s - 1) []

let make_span file_name start_idx _end_idx =
  let make_pos idx =
    {
      index = { unicode = idx; utf16 = idx };
      line = 0;
      column = { unicode = idx; utf16 = idx };
    }
  in
  {
    file_name = char_list_of_string file_name;
    range = { start_pos = make_pos start_idx; end_pos = make_pos _end_idx };
    context = [];
  }

let tokenize filename source =
  let len = String.length source in
  let rec aux i acc =
    if i >= len then List.rev (TokEOF (make_span filename i i) :: acc)
    else
      let c = source.[i] in
      if c = ' ' || c = '\n' || c = '\r' || c = '\t' then aux (i + 1) acc
      else if c = '/' && i + 1 < len && source.[i + 1] = '/' then
        let start_idx = i in
        let rec consume_comment j =
          if j >= len || source.[j] = '\n' then j else consume_comment (j + 1)
        in
        let _end_idx = consume_comment i in
        let text = String.sub source start_idx (_end_idx - start_idx) in
        aux _end_idx
          (TokComment
             (char_list_of_string text, make_span filename start_idx _end_idx)
          :: acc)
      else if is_alpha c then
        let start_idx = i in
        let rec consume_id j =
          if j < len && is_alphanum source.[j] then consume_id (j + 1) else j
        in
        let _end_idx = consume_id i in
        let text = String.sub source start_idx (_end_idx - start_idx) in
        aux _end_idx
          (TokId
             (char_list_of_string text, make_span filename start_idx _end_idx)
          :: acc)
      else if is_digit c then
        let start_idx = i in
        let rec consume_int j =
          if j < len && is_digit source.[j] then consume_int (j + 1) else j
        in
        let _end_idx = consume_int i in
        let text = String.sub source start_idx (_end_idx - start_idx) in
        aux _end_idx
          (TokInt
             (char_list_of_string text, make_span filename start_idx _end_idx)
          :: acc)
      else if c = '"' then
        let start_idx = i in
        let rec consume_str j =
          if j >= len then j
          else if source.[j] = '"' then j + 1
          else consume_str (j + 1)
        in
        let _end_idx = consume_str (i + 1) in
        let text =
          String.sub source (start_idx + 1) (_end_idx - start_idx - 2)
        in
        aux _end_idx
          (TokStr
             (char_list_of_string text, make_span filename start_idx _end_idx)
          :: acc)
      else if c = '=' && i + 1 < len && source.[i + 1] = '=' then
        aux (i + 2)
          (TokSym (char_list_of_string "==", make_span filename i (i + 2))
          :: acc)
      else if c = '=' && i + 1 < len && source.[i + 1] = '>' then
        aux (i + 2)
          (TokSym (char_list_of_string "=>", make_span filename i (i + 2))
          :: acc)
      else if c = '-' && i + 1 < len && source.[i + 1] = '>' then
        aux (i + 2)
          (TokSym (char_list_of_string "->", make_span filename i (i + 2))
          :: acc)
      else
        aux (i + 1)
          (TokSym
             ( char_list_of_string (String.make 1 c),
               make_span filename i (i + 1) )
          :: acc)
  in
  aux 0 []
