open Compiler_lib.Compiler
open Chester_frontend

let string_of_char_list chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let preamble =
  "const Unit = {};\n\
   const prim__string_eq = (a, b) => a === b;\n\
   const prim__list_length = (l) => l.length;\n\
   const prim__int_eq = (a, b) => a === b;\n\
   const prim__list_make = (len, f) => Array.from({length: len}, (_, i) => \
   f(i));\n\
   const prim__int_sub = (a, b) => a - b;\n\
   const prim__list_get = (l, i) => l[i];\n\
   const prim__int_add = (a, b) => a + b;\n\
   const prim__int_lt = (a, b) => a < b;\n\
   const prim__string_length = (s) => s.length;\n\
   const prim__string_substring = (s, start, end) => s.substring(start, end);\n\
   const prim__string_concat = (s1, s2) => s1 + s2;\n\
   const prim__list_empty = () => [];\n\
   const prim__int_mul = (a, b) => a * b;\n\
   const prim__int_div = (a, b) => Math.floor(a / b);\n\
   const prim__int_mod = (a, b) => ((a % b) + b) % b;\n\
   const prim__int_gt = (a, b) => a > b;\n\
   const prim__int_ge = (a, b) => a >= b;\n\
   const prim__int_le = (a, b) => a <= b;\n\
   const prim__int_neg = (a) => -a;\n\
   const prim__int_to_string = (n) => String(n);\n\
   let __chester_caps = [];\n\
   /* Multi-shot resume via answer-stream replay (Effekt/Koka-style forking):\n\
      each resume(v) re-runs the handle body, replaying prior answers and\n\
      supplying v at the current perform. No AST CPS required. */\n\
   const __chester_handle = (label, bodyFn, handlers) => {\n\
     const run = (answers) => {\n\
       let ai = 0;\n\
       const frame = {\n\
         label,\n\
         handlers,\n\
         take: () => {\n\
           if (ai < answers.length) return { ok: true, v: answers[ai++] };\n\
           return { ok: false };\n\
         },\n\
         fork: (v) => run(answers.slice(0, ai).concat([v]))\n\
       };\n\
       __chester_caps.push(frame);\n\
       try { return bodyFn(); }\n\
       catch (e) {\n\
         if (e && e.__chester_handled === frame) return e.result;\n\
         throw e;\n\
       }\n\
       finally { __chester_caps.pop(); }\n\
     };\n\
     return run([]);\n\
   };\n\
   const __chester_perform = (op, args) => {\n\
     for (let i = __chester_caps.length - 1; i >= 0; i--) {\n\
       const frame = __chester_caps[i];\n\
       const h = frame.handlers[op];\n\
       if (!h) continue;\n\
       const got = frame.take();\n\
       if (got.ok) return got.v;\n\
       const resume = (v) => frame.fork(v);\n\
       let fn = h;\n\
       for (let j = 0; j < args.length; j++) fn = fn(args[j]);\n\
       const result = fn(resume);\n\
       throw { __chester_handled: frame, result };\n\
     }\n\
     throw new Error(\"Unhandled effect operation: \" + op);\n\
   };\n\
   /* Koka-lite evidence: snapshot {label, handlers} for open-row evidence vectors.
      Reinstall via nested handle so multi-shot replay still works. */\n\
   const __chester_evidence = (labels) => {\n\
     const ev = [];\n\
     for (let li = 0; li < labels.length; li++) {\n\
       const lab = labels[li];\n\
       for (let i = __chester_caps.length - 1; i >= 0; i--) {\n\
         if (__chester_caps[i].label === lab) {\n\
           ev.push({ label: lab, handlers: __chester_caps[i].handlers });\n\
           break;\n\
         }\n\
       }\n\
     }\n\
     return ev;\n\
   };\n\
   const __chester_with_evidence = (ev, bodyFn) => {\n\
     let i = 0;\n\
     const go = () => {\n\
       if (i >= ev.length) return bodyFn();\n\
       const { label, handlers } = ev[i++];\n\
       return __chester_handle(label, go, handlers);\n\
     };\n\
     return go();\n\
   };\n\
   const __chester_box = (labels, bodyFn) => {\n\
     const ev = __chester_evidence(labels);\n\
     return () => __chester_with_evidence(ev, bodyFn);\n\
   };\n\
   const int_add = prim__int_add;\n\
   const int_eq = prim__int_eq;\n\
   let _elab_state = null;\n\
   const prim__get_elab_state = () => _elab_state;\n\
   const prim__put_elab_state = (s) => { _elab_state = s; return Unit; };\n\
   const ParseResult = (result, rest) => ({result, rest});\n\
   const Span = (start, end) => ({start, end});\n\
   const lex = (s) => [{kind: \"Whitespace\"}, {kind: \"Id\", text: \"let\"}];\n"

let[@warning "-32"] go_preamble =
  {|
var __chester_caps []map[string]interface{}

type __chesterHandled struct {
  frame  map[string]interface{}
  result interface{}
}

func __chester_handle(label string, bodyFn func() interface{}, handlers map[string]interface{}) interface{} {
  var run func(answers []interface{}) interface{}
  run = func(answers []interface{}) (res interface{}) {
    ai := 0
    frame := map[string]interface{}{"label": label, "handlers": handlers}
    frame["take"] = func() (bool, interface{}) {
      if ai < len(answers) {
        v := answers[ai]
        ai++
        return true, v
      }
      return false, nil
    }
    frame["fork"] = func(v interface{}) interface{} {
      next := append(append([]interface{}{}, answers[:ai]...), v)
      return run(next)
    }
    __chester_caps = append(__chester_caps, frame)
    defer func() { __chester_caps = __chester_caps[:len(__chester_caps)-1] }()
    defer func() {
      if r := recover(); r != nil {
        if h, ok := r.(__chesterHandled); ok && h.frame == frame {
          res = h.result
          return
        }
        panic(r)
      }
    }()
    return bodyFn()
  }
  return run(nil)
}

func __chester_perform(op string, args []interface{}) interface{} {
  for i := len(__chester_caps) - 1; i >= 0; i-- {
    frame := __chester_caps[i]
    handlers := frame["handlers"].(map[string]interface{})
    h, ok := handlers[op]
    if !ok {
      continue
    }
    take := frame["take"].(func() (bool, interface{}))
    if ok2, v := take(); ok2 {
      return v
    }
    resume := func(v interface{}) interface{} {
      return frame["fork"].(func(interface{}) interface{})(v)
    }
    fn := h
    for _, a := range args {
      fn = fn.(func(interface{}) interface{})(a)
    }
    result := fn.(func(interface{}) interface{})(resume)
    panic(__chesterHandled{frame: frame, result: result})
  }
  panic("Unhandled effect operation: " + op)
}

func __chester_evidence(labels []interface{}) []map[string]interface{} {
  var ev []map[string]interface{}
  for _, lab := range labels {
    ls := lab.(string)
    for i := len(__chester_caps) - 1; i >= 0; i-- {
      if __chester_caps[i]["label"] == ls {
        ev = append(ev, map[string]interface{}{
          "label":    ls,
          "handlers": __chester_caps[i]["handlers"],
        })
        break
      }
    }
  }
  return ev
}

func __chester_with_evidence(ev []map[string]interface{}, bodyFn func() interface{}) interface{} {
  var go func(i int) interface{}
  go = func(i int) interface{} {
    if i >= len(ev) {
      return bodyFn()
    }
    lab := ev[i]["label"].(string)
    handlers := ev[i]["handlers"].(map[string]interface{})
    return __chester_handle(lab, func() interface{} { return go(i + 1) }, handlers)
  }
  return go(0)
}

func __chester_box(labels []interface{}, bodyFn func() interface{}) interface{} {
  ev := __chester_evidence(labels)
  return func() interface{} { return __chester_with_evidence(ev, bodyFn) }
}
|}

let process_file filename oc state =
  let ch = open_in filename in
  let len = in_channel_length ch in
  let buf = Bytes.create len in
  really_input ch buf 0 len;
  close_in ch;
  let source = Bytes.to_string buf in

  print_endline ("\n[Parsing " ^ filename ^ "]");
  let tokens = Lexer.tokenize filename source in
  let cst = parse tokens in

  print_endline ("\n[Expanding " ^ filename ^ "]");
  let expanded_cst = expand_cst_top cst in

  print_endline (string_of_char_list (format_cst 100 0 expanded_cst));
  print_endline ("\n[Elaborating & TypeChecking " ^ filename ^ "]");
  match elaborate_top [] expanded_cst None state with
  | Inr (msg, _) ->
      print_endline ("Type Error: " ^ string_of_char_list msg);
      print_endline (string_of_char_list (format_cst 100 0 expanded_cst));
      exit 1
  | Inl ((ast, _), state') ->
      print_endline ("\n[Emitting TypeScript for " ^ filename ^ "]");
      let ts_ast = emit_ts ast in
      let ts_code = string_of_char_list (stringify_ts_stmt ts_ast) in
      output_string oc ts_code;
      state'

let () =
  print_endline "Chester Bootstrapper";
  if Array.length Sys.argv > 1 then begin
    let out_dir = "out" in
    if not (Sys.file_exists out_dir) then Sys.mkdir out_dir 0o755;
    let out_file = Filename.concat out_dir "compiler.ts" in
    let oc = open_out out_file in
    output_string oc preamble;
    let state = ref init_elab_state in
    for i = 1 to Array.length Sys.argv - 1 do
      state := process_file Sys.argv.(i) oc !state
    done;
    close_out oc;
    print_endline ("\nSuccessfully emitted to " ^ out_file)
  end
  else print_endline "Usage: main.exe <file.chester> [file2.chester ...]"
