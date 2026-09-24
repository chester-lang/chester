(* Shared effect runtimes for TS and Go backends (used by main + tests). *)

let ts_primitives =
  "const Unit = {};\n\
   const prim__string_eq = (a, b) => a === b;\n\
   const prim__list_length = (l) => l.length;\n\
   const prim__int_eq = (a, b) => a === b;\n\
   const prim__cst_tag = (cst) => cst._tag || \"unknown_tag\";\n\
   const prim__list_advance = (l) => l.slice(1);\n\
   const prim__list_filter = (l, pred) => l.filter(pred);\n\
   const prim__join_strings = (sep, ls) => ls.join(sep);\n\
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
   const prim__int_to_string = (n) => String(n);\n"

let ts_effects_runtime =
  "let __chester_caps = [];\n\
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
   };\n"

let ts_runtime_file = ts_primitives ^ ts_effects_runtime

let ts_test_preamble =
  ts_primitives
  ^ "const int_add = prim__int_add;\n\
     const int_mul = prim__int_mul;\n\
     const int_sub = prim__int_sub;\n" ^ ts_effects_runtime

let go_preamble_body =
  {|
package main

import (
    "fmt"
    "strings"
    "strconv"
    "io"
    "os"
)

func prim__string_eq(a, b string) bool { return a == b }
func prim__string_concat(a, b string) string { return a + b }
func prim__string_length(a string) int { return len(a) }
func prim__string_substring(s string, start, end int) string { return s[start:end] }

func prim__int_eq(a, b int) bool { return a == b }
func prim__int_add(a, b int) int { return a + b }
func prim__int_sub(a, b int) int { return a - b }
func prim__int_mul(a, b int) int { return a * b }
func prim__int_div(a, b int) int { return a / b }
func prim__int_mod(a, b int) int { return a % b }
func prim__int_lt(a, b int) bool { return a < b }
func prim__int_gt(a, b int) bool { return a > b }
func prim__int_le(a, b int) bool { return a <= b }
func prim__int_ge(a, b int) bool { return a >= b }
func prim__int_neg(a int) int { return -a }

func prim__bool_or(a, b bool) bool { return a || b }
func prim__bool_and(a, b bool) bool { return a && b }
func prim__bool_not(a bool) bool { return !a }

func prim__int_to_string(a int) string { return fmt.Sprintf("%d", a) }

func __chester_as_int(v any) int { return v.(int) }
func __chester_as_string(v any) string { return v.(string) }
func __chester_as_bool(v any) bool { return v.(bool) }

// Surface names used by desugaring (`+` → int_add) and stdlib wrappers.
// Emitter skips AstDef for these names so emitting stdlib does not redeclare.
var int_add = prim__int_add
var int_sub = prim__int_sub
var int_mul = prim__int_mul
var int_div = prim__int_div
var int_mod = prim__int_mod
var int_neg = prim__int_neg
var int_eq = prim__int_eq
var int_lt = prim__int_lt
var int_gt = prim__int_gt
var int_le = prim__int_le
var int_ge = prim__int_ge
var bool_or = prim__bool_or
var bool_and = prim__bool_and
var bool_not = prim__bool_not
var string_eq = prim__string_eq
var string_concat = prim__string_concat
var string_length = prim__string_length
var string_substring = prim__string_substring
var int_to_string = prim__int_to_string
var list_length = prim__list_length

func prim__list_empty() any { return []any{} }
func prim__list_length(l any) int { return len(l.([]any)) }
func prim__list_get(l, i any) any { return l.([]any)[i.(int)] }
func prim__list_advance(l any) any {
	switch v := l.(type) {
	case []any:
		if len(v) == 0 {
			return v
		}
		return v[1:]
	default:
		panic("list_advance on non-list")
	}
}

func prim__list_filter(l any, pred any) any {
	switch v := l.(type) {
	case []any:
		var res []any
		fn := pred.(func(any) any)
		for _, x := range v {
			if fn(x).(bool) {
				res = append(res, x)
			}
		}
		if res == nil {
			return []any{}
		}
		return res
	default:
		panic("list_filter on non-list")
	}
}

func prim__join_strings(sep any, ls any) any {
	switch v := ls.(type) {
	case []any:
		if len(v) == 0 {
			return ""
		}
		var sb strings.Builder
		for i, x := range v {
			if i > 0 {
				sb.WriteString(sep.(string))
			}
			sb.WriteString(x.(string))
		}
		return sb.String()
	default:
		panic("join_strings on non-list")
	}
}

func prim__list_make(l, f any) any {
	res := make([]any, l.(int))
	fn := f.(func(any) any)
	for i := 0; i < l.(int); i++ {
		res[i] = fn(i)
	}
	return res
}
func prim__list_insert_first(l, e any) any {
	return append([]any{e}, l.([]any)...)
}
func prim__list_append(l1, l2 any) any {
	return append(l1.([]any), l2)
}
func prim__list_drop_last(l any) any {
	ls := l.([]any)
	return ls[:len(ls)-1]
}


func __chester_get_args() any {
    args := os.Args[1:]
    res := make([]any, len(args))
    for i, a := range args {
        res[i] = a
    }
    return res
}


func __chester_write_file(path any, data any) any {
    err := os.WriteFile(path.(string), []byte(data.(string)), 0644)
    if err != nil {
        panic(fmt.Sprintf("Failed to write file: %v", err))
    }
    return nil
}

func __chester_read_file(path any) any {
    bytes, err := os.ReadFile(path.(string))
    if err != nil {
        panic(fmt.Sprintf("Failed to read file: %v", err))
    }
    return string(bytes)
}

func __chester_read_stdin() any {
    bytes, _ := io.ReadAll(os.Stdin)
    return string(bytes)
}

func __chester_write_stdout(s any) any {
    fmt.Print(s.(string))
    return nil
}

func __chester_field(obj any, field string) any {
    if obj == nil { panic(fmt.Sprintf("cannot access field %s on nil", field)) }
    if m, ok := obj.(map[string]any); ok {
        if val, ok := m[field]; ok {
            return val
        }
    }
    panic(fmt.Sprintf("unknown field %s on %T", field, obj))
}

var __chester_caps []map[string]any
var __chester_frame_id int

type __chesterHandled struct {
	id     int
	result any
}

func __chester_handle(label string, bodyFn func() any, handlers map[string]any) any {
	var run func(answers []any) any
	run = func(answers []any) (res any) {
		ai := 0
		__chester_frame_id++
		fid := __chester_frame_id
		frame := map[string]any{"label": label, "handlers": handlers, "id": fid}
		frame["take"] = func() (bool, any) {
			if ai < len(answers) {
				v := answers[ai]
				ai++
				return true, v
			}
			return false, nil
		}
		frame["fork"] = func(v any) any {
			next := append(append([]any{}, answers[:ai]...), v)
			return run(next)
		}
		__chester_caps = append(__chester_caps, frame)
		defer func() { __chester_caps = __chester_caps[:len(__chester_caps)-1] }()
		defer func() {
			if r := recover(); r != nil {
				if h, ok := r.(__chesterHandled); ok && h.id == fid {
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

func __chester_perform(op string, args []any) any {
	for i := len(__chester_caps) - 1; i >= 0; i-- {
		frame := __chester_caps[i]
		handlers := frame["handlers"].(map[string]any)
		h, ok := handlers[op]
		if !ok {
			continue
		}
		take := frame["take"].(func() (bool, any))
		if ok2, v := take(); ok2 {
			return v
		}
		resume := func(v any) any {
			return frame["fork"].(func(any) any)(v)
		}
		fn := h
		for _, a := range args {
			fn = fn.(func(any) any)(a)
		}
		result := fn.(func(any) any)(resume)
		panic(__chesterHandled{id: frame["id"].(int), result: result})
	}
	panic("Unhandled effect operation: " + op)
}

func __chester_evidence(labels []any) []map[string]any {
	var ev []map[string]any
	for _, lab := range labels {
		ls := lab.(string)
		for i := len(__chester_caps) - 1; i >= 0; i-- {
			if __chester_caps[i]["label"] == ls {
				ev = append(ev, map[string]any{
					"label":    ls,
					"handlers": __chester_caps[i]["handlers"],
				})
				break
			}
		}
	}
	return ev
}

func __chester_with_evidence(ev []map[string]any, bodyFn func() any) any {
	var nest func(i int) any
	nest = func(i int) any {
		if i >= len(ev) {
			return bodyFn()
		}
		lab := ev[i]["label"].(string)
		handlers := ev[i]["handlers"].(map[string]any)
		return __chester_handle(lab, func() any { return nest(i + 1) }, handlers)
	}
	return nest(0)
}

func __chester_box(labels []any, bodyFn func() any) any {
	ev := __chester_evidence(labels)
	return func() any { return __chester_with_evidence(ev, bodyFn) }
}

var Unit = struct{}{}


var _global_elab_state any
func prim__get_elab_state() any { return _global_elab_state }
func prim__put_elab_state(s any) any { _global_elab_state = s; return Unit }

|}

(* Emit a Go double-quoted string literal (preamble contains backticks, so raw strings cannot wrap it). *)
let go_string_lit (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* Fixed source of __chester_assemble_go for re-emission into stage2+. Uses strconv.Quote(pre) at runtime. *)
let go_assemble_core_src =
  "\nfunc __chester_assemble_go(body any) any {\n"
  ^ "\tb := strings.Replace(body.(string), \"func main(\", \"func \
     chester_main(\", 1)\n"
  ^ "\tif !strings.Contains(b, \"func chester_main(\") {\n"
  ^ "\t\tb = b + \"\\nfunc chester_main() any { return nil }\\n\"\n" ^ "\t}\n"
  ^ "\tpre := __chester_go_preamble().(string)\n"
  ^ "\tgetter := \"\\nfunc __chester_go_preamble() any { return \" + \
     strconv.Quote(pre) + \" }\\n\"\n"
  ^ "\tsrc := __chester_assemble_go_src()\n"
  ^ "\tsrcFn := \"\\nfunc __chester_assemble_go_src() string { return \" + \
     strconv.Quote(src) + \" }\\n\"\n"
  ^ "\treturn pre + getter + src + srcFn + b + \"\\nfunc main() \
     {\\n\\tfmt.Println(chester_main())\\n}\\n\"\n" ^ "}\n"

let go_assemble_helpers =
  "\nfunc __chester_go_preamble() any { return "
  ^ go_string_lit go_preamble_body
  ^ " }\n" ^ go_assemble_core_src
  ^ "\nfunc __chester_assemble_go_src() string { return "
  ^ go_string_lit go_assemble_core_src
  ^ " }\n"

let go_effects_preamble = go_preamble_body ^ go_assemble_helpers

let rocq_effects_preamble =
  "From Stdlib Require Import Strings.String.\n\
   Open Scope string_scope.\n\n\
   Inductive chester_dyn : Type :=\n\
   | chester_unit : chester_dyn\n\
   | chester_nat : nat -> chester_dyn\n\
   | chester_bool : bool -> chester_dyn\n\
   | chester_str : string -> chester_dyn\n\
   | chester_fun : (chester_dyn -> chester_dyn) -> chester_dyn.\n\n\
   Definition chester_var (v : chester_dyn) : chester_dyn := v.\n\
   Definition chester_set (_ : string) (_ : chester_dyn) : chester_dyn := \
   chester_unit.\n\
   Definition chester_expr_stmt (v : chester_dyn) : chester_dyn := v.\n\
   Definition prim__int_add (a b : nat) : chester_dyn := chester_nat (a + \
   b)%nat.\n\
   Definition int_add (a b : chester_dyn) : chester_dyn := a.\n\
   Definition Unit : chester_dyn := chester_unit.\n\n\
   Parameter __chester_perform : string -> list chester_dyn -> chester_dyn.\n\
   Parameter __chester_handle : string -> (unit -> chester_dyn) -> list \
   (string * chester_dyn) -> chester_dyn.\n\
   Parameter __chester_box : list string -> (unit -> chester_dyn) -> \
   chester_dyn.\n\n"
