(* Shared effect runtimes for TS and Go backends (used by main + tests). *)

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

let ts_test_preamble =
  "const Unit = {};\n\
   const prim__string_eq = (a, b) => a === b;\n\
   const prim__int_add = (a, b) => a + b;\n\
   const int_add = prim__int_add;\n"
  ^ ts_effects_runtime

let go_effects_preamble =
  {|
package main

import "fmt"

var __chester_caps []map[string]interface{}
var __chester_frame_id int

type __chesterHandled struct {
	id     int
	result interface{}
}

func __chester_handle(label string, bodyFn func() interface{}, handlers map[string]interface{}) interface{} {
	var run func(answers []interface{}) interface{}
	run = func(answers []interface{}) (res interface{}) {
		ai := 0
		__chester_frame_id++
		fid := __chester_frame_id
		frame := map[string]interface{}{"label": label, "handlers": handlers, "id": fid}
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
		panic(__chesterHandled{id: frame["id"].(int), result: result})
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
	var nest func(i int) interface{}
	nest = func(i int) interface{} {
		if i >= len(ev) {
			return bodyFn()
		}
		lab := ev[i]["label"].(string)
		handlers := ev[i]["handlers"].(map[string]interface{})
		return __chester_handle(lab, func() interface{} { return nest(i + 1) }, handlers)
	}
	return nest(0)
}

func __chester_box(labels []interface{}, bodyFn func() interface{}) interface{} {
	ev := __chester_evidence(labels)
	return func() interface{} { return __chester_with_evidence(ev, bodyFn) }
}

func prim__int_add(a interface{}, b interface{}) interface{} {
	return a.(int) + b.(int)
}

var int_add = prim__int_add
var Unit = struct{}{}
|}
