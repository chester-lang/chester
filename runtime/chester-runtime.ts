const Unit = {};
const Int = "Int";
const Integer = "Integer";
const String = "String";
const Bool = "Bool";
const Type = "Type";
const prim__string_eq = (a, b) => a === b;
const prim__list_length = (l) => l.length;
const prim__int_eq = (a, b) => a === b;
const prim__cst_tag = (cst) => cst._tag || "unknown_tag";
const prim__list_advance = (l) => l.slice(1);
const prim__list_filter = (l, pred) => l.filter(pred);
const prim__join_strings = (sep, ls) => ls.join(sep);
const prim__list_make = (len, f) => Array.from({length: len}, (_, i) => f(i));
const prim__list_append = (l, x) => [...l, x];
const prim__list_drop_last = (l) => l.slice(0, -1);
const prim__list_insert_first = (l, x) => [x, ...l];
const prim__int_sub = (a, b) => a - b;
const prim__list_get = (l, i) => l[i];
const prim__int_add = (a, b) => a + b;
const prim__int_lt = (a, b) => a < b;
const prim__string_length = (s) => s.length;
const prim__string_substring = (s, start, end) => s.substring(start, end);
const prim__string_concat = (s1, s2) => s1 + s2;
const prim__list_empty = () => [];
const prim__int_mul = (a, b) => a * b;
const prim__int_div = (a, b) => Math.floor(a / b);
const prim__int_mod = (a, b) => ((a % b) + b) % b;
const prim__int_gt = (a, b) => a > b;
const prim__int_ge = (a, b) => a >= b;
const prim__int_le = (a, b) => a <= b;
const prim__int_ne = (a, b) => a !== b;
const prim__int_neg = (a) => -a;
const prim__int_to_string = (n) => String(n);
const prim__bool_or = (a, b) => a || b;
const prim__bool_and = (a, b) => a && b;
const prim__bool_not = (a) => !a;
const int_add = prim__int_add;
const int_sub = prim__int_sub;
const int_mul = prim__int_mul;
const int_div = prim__int_div;
const int_mod = prim__int_mod;
const int_neg = prim__int_neg;
const int_eq = prim__int_eq;
const int_ne = prim__int_ne;
const int_lt = prim__int_lt;
const int_gt = prim__int_gt;
const int_le = prim__int_le;
const int_ge = prim__int_ge;
const bool_or = prim__bool_or;
const bool_and = prim__bool_and;
const bool_not = prim__bool_not;
const string_eq = prim__string_eq;
const string_concat = prim__string_concat;
const string_length = prim__string_length;
const string_substring = prim__string_substring;
const int_to_string = prim__int_to_string;
const list_length = prim__list_length;
let __chester_caps = [];
const __chester_handle = (label, bodyFn, handlers) => {
const shallow = !!handlers["__chester_shallow__"];
const multishot = !!handlers["__chester_multishot__"];
const run = (answers, install, hs) => {
let ai = 0;
let shots = 0;
const useHandlers = hs !== undefined ? hs : handlers;
const frame = {
label,
handlers: useHandlers,
shallow,
multishot,
take: () => {
if (ai < answers.length) return { ok: true, v: answers[ai++] };
return { ok: false };
},
fork: (v) => {
shots++;
if (!multishot && shots > 1)
throw new Error("one-shot resume: continuation already resumed");
const next = answers.slice(0, ai).concat([v]);
/* Always reinstall for answer-stream take. Shallow uses empty handlers so
new performs forward outward (Handlers-in-Action shallow). */
return run(next, true, shallow ? {} : undefined);
}
};
if (install !== false) __chester_caps.push(frame);
try {
const v = bodyFn();
if (__chester_caps.length && __chester_caps[__chester_caps.length-1] === frame)
__chester_caps.pop();
if (typeof handlers["return"] === "function") return handlers["return"](v);
return v;
}
catch (e) {
if (e && e.__chester_handled === frame) return e.result;
throw e;
}
finally {
if (__chester_caps.length && __chester_caps[__chester_caps.length-1] === frame)
__chester_caps.pop();
}
};
return run([], true);
};
/* Ambient FFI io capability — not user-handleable; present for stdlib print. */
__chester_caps.push({ label: "io", handlers: {}, take: () => ({ ok: false }), fork: (v) => v, shallow: false, multishot: false, ambient: true });
const __chester_perform = (op, args) => {
const slash = op.indexOf("/");
const wantLabel = slash >= 0 ? op.slice(0, slash) : null;
const opName = slash >= 0 ? op.slice(slash + 1) : op;
for (let i = __chester_caps.length - 1; i >= 0; i--) {
const frame = __chester_caps[i];
if (wantLabel !== null && frame.label !== wantLabel) continue;
/* Prefer pending answer-stream values before requiring a handler clause
(shallow resume installs an answer-only frame with empty handlers). */
const got = frame.take();
if (got.ok) return got.v;
const h = frame.handlers[opName];
if (!h) continue;
const resume = (v) => frame.fork(v);
__chester_caps.length = i;
let fn = h;
for (let j = 0; j < args.length; j++) fn = fn(args[j]);
const result = fn(resume);
throw { __chester_handled: frame, result };
}
throw new Error("Unhandled effect operation: " + op);
};
const __chester_evidence = (labels) => {
const ev = [];
for (let li = 0; li < labels.length; li++) {
const lab = labels[li];
let found = false;
for (let i = __chester_caps.length - 1; i >= 0; i--) {
if (__chester_caps[i].label === lab) {
ev.push({ label: lab, handlers: __chester_caps[i].handlers });
found = true;
break;
}
}
if (!found) throw new Error("Missing capability for box: " + lab);
}
return ev;
};
const __chester_with_evidence = (ev, bodyFn) => {
let i = 0;
const go = () => {
if (i >= ev.length) return bodyFn();
const { label, handlers } = ev[i++];
return __chester_handle(label, go, handlers);
};
return go();
};
const __chester_box = (labels, bodyFn) => {
const ev = __chester_evidence(labels);
return () => __chester_with_evidence(ev, bodyFn);
};
