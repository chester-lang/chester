const Unit = {};
const prim__string_eq = (a, b) => a === b;
const prim__list_length = (l) => l.length;
const prim__int_eq = (a, b) => a === b;
const prim__list_make = (len, f) => Array.from({length: len}, (_, i) => f(i));
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
const prim__int_neg = (a) => -a;
const prim__int_to_string = (n) => String(n);
let __chester_caps = [];
const __chester_handle = (label, bodyFn, handlers) => {
const run = (answers) => {
let ai = 0;
const frame = {
label,
handlers,
take: () => {
if (ai < answers.length) return { ok: true, v: answers[ai++] };
return { ok: false };
},
fork: (v) => run(answers.slice(0, ai).concat([v]))
};
__chester_caps.push(frame);
try { return bodyFn(); }
catch (e) {
if (e && e.__chester_handled === frame) return e.result;
throw e;
}
finally { __chester_caps.pop(); }
};
return run([]);
};
const __chester_perform = (op, args) => {
for (let i = __chester_caps.length - 1; i >= 0; i--) {
const frame = __chester_caps[i];
const h = frame.handlers[op];
if (!h) continue;
const got = frame.take();
if (got.ok) return got.v;
const resume = (v) => frame.fork(v);
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
for (let i = __chester_caps.length - 1; i >= 0; i--) {
if (__chester_caps[i].label === lab) {
ev.push({ label: lab, handlers: __chester_caps[i].handlers });
break;
}
}
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
