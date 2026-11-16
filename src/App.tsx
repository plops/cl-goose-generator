import { Download, Check, Copy } from 'lucide-react';
import { useState } from 'react';

interface ExamplePair {
  id: string;
  title: string;
  summary: string;
  lisp: string;
  goose: string;
}

const EXAMPLES: ExamplePair[] = [
  {
    id: 'inline-varint',
    title: '01. Inline Variable-Size Data & Copy-Free Returns',
    summary: 'Varint fields, inferred var items = [], and destination-constructed returns with zero heap allocations.',
    lisp: `(do0
  (import std)
  (defstruct Item
    (sku (arr u8 varint))
    (qty varint)
    (cents varint))
  (defstruct Order
    (id varint)
    (customer (arr u8 varint))
    (items (arr Item varint)))

  (defun parse_items (field)
    (declare (type (slice u8) field)
             (values (grow Item)))
    (var ((items (list)))
      (with-block (each_split field #\\;)
        (let ((p (split it #\\:))
              ((tuple qty ok1) (parse_int (aref p 1)))
              ((tuple cents ok2) (parse_int (aref p 2))))
          (assert (and ok1 ok2))
          (dot items (push (make Item :sku (aref p 0) :qty qty :cents cents)))))
      (return items))))`,
    goose: `import std;
struct Item { sku: u8[varint], qty: varint, cents: varint }
struct Order { id: varint, customer: u8[varint], items: Item[varint] }
fn parse_items(field: u8[:]) -> Item[>..] {
    var items = [];
    each_split(field, ';') {
        let p = split(it, ':');
        let qty, ok1 = parse_int(p[1]);
        let cents, ok2 = parse_int(p[2]);
        assert(ok1 && ok2);
        items.push(Item { sku: p[0], qty: qty, cents: cents });
    };
    return items;
}`,
  },
  {
    id: 'slot-pool-self',
    title: '02. Slot Pools, Pool-Relative Links & Self Sentinels',
    summary: 'Non-nesting lifetimes with slot_pool<Node>, 4-byte in-pool offsets, and self sentinel construction.',
    lisp: `(do0
  (import slot_pool)
  (defstruct Node
    (val i64)
    (prev (rel Node u32 :in (dot pool items)))
    (next (rel Node u32 :in (dot pool items))))

  (defparameter pool (make (inst slot_pool Node)))
  (defconstant (.= head)
    (with-block (dot pool (alloc_with))
      (make Node :val 0 :prev self :next self)))

  (defun insert_after (p v)
    (declare (type (ref Node) p)
             (type i64 v)
             (values (ref Node)))
    (let (((.= n) (dot pool (alloc_ref (make Node :val v :prev p :next (dot p next))))))
      (.= (dot n next prev) n)
      (.= (dot p next) n)
      (return n))))`,
    goose: `import slot_pool;
struct Node { val: i64, prev: Node&<u32 in pool.items>, next: Node&<u32 in pool.items> }
var pool = slot_pool<Node> {};
let head .= pool.alloc_with() {
    Node { val: 0, prev: self, next: self }
};
fn insert_after(p: Node&, v: i64) -> Node& {
    let n .= pool.alloc_ref(Node { val: v, prev: p, next: p.next });
    n.next.prev .= n;
    p.next .= n;
    return n;
}`,
  },
  {
    id: 'enums-reflection',
    title: '03. Variable-Mode Enums, Splicing & Const-For Reflection',
    summary: 'Packed Shape.. payloads mutated in place via &r, Lisp ,@(loop ...) splicing, and compile-time reflection.',
    lisp: `(do0
  (defenum Shape
    (Circle (r f64))
    (Rect (w f64) (h f64))
    Dot)

  (defun reset_defaults (src)
    (declare (generics elem)
             (type (ref elem) src)
             (values elem))
    (return (make elem
              (const-for ((tuple fname facc) (fields elem))
                (const-if (not (has_default facc))
                  (:field facc (dot src facc))))
              :..)))

  (defun main ()
    (var ((packed (list) (grow (var-mode Shape))))
      ,@(loop for (tag args) in '((Circle (:r 2.0)) (Rect (:w 3.0 :h 4.0)) (Dot ()))
              collect \`(dot packed (push (variant Shape ,tag ,@args))))
      (for (s packed)
        (match s
          ((Rect &r) (incf (dot r w) 1.0))
          (_ (scope)))))))`,
    goose: `enum Shape { Circle { r: f64 }, Rect { w: f64, h: f64 }, Dot }
fn reset_defaults<elem>(src: elem&) -> elem {
    return elem { const for <fname, facc> in fields<elem> { const if !has_default<facc> { facc: src.facc, } }, .. };
}
fn main() {
    var packed: Shape..[>..] = [];
    packed.push(Shape.Circle { r: 2.0 });
    packed.push(Shape.Rect { w: 3.0, h: 4.0 });
    packed.push(Shape.Dot);
    for s in packed {
        match s {
            Rect &r => {
                r.w += 1.0;
            },
            _ => {},
        }
    }
}`,
  },
  {
    id: 'checkfree-threads',
    title: '04. Index Types, Checkfree Proofs & Share-Nothing Workers',
    summary: 'Prove zero bounds checks remain with checkfree and generate typed thread_fn workers.',
    lisp: `(do0
  (defparameter nodes (list) (grow i32))
  (deftype NodeId (in u32 nodes))

  (defun sum_at (ids weights)
    (declare (checkfree)
             (type (slice NodeId) ids)
             (type (ref (grow i32)) weights)
             (values i64))
    (assert (>= (dot weights len) (dot nodes len)))
    (var ((total 0))
      (for (id ids)
        (incf total (+ (aref nodes id) (aref weights id))))
      (return total)))

  (defun worker ()
    (declare (thread-fn))
    (loop
      (let ((job ((inst qget Job))))
        (guard (>= (dot job n) 0) :else (break))
        (qput (make Result :n (dot job n) :digits (count_digits (dot job n))))))))`,
    goose: `var nodes: i32[>..] = [];
type NodeId = u32 in nodes;
checkfree fn sum_at(ids: NodeId[:], weights: i32[>..]&) -> i64 {
    assert(weights.len >= nodes.len);
    var total = 0;
    for id in ids {
        total += nodes[id] + weights[id];
    }
    return total;
}
thread_fn worker() {
    loop {
        let job = qget<Job>();
        guard job.n >= 0 else {
            break;
        }
        qput(Result { n: job.n, digits: count_digits(job.n) });
    }
}`,
  },
];

const OPERATOR_ROWS = [
  ['Bindings', '(let ((x 1)) ...), (var ((x 1) (y nil i32)) ...)', 'let x = 1; var x = 1; var y: i32;'],
  ['Ref & Multi-Bind', '(var (((.= cur) (aref p 0))) ...), (let (((tuple q ok) (f))) ...)', 'var cur .= p[0]; let q, ok = f();'],
  ['Globals', '(defparameter x init [T]), (defconstant c init)', 'var x[: T] = init; let c = init;'],
  ['Functions', '(defun f (a &optional (b 0)) (declare (type i64 a b) (values i64) (checkfree)) ...)', 'checkfree fn f(a: i64, b: i64 = 0) -> i64 { ... }'],
  ['Returns', '(return x), (return (values a b)), (return-from load (values ...))', 'return x; return a, b; return ... from load;'],
  ['Mutations', '(setf a b), (.= r x), (incf x), (incf x n), (decf x n)', 'a = b; r .= x; x++; x += n; x -= n;'],
  ['Loops', '(for (i n) ...), (for (&x xs) ...), (for ((tuple x i) xs) ...)', 'for i in n, for &x in xs, for x, i in xs'],
  ['Arrays & Slices', '(list), (list 1 2), (fill 0 8), (subseq a lo hi), (^ k)', '[], [1, 2], [0; 8], a[lo..hi], ^k'],
  ['Types', '(arr T 4), (arr T varint), (cap-arr T 8), (grow T), (stack T), (slice T), (rel T u32 :in p), (in u32 a)', 'T[4], T[varint], T[..8], T[>..], T[>..<], T[:], T&<u32 in p>, u32 in a'],
];

const SETUP_SNIPPET = `ln -s /path/to/cl-goose-generator ~/quicklisp/local-projects/cl-goose-generator

;; In Emacs / SLIME REPL:
(ql:quickload :cl-goose-generator)
(asdf:test-system :cl-goose-generator)`;

export default function App() {
  const [copiedId, setCopiedId] = useState<string | null>(null);

  const handleCopy = async (id: string, text: string) => {
    try {
      await navigator.clipboard.writeText(text);
      setCopiedId(id);
      setTimeout(() => setCopiedId((prev) => (prev === id ? null : prev)), 1500);
    } catch {
      // Clipboard API unavailable
    }
  };

  return (
    <div className="min-h-screen bg-slate-50 text-slate-900">
      <header className="border-b border-slate-200 bg-white">
        <div className="mx-auto flex max-w-6xl items-center justify-between gap-8 px-6 py-4">
          <a
            href="#install"
            className="font-display text-lg font-bold tracking-tight text-slate-900 whitespace-nowrap shrink-0"
          >
            cl-goose-generator
          </a>

          <nav className="hidden md:flex items-center gap-6 text-sm font-medium text-slate-600">
            <a href="#install" className="hover:text-slate-900 transition-colors whitespace-nowrap shrink-0">
              Install
            </a>
            <a href="#showcase" className="hover:text-slate-900 transition-colors whitespace-nowrap shrink-0">
              Showcase
            </a>
            <a href="#operators" className="hover:text-slate-900 transition-colors whitespace-nowrap shrink-0">
              Operators
            </a>
          </nav>

          <a
            href="/api/download-asdf"
            download="cl-goose-generator.tar.gz"
            className="inline-flex items-center gap-2 rounded-lg bg-teal-700 px-4 py-2 text-xs font-medium text-white hover:bg-teal-800 transition-colors whitespace-nowrap shrink-0"
          >
            <Download className="h-3.5 w-3.5" aria-hidden="true" />
            <span>Download ASDF</span>
          </a>
        </div>
      </header>

      <main className="mx-auto max-w-6xl px-6 py-12 space-y-16">
        <section id="install" className="grid gap-8 lg:grid-cols-12 lg:items-start">
          <div className="lg:col-span-7 space-y-4">
            <p className="text-xs font-medium text-slate-500">
              Common Lisp ASDF System · Quicklisp Ready · Emacs &amp; SLIME
            </p>
            <h1 className="font-display text-3xl sm:text-4xl font-bold tracking-tight text-slate-900 text-balance">
              Programmable Goose code generation from Common Lisp S-expressions.
            </h1>
            <p className="text-base leading-relaxed text-slate-600 max-w-2xl">
              Model structs, root structs, relative references, pools, index types, and{' '}
              <code className="text-sm text-slate-800">const for</code> specializations using standard Common Lisp
              quasiquotes, <code className="text-sm text-slate-800">declare</code> forms, and{' '}
              <code className="text-sm text-slate-800">loop</code> splicing—without mutating your global readtable.
            </p>
          </div>

          <div className="lg:col-span-5 rounded-lg border border-slate-200 bg-white p-5">
            <div className="flex items-center justify-between gap-4 pb-3 border-b border-slate-100">
              <span className="text-sm font-semibold text-slate-900">Quicklisp &amp; SLIME Setup</span>
              <button
                type="button"
                onClick={() => handleCopy('setup', SETUP_SNIPPET)}
                className="inline-flex items-center gap-1.5 text-xs font-medium text-slate-600 hover:text-slate-900 whitespace-nowrap shrink-0"
              >
                {copiedId === 'setup' ? (
                  <>
                    <Check className="h-3.5 w-3.5 text-teal-700" aria-hidden="true" />
                    <span>Copied</span>
                  </>
                ) : (
                  <>
                    <Copy className="h-3.5 w-3.5" aria-hidden="true" />
                    <span>Copy</span>
                  </>
                )}
              </button>
            </div>
            <pre className="mt-3 overflow-x-auto text-xs leading-relaxed text-slate-800">
              <code>{SETUP_SNIPPET}</code>
            </pre>
          </div>
        </section>

        <section id="showcase" className="space-y-8">
          <div className="space-y-1">
            <h2 className="font-display text-2xl font-semibold tracking-tight text-slate-900">
              Lisp DSL to Goose Output
            </h2>
            <p className="text-sm text-slate-600">
              Runnable definitions live in <code className="text-xs text-slate-800">examples/showcase.lisp</code> inside the ASDF archive.
            </p>
          </div>

          <div className="space-y-8">
            {EXAMPLES.map((ex) => (
              <article key={ex.id} className="rounded-lg border border-slate-200 bg-white overflow-hidden">
                <div className="border-b border-slate-200 px-5 py-4 flex flex-col sm:flex-row sm:items-center sm:justify-between gap-2">
                  <div>
                    <h3 className="font-display text-base font-semibold text-slate-900">{ex.title}</h3>
                    <p className="text-sm text-slate-600">{ex.summary}</p>
                  </div>
                  <button
                    type="button"
                    onClick={() => handleCopy(ex.id, ex.lisp)}
                    className="inline-flex items-center gap-1.5 text-xs font-medium text-slate-600 hover:text-slate-900 whitespace-nowrap shrink-0 self-start sm:self-center"
                  >
                    {copiedId === ex.id ? (
                      <>
                        <Check className="h-3.5 w-3.5 text-teal-700" aria-hidden="true" />
                        <span>Copied Lisp</span>
                      </>
                    ) : (
                      <>
                        <Copy className="h-3.5 w-3.5" aria-hidden="true" />
                        <span>Copy Lisp</span>
                      </>
                    )}
                  </button>
                </div>

                <div className="grid lg:grid-cols-2 divide-y lg:divide-y-0 lg:divide-x divide-slate-200">
                  <div className="p-5 overflow-x-auto">
                    <div className="text-xs font-medium text-slate-500 mb-2">Common Lisp Input</div>
                    <pre className="text-xs leading-relaxed text-slate-800">
                      <code>{ex.lisp}</code>
                    </pre>
                  </div>
                  <div className="p-5 overflow-x-auto bg-slate-50/60">
                    <div className="text-xs font-medium text-slate-500 mb-2">Emitted Goose</div>
                    <pre className="text-xs leading-relaxed text-slate-900">
                      <code>{ex.goose}</code>
                    </pre>
                  </div>
                </div>
              </article>
            ))}
          </div>
        </section>

        <section id="operators" className="space-y-4">
          <h2 className="font-display text-2xl font-semibold tracking-tight text-slate-900">
            Operator Reference
          </h2>
          <div className="rounded-lg border border-slate-200 bg-white overflow-x-auto">
            <table className="w-full text-left border-collapse text-sm">
              <thead>
                <tr className="border-b border-slate-200 bg-slate-50 text-xs font-medium text-slate-600">
                  <th className="py-3 px-4">Category</th>
                  <th className="py-3 px-4">Common Lisp Form</th>
                  <th className="py-3 px-4">Emitted Goose</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-slate-100">
                {OPERATOR_ROWS.map(([cat, lisp, goose]) => (
                  <tr key={cat} className="hover:bg-slate-50/70">
                    <td className="py-3 px-4 font-medium text-slate-900 whitespace-nowrap">{cat}</td>
                    <td className="py-3 px-4 font-mono text-xs text-slate-700">{lisp}</td>
                    <td className="py-3 px-4 font-mono text-xs text-slate-900">{goose}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </section>
      </main>
    </div>
  );
}
