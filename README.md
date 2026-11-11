# compiler-optimizer

An optimizing compiler for a small imperative language, written in OCaml. Its design follows LLVM: source programs are lowered into a block-structured IR, which is then rewritten by a pipeline of composable, independently testable passes run by a pass manager until nothing changes. The result is either interpreted directly or emitted as textual LLVM IR.

```
source ──► lexer ──► parser ──► AST ──► lowering ──► IR ──► pass manager ──► IR ──┬──► IR interpreter
                                 │                          (verify after        │
                                 └──► reference interpreter   every pass)        └──► LLVM IR (.ll)
```

Across the 60 programs in `test/programs`, the `-O1` pipeline removes **25.8% of static instructions** and **18.1% of executed instructions**. Every program prints the same output as the source-level reference interpreter, at both `-O0` and `-O1`, and also when compiled to LLVM and run under `lli`.

**Try it in the browser: [compiler-optimizer-playground.vercel.app](https://compiler-optimizer-playground.vercel.app).** Step through each optimization pass and watch the program execute.

## Building

Requires OCaml ≥ 5.0, Dune ≥ 3.0, and OUnit2 for the tests.

```sh
opam install dune ounit2
dune build
dune test
```

LLVM is optional. If it is installed, `scripts/check_llvm.sh` runs every test program through the LLVM backend.

## The language

Integers, booleans (stored as 0/1), functions, block-scoped `let`, `if`/`else if`/`else`, `while` with `break`/`continue`, short-circuit `&&`/`||`, and `print`. Execution starts at `main`.

```
fn gcd(a, b) {
  while (b != 0) {
    let t = b;
    b = a % b;
    a = t;
  }
  return a;
}

fn main() {
  print(gcd(1071, 462));
}
```

Division or remainder by zero is a runtime error, and the optimizer has to preserve it: a division whose result is unused is still kept if it might trap.

## Usage

```
optc interp <file>          run the source-level reference interpreter
optc run    <file>          compile, optimize and execute the IR
optc ir     <file>          print the IR after optimization
optc llvm   <file>          emit textual LLVM IR
optc bench  <dir>           compare -O0 and -O1 across every .mini in a directory
optc passes                 list the available passes

  -O0 | -O1                 optimization level (default -O1)
  --passes=a,b,c            run a custom pipeline
  --stats                   per-pass statistics on stderr
```

```sh
dune exec ./bin/optc.exe -- ir --stats examples/scale.mini
dune exec ./bin/optc.exe -- ir --passes=constprop,dce examples/scale.mini
dune exec ./bin/optc.exe -- llvm test/programs/03_fib_recursive.mini > fib.ll && lli fib.ll
```

## Playground

`web/` is a browser front end that runs this same compiler, compiled to JavaScript with js_of_ocaml. It is live at https://compiler-optimizer-playground.vercel.app.

- **Optimize** steps through every pass the `-O1` pipeline runs. Each step shows the control-flow graph, with new instructions highlighted, and a diff against the previous step.
- **Run** steps through execution of the `-O0` or `-O1` IR. It highlights the current instruction and the branch taken, and shows the call stack with each frame's registers and the program output.

```sh
opam install js_of_ocaml js_of_ocaml-ppx
bash web/build.sh                       # writes web/optimizer.js
python3 -m http.server -d web 8000      # open http://localhost:8000
cd web && vercel deploy --prod          # update the live site
```

Under js_of_ocaml, OCaml's `int` is 32-bit, so programs with literals or results beyond ±2³¹ behave differently than in the native build. Execution traces in the page stop after 20,000 steps.

## The IR

Each function is a list of basic blocks. The first block is the entry block, and no branch may target it. A block holds a list of instructions and ends in exactly one terminator.

| instruction            | meaning                                   |
| ---------------------- | ----------------------------------------- |
| `%d = <op> a, b`       | `add sub mul div rem eq ne lt le gt ge`   |
| `%d = neg a`, `not a`  | unary operators                           |
| `%d = mov a`           | copy                                      |
| `%d = call @f(a, ...)` | call; the destination is optional         |
| `print a`              | output                                    |
| `jmp L`                | unconditional branch                      |
| `br c, L1, L2`         | branch on `c != 0`                        |
| `ret a`                | return                                    |

Operands are registers (`%x`) or immediates. Source variables become named registers, and a shadowed name gets a fresh suffix (`%x.1`). Temporaries are numbered (`%0`, `%1`, …), the way LLVM numbers unnamed values. The IR is not in SSA form: a register can be assigned more than once, which keeps lowering simple and makes the dataflow analyses do the real work.

Lowering is deliberately naive, like `clang -O0`. Every subexpression gets its own temporary, every `let` is a copy, `&&`/`||` become explicit control flow, and code after a `return` is placed in an orphan block. Cleaning that up is the optimizer's job.

### Before and after

`examples/scale.mini`:

```
fn scale(x) {
  let factor = 2 + 2;
  let bias = factor - 4;
  let unused = x * 100;
  if (factor > 3) {
    return x * factor + bias;
  }
  return -1;
}
```

`-O0` (19 instructions):

```
define @scale(%x) {
entry:
  %0 = add 2, 2
  %factor = mov %0
  %1 = sub %factor, 4
  %bias = mov %1
  %2 = mul %x, 100
  %unused = mov %2
  %3 = gt %factor, 3
  br %3, if.then1, if.else2
if.then1:
  %4 = mul %x, %factor
  %5 = add %4, %bias
  ret %5
dead4:
  jmp if.end3
if.else2:
  jmp if.end3
if.end3:
  %6 = neg 1
  ret %6
dead5:
  ret 0
}
```

`-O1` (5 instructions across the whole program):

```
define @scale(%x) {
entry:
  %4 = mul %x, 4
  ret %4
}
```

```
pipeline reached a fixpoint after 3 round(s)
  simplifycfg  runs=6   removed=4
  constprop    runs=3   removed=0
  copyprop     runs=3   removed=0
  instcombine  runs=3   removed=0
  dce          runs=3   removed=10
  instructions: 19 -> 5 (73.7% fewer)
```

Constant propagation and instcombine rewrite instructions in place without deleting them, so their contribution shows up as removals in the DCE and simplifycfg passes that run after them.

## Passes

All analyses run on a generic worklist solver (`lib/dataflow.ml`), a functor over a join-semilattice that provides forward and backward solving. Each pass is a plain `Ir.func -> Ir.func` function. The pass manager runs a pipeline, verifies the IR after every pass, records per-pass statistics, and repeats the pipeline until the program stops changing.

`-O1` is `simplifycfg, constprop, copyprop, instcombine, dce, simplifycfg`.

### `constprop`: sparse conditional constant propagation

This is a forward analysis over the lattice `unreached ⊑ Const n ⊑ Varying`, kept per register. The transfer function returns a fact for each outgoing edge rather than one fact per block: when a branch condition is a known constant, only the taken edge receives the state and the other edge stays unreached. This is the optimistic part of Wegman–Zadeck SCCP. It lets the analysis prove facts through loops that a pessimistic analysis would give up on:

```
let x = 1;
while (i < 10) {
  if (x == 1) { x = 1; } else { x = 2; }
  ...
}
print(x);            // becomes print 1
```

The `else` edge is never marked feasible, so `x = 2` never reaches the loop header and `x` stays `Const 1`. After the analysis converges, operands with known values become immediates, instructions with known results become `mov` of a constant, and branches on constants become `jmp`. Division by a constant zero is never folded.

### `copyprop`: global copy propagation

This is a forward available-copies analysis. A fact is a map `dst ↦ src` for each `mov %dst, %src` that holds on every path, so the join is an intersection. Redefining either side kills the copy. Uses are rewritten to the original source, and that usually leaves the copy itself dead.

### `instcombine`: local algebraic simplification

This pass rewrites within a block, guided by a table of earlier definitions in that block. An entry is invalidated as soon as its destination or any of its operands is redefined.

- identities: `x+0`, `x*1`, `x/1` → `x`; `x*0`, `x-x`, `x%1` → `0`; `x==x` → `1`
- canonicalization: constants move to the right (`3 < x` → `x > 3`), and `x - c` becomes `x + (-c)`
- reassociation: `(x + 1) + 2` → `x + 3`, `(x * 2) * 3` → `x * 6`
- comparisons: `not (a < b)` → `a >= b`, and `(cmp) != 0` → `cmp`
- branches: `br (x != 0)` → `br x`, and `br (not x), T, F` → `br x, F, T`

### `dce`: dead code elimination

This is a backward liveness analysis. The transfer function skips the uses of an instruction it is about to delete, which computes strong liveness. That removes dead cycles such as a counter that only feeds itself (`wasted = wasted + i` inside a loop), which plain liveness would keep. Calls and prints are never removed, though an unused call result is dropped. A division is kept unless its divisor is a known non-zero constant, because removing it could hide a runtime error.

### `simplifycfg`: control-flow cleanup

Repeated until it reaches a fixpoint:

- fold `br` on a constant, or with identical targets, into `jmp`
- thread jumps through empty blocks (with cycle detection, so `while (true) {}` survives)
- delete blocks unreachable from the entry
- merge a block into its only predecessor when that predecessor ends in a `jmp` to it

## Correctness

The project has two interpreters with independent implementations. `Interp` walks the AST using its own environments and control-flow exceptions. `Ir_interp` executes the IR. Both share only the arithmetic definitions in `Semantics`, so a lowering bug shows up as a disagreement between them.

`dune test` runs 538 OUnit2 tests:

- **Unit tests** for the parser (precedence, associativity, error lines), lowering errors, and each pass on hand-built IR: copies killed by redefinition, copies that disagree at a join, reassociation blocked by an intervening write, division traps kept by DCE, jump threading, block merging, and the verifier rejecting malformed IR.
- **Differential tests.** Each of the 60 programs in `test/programs` is checked against its `.expected` output under the reference interpreter, unoptimized IR, `-O1`, and each of the five passes run alone. The last group catches passes that only work when other passes have run before them. The suite also checks that `-O1` never makes a program larger and that a second `-O1` run changes nothing.
- **IR verification after every pass**: unique labels, valid branch targets, no branch into the entry block, every used register defined, calls matching the callee's arity.

The test programs cover recursion (Ackermann, McCarthy 91, mutual recursion), loops with `break`/`continue`, shadowing, short-circuit evaluation with side effects, argument evaluation order, arithmetic on negative numbers, runtime errors inside callees, and programs aimed at specific passes (dead counters, identities, reassociation, constant branches).

`scripts/regen_expected.sh` regenerates the `.expected` files from the reference interpreter. `scripts/check_llvm.sh` compiles every program to LLVM at `-O0` and `-O1`, verifies the module with `opt -passes=verify`, runs it with `lli`, and compares the output. All 120 runs pass.

## LLVM backend

`Llvm_emit` produces a self-contained module that targets opaque pointers (LLVM 15+). Each IR register becomes an `alloca` slot. Running LLVM's `mem2reg` promotes these back to SSA, the same way clang hands code to LLVM. Comparisons are emitted as `icmp` followed by `zext`. Division goes through small runtime helpers that print the same error message as the interpreters and exit, so trapping behavior matches across all three backends.

## Results

`dune exec ./bin/optc.exe -- bench test/programs`, abridged:

```
program                         -O0    -O1  static     dyn -O0     dyn -O1 dynamic  output
02_constant_chain                14      3   78.6%          14           3   78.6%  match
13_ackermann                     28     21   25.0%       18649       12416   33.4%  match
18_constant_branches             46      5   89.1%          30           5   83.3%  match
24_perfect_numbers               40     33   17.5%     7008139     5657873   19.3%  match
30_hanoi                         35     28   20.0%       62031       41385   33.3%  match
37_dead_counters                 16      9   43.8%         508         256   49.6%  match
39_reassociation                 20     10   50.0%          41          16   61.0%  match
60_sparse_conditional            31     17   45.2%         143          86   39.9%  match
...
------------------------------------------------------------------------------------------------
total (60 programs)            1699   1261   25.8%     8248222     6753783   18.1%  60/60 match
```

The weakest results are on tight arithmetic loops such as `09_nested_loops`. There the remaining waste is the `%t = add %i, 1; %i = mov %t` pattern at each loop back-edge, which copy propagation alone cannot remove because the copy is live around the loop. Removing it needs SSA construction or register coalescing, which are the natural next steps.

## Layout

```
lib/
  ast.ml  lexer.ml  parser.ml      front end (hand-written lexer, precedence-climbing parser)
  semantics.ml                     shared arithmetic and result types
  interp.ml                        reference AST interpreter
  lower.ml                         AST -> IR
  ir.ml  cfg.ml  verify.ml         IR definition, printer, CFG utilities, verifier
  ir_interp.ml                     IR interpreter with dynamic instruction counting
  dataflow.ml                      generic forward/backward worklist solver
  constprop.ml copyprop.ml instcombine.ml dce.ml simplifycfg.ml
  pass_manager.ml                  pipelines, fixpoint iteration, statistics
  llvm_emit.ml                     LLVM IR backend
bin/optc.ml                        command-line driver
test/test_main.ml                  OUnit2 suite
test/programs/                     60 programs with expected output
scripts/                           expected-output regeneration and LLVM checks
```
