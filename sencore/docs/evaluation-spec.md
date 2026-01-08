# Work In Progress Sencore Evaluation Specification

## Todo
- correct runtime closure partial evaluation (currently captures scope with free
  vars)
- clear ordering of side effects in comptime evaluation
- clean up and make notation clearer

## Overview

Sencore uses a **two-phase evaluation model**:
1. **Compile-time evaluation** (`comptime_eval`, denoted `⇓`): Fully reduces expressions to values
2. **Partial evaluation** (`partial_eval`, denoted `⇝`): Reduces comptime expressions while preserving runtime residual code

## Types

| Type | Description |
|------|-------------|
| `void` | Unit type |
| `i32` | 32-bit signed integer |
| `bool` | Boolean |
| `memptr` | Virtual memory pointer (allocation ID + offset) |
| `type` | Type as a first-class value |
| `fn` | Function/closure |
| `struct` | User-defined struct (identified by unique def_uuid) |

## Values

```
v ::= () | n | true | false | ptr(alloc, offset) | T | closure | struct_val
```

## Expressions

```
e ::= x                           -- Variable
    | v                           -- Literal value
    | (builtin arg*)              -- Builtin call
    | (e.field)                   -- Member access
    | (if e1 e2 e3)               -- Conditional
    | (e1 e2)                     -- Application
    | (funcdef c x T e)           -- Lambda (c = comptime flag)
    | (recfuncdef f c x T e)      -- Recursive lambda
    | (struct_def capture fields) -- Struct type definition
    | (struct_init T fields)      -- Struct instantiation
```

## Scoping

- **Lexical scoping** with closure capture at definition site
- Scope is a linked list of bindings (scope tree with parent pointers)
- Bindings can be `Value(v)` (resolved) or `Free` (runtime variable)

## Closure Formation

Runtime closure bodies are always partially evaluated at creation time. This ensures captured comptime values are resolved regardless of how the closure is later used.

```
Comptime closure (body unevaluated - will be fully applied later):
-------------------------------------------------------------------
G |- mkClosure(true, f?, x, t, body) = <true, f?, x, t, body, G>

Runtime closure (body partially evaluated - will be residual code):
G, x->Free, f?->Free |- body >> body'
-------------------------------------------------------------------
G |- mkClosure(false, f?, x, t, body) = <false, f?, x, t, body', G'>
    where G' = G extended with free bindings
```

## Evaluation Rules

### Compile-Time Evaluation (=>)

Variables resolve to bound values:
```
G(x) = v
--------
G |- x => v
```

Conditionals branch on boolean condition:
```
G |- e1 => true    G |- e2 => v
-------------------------------
G |- (if e1 e2 e3) => v

G |- e1 => false    G |- e3 => v
--------------------------------
G |- (if e1 e2 e3) => v
```

Function definition uses closure formation:
```
G |- T => t    G |- mkClosure(c, f?, x, t, body) = closure
----------------------------------------------------------
G |- (funcdef c f? x T body) => closure
```

Application (closure call with type checking):
```
G |- e1 => <c, f?, x, t, body, Gc>    G |- e2 => v    type(v) = t
Gc, x->v, f?->closure |- body => v'
------------------------------------------------------------------
G |- (e1 e2) => v'
```

Struct definition evaluates field types and captures value:
```
G |- capture => vc    for all i: G |- Ti => ti
----------------------------------------------
G |- (struct_def capture [(fi Ti)*]) => Struct(uuid, [(fi, ti)*], vc)
```

Struct init type-checks fields in definition order:
```
G |- T => Struct(uuid, [(fi, ti)*], _)
for all i: G |- vi => vi' and type(vi') = ti
---------------------------------------------
G |- (struct_init T [(fi vi)*]) => struct_val(Struct(...), [vi'*])
```

Member access extracts field by name:
```
G |- e => struct_val(Struct(_, fields, _), vals)    fields[i] = (f, _)
----------------------------------------------------------------------
G |- (e.f) => vals[i]
```

### Partial Evaluation (>>)

Variables:
```
G(x) = v           G(x) = Free
----------         -----------
G |- x >> v        G |- x >> x
```

Function definition uses closure formation (same rule as comptime):
```
G |- T => t    G |- mkClosure(c, f?, x, t, body) = closure
----------------------------------------------------------
G |- (funcdef c f? x T body) >> closure
```

Comptime application fully evaluates then returns result:
```
G |- e1 >> <true, f?, x, t, body, Gc>    G |- e2 >> v    type(v) = t
Gc, x->v, f?->closure |- body => v'
--------------------------------------------------------------------
G |- (e1 e2) >> v'
```

Runtime application preserved:
```
G |- e1 >> e1'    G |- e2 >> e2'    e1' not comptime closure
------------------------------------------------------------
G |- (e1 e2) >> (e1' e2')
```

Conditionals partially evaluate all branches:
```
G |- e1 >> e1'    G |- e2 >> e2'    G |- e3 >> e3'
--------------------------------------------------
G |- (if e1 e2 e3) >> (if e1' e2' e3')
```

Builtins: comptime-only builtins always evaluate; others partially evaluate args:
```
builtin.comptime_only() = true    G |- args => vals
---------------------------------------------------
G |- (builtin args) >> builtin(vals)

builtin.comptime_only() = false    G |- args >> args'
-----------------------------------------------------
G |- (builtin args) >> (builtin args')
```

## Builtin Classification

| Builtin | Category | Evaluation |
|---------|----------|------------|
| `meta__is_struct`, `meta__struct_get_total_fields`, `meta__struct_get_field`, `error` | Comptime-only | Always fully evaluated |
| `add`, `eq`, `mem__malloc`, `mem__write`, `mem__read` | Dual | Can be comptime or runtime |
| `io__input_size`, `io__input_copy`, `io__return_exit` | Runtime-only | Preserved in residual |

## Memory Model (Comptime)

Virtual memory with:
- `AllocationId`: identifies distinct allocations
- `offset`: i32 word offset within allocation
- Operations: `malloc(size)`, `read(ptr)`, `write(ptr, val)`

## Type Checking

- Struct field initialization: must match definition order exactly
- Function application: argument type must equal parameter type
- Struct types compared by `def_uuid` (nominal typing)

## Key Invariant

**Runtime closure bodies are always in partially-evaluated form.** This is enforced at closure creation time via `mkClosure`, ensuring that no matter how a closure is stored, passed, or retrieved, its body has all capturable comptime values resolved.
