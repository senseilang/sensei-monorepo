# S-Expressions AST Format

This describes how given s-expressions should translate into the Sencore AST.

## Base Expressions
```
() => void constant
34 => number constant
true/false => boolean constant
<name> => variable reference (`Var(name)`)
```

## Compound Expressions

```
[function application] (e1 e2 ... e<N>) =>  (((e1 e2) e3) ...) e<N>
    - also: (apply e1 e2 ... e<N>) for explicit form
[function definition] (funcdef comptime? <bind:name> <type:expr> <body:expr>)
[recursive function definition] (recfuncdef <funcref:name> comptime? <bind:name> <type:expr> <body:expr>)
[if-else] (if <condition:expr> <true_branch:expr> <false_branch:expr>)
[block] (block (comptime? <let_bind:name> <bind_type:expr> <assigned:expr>)* <block_final_result:expr>)
[member-access] (attr <path_segment:name>* <struct_expr:expr>) =>
    ((struct_expr.segment[0]).segment[1]) ... .segment[n]
[struct definition] (struct_def
    <type_capture:expr>
    (<field_name:name> <type:expr>)*
)
[struct init] (struct_init <struct_type:expr> (<field:name> <value:expr>)*)
```

Note: Builtins are not part of the AST syntax. They are injected at evaluation time.

## Sensei Core Builtins

### Meta Programming / Introspection related (`meta__*`)
- `meta__struct_get_field(type, i32) -> type [may_error]`
- `meta__is_struct(type) -> bool`
- `meta__struct_get_total_fields(type) -> bool [may_error]`
- `meta__struct_get_field_value(type, i32) -> fn(type -> ?) [may_error]`
- `error(void) -> [error]`

### Math & Logic
- `add(x: T1, x: T2) -> T3`
    - `add(i32, i32) -> i32`
    - `add(memptr, i32) -> memptr`
- `eq(x: T, b: T) -> bool`
- `i32` (basic numeric type)
- `bool` (boolean `true` / `false`)

### Memory Management
- `memptr` (pointer type)
- `mem__malloc(i32) -> memptr`
- `mem__write(memptr, i32)`
- `mem__read(memptr) -> i32`

### IO / Runtime Only
- `io__input_size(void) -> i32`
- `io__input_copy(memptr, i32, i32)`
- `io__return_exit(memptr, i32)`
