# Sensei Core Builtins

## Meta Programming / Introspection related (`meta__*`)
- `meta__struct_get_field(type, i32) -> type [may_error]`
- `meta__is_struct(type) -> bool`
- `meta__struct_get_total_fields(type) -> bool [may_error]`
- `error(void) -> [error]`

## Math & Logic
- `add(x: T1, x: T2) -> T3`
    - `add(i32, i32) -> i32`
    - `add(memptr, i32) -> memptr`
- `eq(x: T, b: T) -> bool`
- `i32` (basic numeric type)
- `bool` (boolean `true` / `false`)

## Memory Management
- `memptr` (pointer type)
- `mem__malloc(i32) -> memptr`
- `mem__write(memptr, i32)`
- `mem__read(memptr) -> i32`

## IO / Runtime Only
- `io__input_size(void) -> i32`
- `io__input_copy(memptr, i32, i32)`
- `io__return_exit(memptr, i32)`
