Data types
==========

Hydromel has a [structural type system](https://en.wikipedia.org/wiki/Structural_type_system) with [subtyping](https://en.wikipedia.org/wiki/Subtyping). Built-in types and composite types are specified using *type constructors* such as `signed`, `unsigned`, `array`, etc.

A type constructor is a function that returns a type. Some type constructors take arguments: for instance, `unsigned(N)` returns the type of N-bit unsigned integers. When a type constructor has no argument, we usually omit the parentheses: `bit` is the same as `bit()` and returns the type of 1-bit unsigned integers.

Notations
---------

The notation `T <: U` means "`T` is a subtype of `U`", or "A value of type `T` is also a value of type `U`".
The subtype relation is a non-strict partial order relation:

* It is reflexive. A type `T` is a subtype of itself: `T <: T`.
* It is transitive: if `T <: U` and `U <: V`, then `T <: V`.
* It is antisymmetric: if `T <: U` and `U <: T`, then `T = U`.

The notation `x : T` means "`x` has type `T`".
It can be expressed as `typeof(x) <: T`.

Top and bottom types
--------------------

The constructors `any` and `none` are defined with the following subtyping rules:

* All types are subtypes of `any`.
* `none` is a subtype of all types. It is define as a `union` with an empty list of alternatives.

The type of types
-----------------

Hydromel defines the constructor `subtype` with the following meaning:

`T : subtype(U)` means that `T` is a type and `T <: U`.

The general constructor for types is: `type = subtype(any)`.

Since for all types, `T <: T`, we can always write `T : subtype(T)`.
We define `typeof(T) = subtype(T)`.

The expression `T : subtype(U)` can also be rewritten as:
`typeof(T) <: subtype(U)`, so `subtype(T) <: subtype(U)`

Integer types
-------------

Hydromel defines the following integer type constructors:

* `integer`: unbounded signed integers.
* `natural`: unbounded unsigned integers.
* `signed(N)` with `N : natural`: N-bit signed (2's complement) integers.
* `unsigned(N)` with `N : natural`: N-bit unsigned integers.

The corresponding types follow these subtyping relations:

* A natural is an integer: `natural <: integer`.
* An N-bit signed integer is an integer: `signed(N) <: integer`.
* An N-bit unsigned integer is a natural: `unsigned(N) <: natural`.
* If `N ≤ M`, then `signed(N) <: signed(M)`.
* If `N ≤ M`, then `unsigned(N) <: unsigned(M)`.
* An N-bit unsigned integer can be cast to a larger signed integer: if `N < M`, then `unsigned(N) <: signed(M)`.

The `bit` constructor is provided as an alias for the 1-bit unsigned integer type constructor:

`bit = unsigned(1)`

For an integer value `x`, `typeof(x)` returns:

* `unsigned(N)` if `x ≥ 0` with `N` the minimum number of bits to represent `x` as a pure binary value. If `x = 0` then `N = 1`, else, `N` is the integer value such that `2^(N-1) ≤ x < 2^N`.
* `signed(N)` if `x < 0` with `N` the minimum number of bits to represent `x` as a 2's complement binary value. If `x = -1` then `N = 1` else `N` is the integer value such that `-2^(N-1) ≤ x < -2^(N-2)`.

Symbol types
------------

A symbol `~x` has a singleton type `symbol(~x)`.

Symbol types are for internal use and should not be manipulated directly. They are a building block of record and enumeration types (see below).

Composite types
---------------

Composite types can be created using these constructors:

* `array(N, T)` with `N : natural` and `T : type`: defines an array type of length `N` and element type `T`.
* `tuple(T, ...)` with `T : type`: defines a tuple type where each element has the given type `T`.
* `record(K : T, ...)` with `K : symbol(K)` and `T : type`: defines a record type where each field is named `K` and has type `T`.
* `union(T, ...)` with `T : type`: defines a type as an *or* between the given types `T`.
* `range(T)` with `T : subtype(integer)`: defines a type for a range of values of an integer subtype.

Subtyping relations are:

* If `N ≥ M` and `T <: U`, then `array(N, T) <: array(M, U)`.
* If `N ≥ M` and `T(n) <: U(n)` for all `0 ≤ n < M`, then `tuple(T(0), ..., T(N-1)) <: tuple(U(0), ..., U(M-1))`.
* If `{K(0), ..., K(N-1)} ⊇ {L(0), ..., L(M-1)}` and `T(L(n)) <: U(L(n))` for all `0 ≤ n < M`, then `record(K(0) : T(K(0)), ..., K(N-1) : T(K(N-1))) <: record(L(0) : U(L(0)), ..., L(M-1) : U(L(M-1)))`.
* For all `0 ≤ n < N`, `T(n) <: union(T(0), ..., T(N-1))`.
* If `T(n) <: U` for all `0 ≤ n < N`, then `union(T(0), ..., T(N-1)) <: U`

Subtyping relations for arrays, tuples and records can seem counter-intuitive. While the types of their contents are covariant, their sizes follow an opposite relation. All locations accessible in a supertype must exist in its subtypes.

The `array` constructor supports a shortcut to create multidimensional array types:

`array(N, M, ..., T) = array(N, array(M, ..., T))`

The `enumeration` constructor allows to create a union between symbol types:

`enumeration(K, ...) = union(symbol(K), ...)`

The `range` constructor is used internally for expressions that use the `..` operator.

Constant type
-------------

The `const` constructor is used internally to represent the type of a value that is known at elaboration time. A `const` packs a value with its declared or computed type.

`const(x, T)` where `x : T`

It is used primarily when the return type of a function depends on the values of some arguments, such as in bit shifts, concatenation or slicing operations. In most cases, `typeof(x) = T` but there are situations where we want to enforce a more general type for `x`, so that `typeof(x) <: T`.
For instance, the result of a cast `unsigned(4)|3|` is of type `const(3, unsigned(4))` to specify that the value 3 must be processed as a 4-bit wide integer.

During type checking, `const(x, T) <: T`.

`const` types are *collapsed* to ordinary types using these rules:

* `collapse(const(x, integer)) = signed(signed_width(x))`
* `collapse(const(x, natural)) = unsigned(unsigned_width(x))`
* `collapse(const(x, T)) = T` otherwise.

Minimized forms
---------------

Types are *minimized* according to the following rules:

* `minimize(array(N, T)) = array(N, minimize(T))`
* `minimize(tuple(T, ...)) = tuple(minimize(T), ...)`
* `minimize(record(K : T, ...)) = record(K : minimize(T), ...)`
* `minimize(range(T)) = range(minimize(T))`
* `minimize(union(T, ...)) = common_supertype(minimize(T), ...)`
* `minimize(T) = T` in other cases.
