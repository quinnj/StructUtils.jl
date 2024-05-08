# Structs

[![CI](https://github.com/quinnj/Structs.jl/workflows/CI/badge.svg)](https://github.com/quinnj/Structs.jl/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/quinnj/Structs.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/quinnj/Structs.jl)
[![deps](https://juliahub.com/docs/Structs/deps.svg)](https://juliahub.com/ui/Packages/Structs/HHBkp?t=2)
[![version](https://juliahub.com/docs/Structs/version.svg)](https://juliahub.com/ui/Packages/Structs/HHBkp)
[![pkgeval](https://juliahub.com/docs/Structs/pkgeval.svg)](https://juliahub.com/ui/Packages/Structs/HHBkp)

\*\*

## Installation

The package is registered in the [`General`](https://github.com/JuliaRegistries/General) registry and so can be installed at the REPL with `] add Structs`.

## Documentation

The primary interface provided by Structs.jl is in the form of the exported `@noarg`, `@defaults`, and `@tags` macros, along with the unexported (to avoid clashing with the Base definition) of `Structs.@kwdef`. These macros can be used on struct definitions to provide a more ergonomic and flexible way to define structs with default values, keyword constructors, and more.

The `@noarg` macro can be used to define a "no argument" constructor, and must be used with mutable structs. This allows
for programmatic construction and discovery of the supported behavior. Default values and field tags can also be defined in `@noarg` structs.

The `@defaults` macro can be used to define default values for fields in any kind of struct, and constructors will be defined that allow for the omission of fields with default values. Note that all fields with default values must be defined after any fields without default values.

The `@tags` macro can be used to define tags for fields in any kind of struct.

The `@kwdef` macro mirrors the functionality of the Base defintion, while also allowing for the inclusion of field tags.

The other major interface Structs.jl provides is the `Structs.make(T, source)` function. It allows programmatic construction of a type `T` from a variety of source objects.
For example, I could have a custom struct `Foo` and be able to construct an instance from an array of values, a dictionary, a database cursor, a JSON object, etc. This is done by allowing source objects to implement interfaces for how fields should be provided programmatically (the primary means being the `Structs.applyeach` function), while `Structs.make` uses the programmatic knowledge from the above-mentioned macros, along with potential field tags, to construct the object.

Additional documentation is forth-coming around how package developers can use the "under the hood" machinery of Structs.jl to provide a more flexible and ergonomic interface to their users, like custom serialization/deserialization, database interaction, etc.

## Contributing and Questions

Contributions are very welcome, as are feature requests and suggestions. Please open an
[issue][issues-url] if you encounter any problems or would just like to ask a question.

[ci-img]: https://github.com/quinnj/Structs.jl/workflows/CI/badge.svg
[ci-url]: https://github.com/quinnj/Structs.jl/actions?query=workflow%3ACI+branch%3Amaster
[codecov-img]: https://codecov.io/gh/quinnj/Structs.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/quinnj/Structs.jl
[issues-url]: https://github.com/quinnj/Structs.jl/issues
