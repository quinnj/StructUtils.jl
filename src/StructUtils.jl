module StructUtils

using Dates, UUIDs

export @noarg, @defaults, @tags, Selectors

"""
    StructUtils.StructStyle

Abstract type that all concrete struct styles must subtype.
Custom struct styles allow fine-grained control over various
StructUtils.jl interface methods like `fieldtags`, `fielddefaults`,
`lift`, `lower`, etc.
"""
abstract type StructStyle end

"""
    StructUtils.DefaultStyle

Default struct style that all StructUtils.jl interface methods
are defined for by default.
"""
struct DefaultStyle <: StructStyle end

include("macros.jl")

"""
  StructUtils.dictlike(::Type{T}) -> Bool
  StructUtils.dictlike(::StructStyle, ::Type{T}) -> Bool

Returns `true` if `T` is a dictionary-like type, `false` otherwise.
When `StructUtils.make(T, source)` is called, if `dictlike(T)` is `true`,
an instance will be `initialize`d, and then `addkeyval!`ed for each
key-value pair in `source`.
"""
function dictlike end

dictlike(::Type{<:AbstractDict}) = true
dictlike(::Type{<:AbstractVector{<:Pair}}) = true
dictlike(_) = false
dictlike(_, ::Type{T}) where {T} = dictlike(T)
dictlike(st, ::T) where {T} = dictlike(st, T)

"""
  StructUtils.noarg(::Type{T}) -> Bool
  StructUtils.noarg(::StructStyle, ::Type{T}) -> Bool

Signals that `T` is a mutable type that can be constructed by calling an empty
constructor, like `t = T()`. Automatically overloaded when structs use the
`@noarg` macro in their struct definition. The default value is `false` unless
explicitly overloaded.
"""
function noarg end

noarg(_) = false
noarg(_, ::Type{T}) where {T} = noarg(T)
noarg(st, ::T) where {T} = noarg(st, T)

"""
  StructUtils.kwdef(::Type{T}) -> Bool
  StructUtils.kwdef(::StructStyle, ::Type{T}) -> Bool

Signals that `T` can be constructed by passing struct fields as keyword arguments
to the constructor, like `t = T(field1=a, field2=b, ...)`. Automatically overloaded
when structs use the `@kwdef` macro in their struct definition. The default value
is `false` unless explicitly overloaded.

Note that `StructUtils.@kwdef` currently has no relation to `Base.@kwdef`, yet should
be a drop-in replacement for it.
"""
function kwdef end

kwdef(_) = false
kwdef(_, ::Type{T}) where {T} = kwdef(T)
kwdef(st, ::T) where {T} = kwdef(st, T)

"""
    StructUtils.fieldtagkey(::Type{<:StructStyle}) -> Symbol

Field tags defined on struct fields can be grouped by keys that are associated with
a particular struct style. This function returns the key that should be used to
retrieve field tags for a given struct style. By default, this function returns
`nothing`. An example overload might look like:

```julia
struct MySQLStyle <: StructStyle end

StructUtils.fieldtagkey(::Type{MySQLStyle}) = :mysql

@tags struct Foo
    a::Int &(mysql=(name="foo_a",),)
    b::String
end
```

In this example, when `StructUtils.make` is called on `Foo` with the `MySQLStyle` style,
only `(name="foo_a",)` will be retrieved from the field tags for `a` because the
`mysql` key is associated with the `MySQLStyle` struct style.
"""
function fieldtagkey end

fieldtagkey(::Type{T}) where {T} = nothing

"""
    StructUtils.fieldtags(::Type{T}) -> NamedTuple
    StructUtils.fieldtags(::StructStyle, ::Type{T}) -> NamedTuple
    StructUtils.fieldtags(::StructStyle, ::Type{T}, fieldname) -> NamedTuple

Returns a `NamedTuple` of field tags for the struct `T`. Field tags can be
added manually by overloading `fieldtags`, or included via convenient syntax
using the StructUtils.jl macros: `@tags`, `@noarg`, `@defaults`, or `@kwdef`.
"""
function fieldtags end

fieldtags(::Type{T}) where {T} = (;)
fieldtags(_, ::Type{T}) where {T} = fieldtags(T)
fieldtags(::Type{T}, field) where {T} = get(() -> (;), fieldtags(T), field)

function fieldtags(st::StructStyle, ::Type{T}, field) where {T}
    ft = fieldtags(st, T)
    fft = get(() -> (;), ft, field)
    ftk = fieldtagkey(typeof(st))
    return ftk === nothing ? fft : get(() -> (;), fft, ftk)
end

"""
    StructUtils.fielddefaults(::Type{T}) -> NamedTuple
    StructUtils.fielddefaults(::StructStyle, ::Type{T}) -> NamedTuple
    StructUtils.fielddefault(::StructStyle, ::Type{T}, fieldname) -> NamedTuple

Returns a `NamedTuple` of field defaults for the struct `T`. Field defaults can be
added manually by overloading `fielddefaults`, or included via convenient syntax
using the StructUtils.jl macros: `@tags`, `@noarg`, `@defaults`, or `@kwdef`.
"""
function fielddefaults end

fielddefaults(::Type{T}) where {T} = (;)
fielddefaults(_, ::Type{T}) where {T} = fielddefaults(T)
fielddefault(::Type{T}, key) where {T} = haskey(fielddefaults(T), key) ? fielddefaults(T)[key] : nothing
fielddefault(st, ::Type{T}, key) where {T} = haskey(fielddefaults(st, T), key) ? fielddefaults(st, T)[key] : nothing
@doc (@doc fielddefaults) fielddefault

"""
    StructUtils.initialize(T) -> T
    StructUtils.initialize(T, dims) -> T
    StructUtils.initialize(::StructStyle, T) -> T
    StructUtils.initialize(::StructStyle, T, dims) -> T

In `StructUtils.make`, this function is called to initialize a new instance of `T`,
when `T` is `dictlike`, `arraylike`, or `noarg`. For `arraylike`, if the `source`
in `make` is discovered to have multiple dimensions, `dims` will be passed to
`initialize`. The default implementation of `initialize` is to call `T()` or `T(undef, dims...)`
for `<:AbstractArray` types.
"""
function initialize end

initialize(::Type{T}) where {T} = T()
initialize(AT::Type{<:AbstractArray{T,0}}) where {T} = AT(undef)
initialize(::Type{T}) where {T<:AbstractVector} = T(undef, 0)
initialize(::Type{T}, dims) where {T<:AbstractArray} = T(undef, dims)
initialize(_, ::Type{T}) where {T} = initialize(T)
initialize(_, ::Type{T}, dims) where {T} = initialize(T, dims)

"""
    StructUtils.addkeyval!(d, k, v)

Add a key-value pair to a dictionary-like object `d`. This function is called
by `StructUtils.make` when `d` is `dictlike`. The default implementation is to
call `d[k] = v` for `AbstractDict`.
"""
function addkeyval! end

addkeyval!(d::AbstractDict, k, v) = d[k] = v
addkeyval!(d::AbstractVector, k, v) = push!(d, k => v)

_keytype(d) = keytype(d)
_keytype(::AbstractVector{Pair{A,B}}) where {A,B} = A
_valtype(d) = valtype(d)
_valtype(::AbstractVector{Pair{A,B}}) where {A,B} = B

"""
  StructUtils.arraylike(::Type{T}) -> Bool
  StructUtils.arraylike(::StructStyle, ::Type{T}) -> Bool

Returns `true` if `T` is an array-like type, `false` otherwise. This function is
called by `StructUtils.make` to determine if `T` is array-like. The default
implementation returns `true` for `<:AbstractArray`, `<:AbstractSet`, `<:Tuple`,
`<:Base.Generator`, and `<:Core.SimpleVector` types, and `false` for `<:AbstractArray{T,0}`.

Once `initialize` is called, `StructUtils.make` will call `push!` to add values
to the array-like object.
"""
function arraylike end

arraylike(::Type{T}) where {T} = false
arraylike(::T) where {T} = arraylike(T)
arraylike(_, ::Type{T}) where {T} = arraylike(T)
arraylike(_, ::T) where {T} = arraylike(T)
arraylike(::Type{<:Union{AbstractArray,AbstractSet,Tuple,Base.Generator,Core.SimpleVector}}) = true
arraylike(::Type{<:AbstractArray{T,0}}) where {T} = false

"""
  StructUtils.structlike(::Type{T}) -> Bool
  StructUtils.structlike(::StructStyle, ::Type{T}) -> Bool

Returns `true` if `T` is a struct-like type, `false` otherwise. This function is
called by `StructUtils.make` to determine if `T` is struct-like. The default
implementation returns `true` for `isstructtype(T)` and `!Base.issingletontype(T)`.

`structlike` structs are expected to be able to be constructed by the default constructor
like `T(field1, field2, ...)`.

Due to how `StructUtils.make` works, `structlike` is often overloaded to `false` by "unit" types
where fields should be considered private the `make` process should instead attempt to
`lift` the `source` object into the `unit` type.
"""
function structlike end

structlike(::Type{T}) where {T} = isstructtype(T) && !Base.issingletontype(T)
structlike(_, ::Type{T}) where {T} = structlike(T)
structlike(::T) where {T} = structlike(T)
structlike(_, ::T) where {T} = structlike(T)
structlike(::Type{<:Dates.TimeType}) = false
structlike(::Type{<:AbstractString}) = false
structlike(::Type{Symbol}) = false
structlike(::Type{<:Number}) = false
structlike(::Type{<:AbstractChar}) = false
structlike(::Type{Nothing}) = false
structlike(::Type{Missing}) = false
structlike(::Type{UUID}) = false
structlike(::Type{VersionNumber}) = false
structlike(::Type{Regex}) = false
structlike(::Type{<:AbstractArray{T,0}}) where {T} = false
structlike(::Module) = false
structlike(::Function) = false

"""
  StructUtils.nulllike(T) -> Bool
  StructUtils.nulllike(::StructStyle, T) -> Bool

Returns `true` if `T` is a null-like type, `false` otherwise. This function is
mainly used in the default `choosetype` implementation to determine if a
`Union` type can be narrowed by excluding `nulllike` types like `Nothing` and `Missing`.
"""
function nulllike end

nulllike(::Type{T}) where {T} = false
nulllike(::T) where {T} = nulllike(T)
nulllike(_, ::Type{T}) where {T} = nulllike(T)
nulllike(_, ::T) where {T} = nulllike(T)
nulllike(::Type{Nothing}) = true
nulllike(::Type{Missing}) = true

"""
  StructUtils.lower(x) -> x
  StructUtils.lower(::StructStyle, x) -> x

Domain value transformation function. This function is called by
`StructUtils.applyeach` on each value in the `source` object before
calling the apply function. By default, `lower` is the identity function.
This allows a domain transformation of values according to the
style used.
"""
function lower end

lower(x) = x
lower(::StructStyle, x) = lower(x)

function lower(st::StructStyle, x, tags)
    if x isa Dates.TimeType && haskey(tags, :dateformat)
        return Dates.format(x, tags.dateformat)
    elseif haskey(tags, :lower)
        return tags.lower(x)
    else
        return lower(st, x)
    end
end

"""
  StructUtils.lift(::Type{T}, x) -> T
  StructUtils.lift(::StructStyle, ::Type{T}, x) -> T

Lifts a value `x` to a type `T`. This function is called by `StructUtils.make`
to lift values to the appropriate type. The default implementation is
the identity function for most types, but it also includes special cases
for `Symbol`, `Char`, `UUID`, `VersionNumber`, `Regex`, and `TimeType` types.
Allows transforming a "domain value" that may be some primitive representation
into a more complex Julia type.
"""
function lift end

lift(::Type{Symbol}, x) = Symbol(x)
lift(::Type{T}, x) where {T} = Base.issingletontype(T) ? T() : convert(T, x)
lift(::Type{>:Missing}, ::Nothing) = missing
lift(::Type{>:Nothing}, ::Nothing) = nothing
lift(::Type{>:Union{Missing,Nothing}}, ::Nothing) = nothing
lift(::Type{>:Union{Missing,Nothing}}, ::Missing) = missing
lift(::Type{Char}, x::AbstractString) = length(x) == 1 ? x[1] : throw(ArgumentError("expected single character, got $x"))
lift(::Type{UUID}, x::AbstractString) = UUID(x)
lift(::Type{VersionNumber}, x::AbstractString) = VersionNumber(x)
lift(::Type{Regex}, x::AbstractString) = Regex(x)
lift(::Type{T}, x::AbstractString) where {T<:Dates.TimeType} = T(x)
function lift(::Type{T}, x, tags) where {T<:Dates.TimeType}
    if haskey(tags, :dateformat)
        return T(x, tags.dateformat)
    else
        return T(x)
    end
end
# bit of an odd case, but support 0-dimensional array lifting from scalar value
function lift(::Type{A}, x) where {A<:AbstractArray{T,0}} where {T}
    m = A(undef)
    m[1] = lift(T, x)
    return m
end
@inline lift(::Type{T}, x, _) where {T} = lift(T, x)
@inline lift(::StructStyle, ::Type{T}, x) where {T} = lift(T, x)

@inline function lift(st::StructStyle, ::Type{T}, x, tags) where {T}
    if haskey(tags, :lift)
        return tags.lift(x)
    elseif !isempty(tags)
        return lift(T, x, tags)
    else
        return lift(st, T, x)
    end
end

@inline lift(f::F, st::StructStyle, ::Type{T}, x, tags) where {F,T} = f(lift(st, T, x, tags))

"""
    StructUtils.choosetype(::Type{T}, x) -> T
    StructUtils.choosetype(::StructStyle, ::Type{T}, x) -> T

Chooses a concrete type from an abstract or union type `T` based on the value `x`, where
`x` is the "source" object in the context of `StructUtils.make`.
This allows a runtime decision to be made around a concrete subtype
that should be used for `StructUtils.make` based on potentially dynamic values
of the source object.
"""
function choosetype end

choosetype(::Type{T}, x) where {T} = (T >: Missing && !nulllike(x)) ? nonmissingtype(T) :
                                     (T >: Nothing && !nulllike(x)) ? Base.nonnothingtype(T) : T
@inline choosetype(::Type{T}, x, tags) where {T} = choosetype(T, x)
@inline choosetype(::StructStyle, ::Type{T}, x) where {T} = choosetype(T, x)

@inline function choosetype(st::StructStyle, ::Type{T}, x, tags) where {T}
    if haskey(tags, :choosetype)
        return tags.choosetype(x)
    elseif !isempty(tags)
        return choosetype(T, x, tags)
    else
        return choosetype(st, T, x)
    end
end

@inline choosetype(f, style::StructStyle, ::Type{T}, x, tags) where {T} = f(style, choosetype(style, T, x, tags), x, tags)

"""
    StructUtils.applyeach(style, f, x) -> Union{StructUtils.EarlyReturn, Nothing}

A custom `foreach`-like function that operates specifically on `(key, val)` or `(ind, val)` pairs,
and supports short-circuiting (via `StructUtils.EarlyReturn`). It also supports a `StructStyle` argument
to allow for style-specific behavior for non-owned types.

For each key-value or index-value pair in `x`, call `f(k, v)`.
If `f` returns a `StructUtils.EarlyReturn` instance, `applyeach` should
return the `EarlyReturn` immediately and stop iterating (i.e. short-circuit).
Otherwise, the return value of `f` is ignored and iteration continues.

Key types are generally expected to be Symbols, Strings, or Integers.

An example overload of `applyeach` for a generic iterable would be:

```julia
function StructUtils.applyeach(style::StructUtils.StructStyle, f, x::MyIterable)
    for (i, v) in enumerate(x)
        ret = f(i, StructUtils.lower(style, v))
        # if `f` returns EarlyReturn, return immediately
        ret isa StructUtils.EarlyReturn && return ret
    end
    return
end
```

Note that `applyeach` must include the `style` argument when overloading,
even though it can be _called_ with out it, where the `DefaultStyle` will be used.

Also note that before applying `f`, the value `v` is passed through `StructUtils.lower(style, v)`.

If a value is `#undef` or otherwise not defined, the `f` function should be called with `nothing`.
"""
function applyeach end

"""
    StructUtils.EarlyReturn{T}

A wrapper type that can be used in function arguments to `applyeach`
to short-circuit iteration and return a value from `applyeach`.

Example usage:

```julia
function find_needle_in_haystack(haystack, needle)
    ret = applyeach(haystack) do k, v
        k == needle && return StructUtils.EarlyReturn(v)
    end
    ret isa StructUtils.EarlyReturn && return ret.value
    throw(ArgumentError("needle not found in haystack")
end
````
"""
struct EarlyReturn{T}
    value::T
end

@inline applyeach(f, x) = applyeach(DefaultStyle(), f, x)
@inline applyeach(f, st::StructStyle, x) = applyeach(st, f, x)

@inline function applyeach(st::StructStyle, f, x::AbstractArray)
    for i in eachindex(x)
        ret = if @inbounds(isassigned(x, i))
            f(i, lower(st, @inbounds(x[i])))
        else
            f(i, lower(st, nothing))
        end
        ret isa EarlyReturn && return ret
    end
    return length(x)
end

# special-case Pair vectors to act like Dicts
@inline function applyeach(st::StructStyle, f, x::AbstractVector{Pair{K,V}}) where {K,V}
    for (k, v) in x
        ret = f(k, lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return
end

# appropriate definition for iterables that
# can't have #undef values
@inline function applyeach(st::StructStyle, f, x::Union{AbstractSet,Base.Generator,Core.SimpleVector})
    for (i, v) in enumerate(x)
        ret = f(i, lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return
end

# generic definition for Tuple, NamedTuple, structs
function applyeach(st::StructStyle, f, x::T) where {T}
    if @generated
        N = fieldcount(T)
        ex = quote
            Base.@_inline_meta
            defs = fielddefaults(st, T)
        end
        for i = 1:N
            fname = Meta.quot(fieldname(T, i))
            push!(ex.args, quote
                ftags = fieldtags(st, T, $fname)
                if !haskey(ftags, :ignore) || !ftags.ignore
                    fname = get(ftags, :name, $fname)
                    ret = if isdefined(x, $i)
                        f(fname, lower(st, getfield(x, $i), ftags))
                    elseif haskey(defs, $fname)
                        # this branch should be really rare because we should
                        # have applied a field default in the struct constructor
                        f(fname, lower(st, defs[$fname], ftags))
                    else
                        f(fname, lower(st, nothing, ftags))
                    end
                    ret isa EarlyReturn && return ret
                end
            end)
        end
        push!(ex.args, :(return))
        return ex
    else
        defs = fielddefaults(st, T)
        for i = 1:fieldcount(T)
            fname = fieldname(T, i)
            ftags = fieldtags(st, T, fname)
            if !haskey(ftags, :ignore) || !ftags.ignore
                fname = get(ftags, :name, fname)
                ret = if isdefined(x, i)
                    f(fname, lower(st, getfield(x, i), ftags))
                elseif haskey(defs, fname)
                    f(fname, lower(st, defs[fname], ftags))
                else
                    f(fname, lower(st, nothing, ftags))
                end
                ret isa EarlyReturn && return ret
            end
        end
        return
    end
end

@inline function applyeach(st::StructStyle, f, x::AbstractDict)
    for (k, v) in x
        ret = f(k, lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return
end

struct KeyValStructClosure{T,S,V}
    style::S
    val::V # either actual T for noarg or Tuple of Refs otherwise and tuples
    i::Union{Base.RefValue{Int},Nothing}
end

KeyValStructClosure{T}(style::S, val::V) where {T,S,V} = KeyValStructClosure{T,S,V}(style, val, T <: Tuple ? Ref(0) : nothing)

struct NoArgFieldRef{T}
    val::T
    i::Int
end

(f::NoArgFieldRef{T})(val::S) where {T,S} = setfield!(f.val, f.i, val, Base.isfieldatomic(T, f.i) ? :sequentially_consistent : :not_atomic)

mutable struct FieldRef{T}
    set::Bool
    val::T
    FieldRef{T}() where {T} = new{T}(false)
    FieldRef{T}(x) where {T} = new{T}(true, x)
end

(f::FieldRef{T})(val) where {T} = (f.set = true; f.val = val)
Base.getindex(f::FieldRef) = f.set ? f.val : nothing

keyeq(a, b::String) = string(a) == b
keyeq(a::AbstractString, b::String) = String(a) == b
keyeq(a, b) = isequal(a, b)
keyeq(x) = y -> keyeq(x, y)

function (f::KeyValStructClosure{T,S,V})(key::K, val::VV) where {T,S,V,K,VV}
    if @generated
        N = fieldcount(T)
        ex = quote
            Base.@_inline_meta
            if T <: Tuple
                f.i[] += 1
            end
        end
        for i = 1:N
            ftype = fieldtype(T, i)
            if T <: Tuple
                push!(ex.args, quote
                    if f.i[] == $i
                        return make(f.val[$i], f.style, $ftype, val)
                    end
                end)
            else
                fname = Meta.quot(fieldname(T, i))
                mexpr = quote
                    if noarg(f.style, T)
                        return make(NoArgFieldRef(f.val, $i), f.style, $ftype, val, ftags)
                    else
                        return make(f.val[$i], f.style, $ftype, val, ftags)
                    end
                end
                if K == Int
                    push!(ex.args, quote
                        if key == $i
                            ftags = fieldtags(f.style, T, $fname)
                            $mexpr
                        end
                    end)
                elseif K == Symbol
                    push!(ex.args, quote
                        ftags = fieldtags(f.style, T, $fname)
                        fname = get(ftags, :name, $fname)
                        if key == fname
                            $mexpr
                        end
                    end)
                else
                    fstr = String(fieldname(T, i))
                    push!(ex.args, quote
                        ftags = fieldtags(f.style, T, $fname)
                        fstr = String(get(ftags, :name, $fstr))
                        if keyeq(key, fstr)
                            $mexpr
                        end
                    end)
                end
            end
        end
        # Core.println(ex)
        return ex
    else
        if T <: Tuple
            i = f.i[] += 1
            if i <= fieldcount(T)
                return make(f.val[i], f.style, fieldtype(T, i), val)
            end
        else
            for i = 1:fieldcount(T)
                ftype = fieldtype(T, i)
                if K == Int
                    if key == i
                        fname = fieldname(T, key)
                        ftags = fieldtags(f.style, T, fname)
                        if noarg(f.style, T)
                            return make(NoArgFieldRef(f.val, i), f.style, ftype, val, ftags)
                        else
                            return make(f.val[i], f.style, ftype, val, ftags)
                        end
                    end
                elseif K == Symbol
                    fname = fieldname(T, i)
                    ftags = fieldtags(f.style, T, fname)
                    fname = get(ftags, :name, fname)
                    if key == fname
                        if noarg(f.style, T)
                            return make(NoArgFieldRef(f.val, i), f.style, ftype, val, ftags)
                        else
                            return make(f.val[i], f.style, ftype, val, ftags)
                        end
                    end
                else # K == String
                    fname = fieldname(T, i)
                    ftags = fieldtags(f.style, T, fname)
                    fstr = String(fname)
                    fstr = String(get(ftags, :name, fstr))
                    if keyeq(key, fstr)
                        if noarg(f.style, T)
                            return make(NoArgFieldRef(f.val, i), f.style, ftype, val, ftags)
                        else
                            return make(f.val[i], f.style, ftype, val, ftags)
                        end
                    end
                end
            end
        end
    end
end

function getfielddefault(style::S, ::Type{T}, key) where {S,T}
    fd = fielddefault(style, T, key)
    if fd !== nothing
        return FieldRef{fieldtype(T, key)}(fd)
    else
        return FieldRef{fieldtype(T, key)}()
    end
end

function makestruct(f::F, style::S, ::Type{T}, source) where {F,S,T}
    if @generated
        N = fieldcount(T)
        ex = quote
            Base.@_inline_meta
        end
        syms = Symbol[]
        for i = 1:N
            nm = gensym(T <: Tuple ? :tuple : fieldname(T, i))
            push!(syms, nm)
            fname = Meta.quot(fieldname(T, i))
            FT = fieldtype(T, i)
            push!(ex.args, quote
                fd = fielddefault(style, $T, $fname)
                $nm = fd === nothing ? FieldRef{$FT}() : FieldRef{$FT}(fd)
            end)
        end
        if T <: Tuple
            vals = Expr(:tuple, Any[:($(syms[i])) for i = 1:N]...)
            push!(ex.args, :(st = applyeach(style, KeyValStructClosure{$T}(style, $vals), source)))
            push!(ex.args, :(x = $(Expr(:tuple, Any[:($(syms[i])[]) for i = 1:N]...))))
        else
            vals = Expr(:tuple, Any[:($(syms[i])) for i = 1:N]...)
            push!(ex.args, :(st = applyeach(style, KeyValStructClosure{$T}(style, $vals), source)))
            push!(ex.args, :(x = $(Expr(:new, :($T), Any[:($(syms[i])[]) for i = 1:N]...))))
        end
        push!(ex.args, :(f(x)))
        push!(ex.args, :(return st))
        # println(ex)
        return ex
    else
        # println("non-generated makestruct")
        vals = ntuple(i -> getfielddefault(style, T, fieldname(T, i)), fieldcount(T))
        st = applyeach(style, KeyValStructClosure{T}(style, vals), source)
        if T <: Tuple
            f(ntuple(i -> vals[i][], fieldcount(T)))
        elseif T <: NamedTuple
            f(T(Tuple(vals[i][] for i = 1:fieldcount(T))))
        else
            f(T((vals[i][] for i = 1:fieldcount(T))...))
        end
        return st
    end
end

struct DictClosure{S,T}
    style::S
    dict::T
end

mutable struct DictKeyClosure{K}
    key::K
    DictKeyClosure{K}() where {K} = new{K}()
end

@inline (f::DictKeyClosure{K})(x) where {K} = setfield!(f, :key, x)

struct DictValClosure{D,K}
    dict::D # Dict instance
    key::DictKeyClosure{K}
end

@inline (f::DictValClosure{D,K})(x) where {D,K} = addkeyval!(f.dict, f.key.key, x)

@inline function (f::DictClosure{S,T})(k, v) where {S,T}
    KT = _keytype(f.dict)
    VT = _valtype(f.dict)
    kc = DictKeyClosure{KT}()
    make(kc, f.style, KT, k)
    return make(DictValClosure(f.dict, kc), f.style, VT, v)
end

struct ArrayClosure{S,T}
    style::S
    arr::T
end

struct ArrayPushClosure{T}
    arr::T
end

@inline (f::ArrayPushClosure{T})(x) where {T} = push!(f.arr, x)

function (f::ArrayClosure{S,T})(k, v) where {S,T}
    return make(ArrayPushClosure(f.arr), f.style, eltype(f.arr), v)
end

struct LengthClosure
    len::Ptr{Int}
end

@inline (f::LengthClosure)(_, _) = unsafe_store!(f.len, unsafe_load(f.len) + 1)

function applylength(x)
    ref = Ref(0)
    lc = LengthClosure(Base.unsafe_convert(Ptr{Int}, ref))
    GC.@preserve ref begin
        StructUtils.applyeach(lc, x)
        return unsafe_load(lc.len)
    end
end

# recursively build up multidimensional array dimensions
# "[[1.0],[2.0]]" => (1, 2)
# "[[1.0,2.0]]" => (2, 1)
# "[[[1.0]],[[2.0]]]" => (1, 1, 2)
# "[[[1.0],[2.0]]]" => (1, 2, 1)
# "[[[1.0,2.0]]]" => (2, 1, 1)
# length of innermost array is 1st dim
function discover_dims(x)
    @assert arraylike(x)
    len = applylength(x)
    ret = applyeach(x) do _, v
        return arraylike(v) ? EarlyReturn(discover_dims(v)) : EarlyReturn(())
    end
    return (ret.value..., len)
end

struct MultiDimClosure{S,A}
    style::S
    arr::A
    dims::Vector{Int}
    cur_dim::Base.RefValue{Int}
end

@inline function (f::MultiDimClosure{S,A})(i, val) where {S,A}
    f.dims[f.cur_dim[]] = i
    if arraylike(val)
        f.cur_dim[] -= 1
        st = applyeach(f, val)
        f.cur_dim[] += 1
    else
        st = make(MultiDimValFunc(f.style, f.arr, f.dims), f.style, eltype(f.arr), val)
    end
    return st
end

struct MultiDimValFunc{S,A}
    style::S
    arr::A
    dims::Vector{Int}
end

@inline (f::MultiDimValFunc{S,A})(x) where {S,A} = setindex!(f.arr, x, f.dims...)

"""
    StructUtils.make(T, source) -> T
    StructUtils.make(style, T, source) -> T
    StructUtils.make(f, style, T, source) -> nothing
    StructUtils.make!(style, x::T, source)

Construct a struct of type `T` from `source` using the given `style`. The `source` can be any
type of object, and the `style` can be any `StructStyle` subtype.

`make` will use any knowledge of `noarg`, `arraylike`, or `dictlike` in order to
determine how to construct an instance of `T`. The fallback for structs is to rely on
the automatic "all argument" constructor that structs have defined by default.

`make` calls `applyeach` on the `source` object, where the key-value pairs
from `source` will be used in constructing `T`.

The 3rd definition above allows passing in an "applicator" function that is
applied to the constructed struct. This is useful when the initial `T` is
abstract or a union type and `choosetype` is used to determine the concrete
runtime type to construct.

The 4th definition allows passing in an already-constructed instance of `T` (`x`),,
which must be mutable, and source key-value pairs will be applied as
appropriate to `x`.

For structs, `fieldtags` will be accounted for and certain tags can be used
to influence the construction of the struct.
"""
function make end

make(::Type{T}, source; style::StructStyle=DefaultStyle()) where {T} = make(style, T, source)

mutable struct ValueClosure{T}
    x::T
    ValueClosure{T}() where {T} = new{T}()
end

@inline (f::ValueClosure{T})(x) where {T} = setfield!(f, :x, x)

struct MakeClosure{F}
    f::F
end

@inline (f::MakeClosure)(style::S, ::Type{T}, source::V, tags) where {S,T,V} = _make(f.f, style, T, source, tags)

@inline function make(style::StructStyle, ::Type{T}, source, tags=(;)) where {T}
    vc = ValueClosure{T}()
    choosetype(MakeClosure(vc), style, T, source, tags)
    return vc.x
end

@inline function make(f::F, style::StructStyle, ::Type{T}, source, tags=(;)) where {F,T}
    return choosetype(MakeClosure(f), style, T, source, tags)
end

# assume choosetype has been applied to T
# f is function to be applied to made value
# returns state from applyeach if any
function _make(f::F, style::StructStyle, ::Type{T}, source, tags=(;)) where {F,T}
    if dictlike(style, T)
        x = initialize(style, T)
        st = applyeach(style, DictClosure(style, x), source)
        f(x)
    elseif arraylike(style, T)
        if T <: Tuple # special-case Tuple since it's arraylike, but we want to "make" it like a struct
            st = makestruct(f, style, T, source)
        elseif ndims(T) > 1
            # multidimensional arrays
            dims = discover_dims(source)
            x = initialize(style, T, dims)
            n = ndims(T)
            st = applyeach(style, MultiDimClosure(style, x, ones(Int, n), Ref(n)), source)
            f(x)
        else
            x = initialize(style, T)
            st = applyeach(style, ArrayClosure(style, x), source)
            f(x)
        end
    elseif noarg(style, T)
        x = initialize(style, T)
        st = applyeach(style, KeyValStructClosure{T}(style, x), source)
        f(x)
    elseif structlike(style, T)
        st = makestruct(f, style, T, source)
    else
        st = lift(f, style, T, source, tags)
    end
    return st
end

make!(x::T, source; style::StructStyle=DefaultStyle()) where {T} = make!(style, x, source)
make!(::Type{T}, source; style::StructStyle=DefaultStyle()) where {T} = make!(style, T, source)

function make!(style::StructStyle, x::T, source) where {T}
    if dictlike(style, T)
        st = applyeach(DictClosure(style, x), source)
        return x
    elseif arraylike(style, T)
        st = applyeach(ArrayClosure(style, x), source)
        return x
    elseif noarg(style, T)
        st = applyeach(KeyValStructClosure{T}(style, x), source)
        return x
    else
        throw(ArgumentError("Type `$T` does not support in-place `make!`"))
    end
end

make!(style::StructStyle, ::Type{T}, source) where {T} = make!(style, initialize(style, T), source)

@doc (@doc make) make!

include("selectors.jl")

end
