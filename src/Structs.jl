module Structs

using Dates, UUIDs

export @noarg, @defaults, @tags

abstract type StructStyle end
struct DefaultStyle <: StructStyle end

include("macros.jl")

dictlike(::Type{<:AbstractDict}) = true
dictlike(::Type{<:AbstractVector{<:Pair}}) = true
dictlike(_) = false
dictlike(_, ::Type{T}) where {T} = dictlike(T)
dictlike(st, ::T) where {T} = dictlike(st, T)

noarg(_) = false
noarg(_, ::Type{T}) where {T} = noarg(T)
noarg(st, ::T) where {T} = noarg(st, T)
kwdef(_) = false
kwdef(_, ::Type{T}) where {T} = kwdef(T)
kwdef(st, ::T) where {T} = kwdef(st, T)

fieldtags(::Type{T}) where {T} = (;)
fieldtags(_, ::Type{T}) where {T} = fieldtags(T)
fieldtags(::Type{T}, key) where {T} = haskey(fieldtags(T), key) ? fieldtags(T)[key] : nothing
fieldtags(st, ::Type{T}, key) where {T} = haskey(fieldtags(st, T), key) ? fieldtags(st, T)[key] : nothing
fielddefaults(::Type{T}) where {T} = (;)
fielddefaults(_, ::Type{T}) where {T} = fielddefaults(T)
fielddefault(::Type{T}, key) where {T} = haskey(fielddefaults(T), key) ? fielddefaults(T)[key] : nothing
fielddefault(st, ::Type{T}, key) where {T} = haskey(fielddefaults(st, T), key) ? fielddefaults(st, T)[key] : nothing

initialize(::Type{T}) where {T} = T()
initialize(AT::Type{<:AbstractArray{T, 0}}) where {T} = AT(undef)
initialize(::Type{T}) where {T <: AbstractVector} = T(undef, 0)
initialize(::Type{T}, dims) where {T <: AbstractArray} = T(undef, dims)
initialize(_, ::Type{T}) where {T} = initialize(T)
initialize(_, ::Type{T}, dims) where {T} = initialize(T, dims)

addkeyval!(d::AbstractDict, k, v) = d[k] = v
addkeyval!(d::AbstractVector, k, v) = push!(d, k => v)

_keytype(d) = keytype(d)
_keytype(::AbstractVector{Pair{A, B}}) where {A, B} = A
_valtype(d) = valtype(d)
_valtype(::AbstractVector{Pair{A, B}}) where {A, B} = B

arraylike(::Type{T}) where {T} = false
arraylike(::T) where {T} = arraylike(T)
arraylike(_, ::Type{T}) where {T} = arraylike(T)
arraylike(_, ::T) where {T} = arraylike(T)
arraylike(::Type{<:Union{AbstractArray, AbstractSet, Tuple, Base.Generator, Core.SimpleVector}}) = true
arraylike(::Type{<:AbstractArray{T, 0}}) where {T} = false

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
structlike(::Type{<:AbstractArray{T, 0}}) where {T} = false
structlike(::Module) = false
structlike(::Function) = false

nulllike(::Type{T}) where {T} = false
nulllike(::T) where {T} = nulllike(T)
nulllike(_, ::Type{T}) where {T} = nulllike(T)
nulllike(_, ::T) where {T} = nulllike(T)
nulllike(::Type{Nothing}) = true
nulllike(::Type{Missing}) = true

function lower end

lower(x) = x
lower(::StructStyle, x) = lower(x)

function lower(st::StructStyle, x, tags)
    if x isa Dates.TimeType && tags !== nothing && haskey(tags, :dateformat)
        return Dates.format(x, tags.dateformat)
    elseif tags !== nothing && haskey(tags, :lower)
        return tags.lower(x)
    else
        return lower(st, x)
    end
end

function lift end

lift(::Type{Symbol}, x) = Symbol(x)
lift(::Type{T}, x) where {T} = Base.issingletontype(T) ? T() : convert(T, x)
lift(::Type{>:Missing}, ::Nothing) = missing
lift(::Type{>:Nothing}, ::Nothing) = nothing
lift(::Type{>:Union{Missing, Nothing}}, ::Nothing) = missing
lift(::Type{>:Union{Missing, Nothing}}, ::Missing) = missing
lift(::Type{Char}, x::AbstractString) = length(x) == 1 ? x[1] : throw(ArgumentError("expected single character, got $x"))
lift(::Type{UUID}, x::AbstractString) = UUID(x)
lift(::Type{VersionNumber}, x::AbstractString) = VersionNumber(x)
lift(::Type{Regex}, x::AbstractString) = Regex(x)
lift(::Type{T}, x::AbstractString) where {T <: Dates.TimeType} = T(x)
function lift(::Type{T}, x, tags) where {T <: Dates.TimeType}
    if tags !== nothing && haskey(tags, :dateformat)
        return T(x, tags.dateformat)
    else
        return T(x)
    end
end
# bit of an odd case, but support 0-dimensional array lifting from scalar value
function lift(::Type{A}, x) where {A <: AbstractArray{T, 0}} where {T}
    m = A(undef)
    m[1] = lift(T, x)
    return m
end
@inline lift(::Type{T}, x, _) where {T} = lift(T, x)
@inline lift(::StructStyle, ::Type{T}, x, tags=nothing) where {T} = tags === nothing ? lift(T, x) : lift(T, x, tags)
@inline lift(f::F, st::StructStyle, ::Type{T}, x, tags=nothing) where {F, T} = tags === nothing ? f(lift(st, T, x)) : f(lift(st, T, x, tags))

function choosetype end

choosetype(::Type{T}, x) where {T} = (T >: Missing && !nulllike(x)) ? nonmissingtype(T) :
    (T >: Nothing && !nulllike(x)) ? Base.nonnothingtype(T) : T
@inline choosetype(::Type{T}, x, tags) where {T} = choosetype(T, x)

@inline choosetype(::StructStyle, ::Type{T}, x, tags=nothing) where {T} = tags === nothing ? choosetype(T, x) : choosetype(T, x, tags)
@inline choosetype(f, style::StructStyle, ::Type{T}, x, tags=nothing) where {T} = tags === nothing ? f(style, choosetype(style, T, x), x, nothing) : f(style, choosetype(style, T, x, tags), x, tags)

"""
    Structs.applyeach(f, x) -> Union{Structs.EarlyReturn, Nothing}

A custom `foreach`-like function that operates specifically on `(key, val)` or `(ind, val)` pairs,
and supports short-circuiting (via `Structs.EarlyReturn`).

For each key-value or index-value pair in `x`, call `f(k, v)`.
If `f` returns a `Structs.EarlyReturn` instance, `applyeach` should
return the `EarlyReturn` immediately and stop iterating (i.e. short-circuit).
Otherwise, the return value of `f` is ignored and iteration continues.

Key types are generally expected to be Symbols, Strings, or Integers.

An example overload of `applyeach` for a generic iterable would be:

```julia
function Structs.applyeach(f, x::MyIterable)
    for (i, v) in enumerate(x)
        ret = f(i, v)
        # if `f` returns EarlyReturn, return immediately
        ret isa Structs.EarlyReturn && return ret
    end
    return
end
```
"""
function applyeach end

"""
    Structs.EarlyReturn{T}

A wrapper type that can be used in function arguments to `applyeach`
to short-circuit iteration and return a value from `applyeach`.

Example usage:

```julia
function find_needle_in_haystack(haystack, needle)
    ret = applyeach(haystack) do k, v
        k == needle && return Structs.EarlyReturn(v)
    end
    ret isa Structs.EarlyReturn && return ret.value
    throw(ArgumentError("needle not found in haystack")
end
````
"""
struct EarlyReturn{T}
    value::T
end

@inline applyeach(f, x) = applyeach(DefaultStyle(), f, x)

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
@inline function applyeach(st::StructStyle, f, x::AbstractVector{Pair{K, V}}) where {K, V}
    for (k, v) in x
        ret = f(k, lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return
end

# appropriate definition for iterables that
# can't have #undef values
@inline function applyeach(st::StructStyle, f, x::Union{AbstractSet, Base.Generator, Core.SimpleVector})
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
                if ftags === nothing || !haskey(ftags, :ignore) || !ftags.ignore
                    fname = (ftags === nothing || !haskey(ftags, :name)) ? $fname : ftags.name
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
            if ftags === nothing || !haskey(ftags, :ignore) || !ftags.ignore
                fname = (ftags === nothing || !haskey(ftags, :name)) ? fname : ftags.name
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

struct KeyValStructClosure{T, S, V}
    style::S
    val::V # either actual T for noarg or Tuple of Refs otherwise and tuples
    i::Union{Base.RefValue{Int}, Nothing}
end

@inline KeyValStructClosure{T}(style::S, val::V) where {T, S, V} = KeyValStructClosure{T, S, V}(style, val, T <: Tuple ? Ref(0) : nothing)

struct NoArgFieldRef{T}
    val::T
    i::Int
end

@inline (f::NoArgFieldRef{T})(val::S) where {T, S} = setfield!(f.val, f.i, val)

struct FieldRef{T}
    val::T # Ref{T}
end

@inline (f::FieldRef{T})(val::S) where {T, S} = setindex!(f.val, val)

keyeq(a, b::String) = string(a) == b
keyeq(a::AbstractString, b::String) = String(a) == b

function (f::KeyValStructClosure{T, S, V})(key::K, val::VV) where {T, S, V, K, VV}
    if @generated
        N = fieldcount(T)
        ex = quote
            Base.@_inline_meta
            if T <: Tuple
                f.i[] += 1
            else
                tags = fieldtags(f.style, T)
            end
        end
        for i = 1:N
            ftype = fieldtype(T, i)
            if T <: Tuple
                push!(ex.args, quote
                    if f.i[] == $i
                        return make(FieldRef(f.val[$i]), f.style, $ftype, val)
                    end
                end)
            else
                fname = Meta.quot(fieldname(T, i))
                mexpr = quote
                    ftags = haskey(tags, $fname) ? tags[$fname] : nothing
                    if noarg(f.style, T)
                        return make(NoArgFieldRef(f.val, $i), f.style, $ftype, val, ftags)
                    else
                        return make(FieldRef(f.val[$i]), f.style, $ftype, val, ftags)
                    end
                end
                if K == Int
                    push!(ex.args, quote
                        if key == $i
                            $mexpr
                        end
                    end)
                elseif K == Symbol
                    push!(ex.args, quote
                        fname = haskey(tags, $fname) && haskey(tags[$fname], :name) ? tags[$fname].name : $fname
                        if key == fname
                            $mexpr
                        end
                    end)
                else
                    fstr = String(fieldname(T, i))
                    push!(ex.args, quote
                        fstr = haskey(tags, $fname) && haskey(tags[$fname], :name) ? String(tags[$fname].name) : $fstr
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
        error("bad generated function")
    end
end

function getfielddefault(style::S, ::Type{T}, key::Symbol) where {S, T}
    fd = fielddefault(style, T, key)
    if fd !== nothing
        return Ref(fd)
    else
        return Ref{Union{Nothing, fieldtype(T, key)}}(nothing)
    end
end

function makestruct(f::F, style::S, ::Type{T}, source) where {F, S, T}
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
            push!(ex.args, quote
                $nm = Ref{Union{Nothing, $(fieldtype(T, i))}}(fielddefault(style, $T, $fname))
            end)
        end
        if T <: Tuple
            vals = Expr(:tuple, Any[ :($(syms[i])) for i = 1:N ]...)
            push!(ex.args, :(st = applyeach(style, KeyValStructClosure{$T}(style, $vals), source)))
            push!(ex.args, :(x = $(Expr(:tuple, Any[ :($(syms[i])[]) for i = 1:N ]...))))
        else
            vals = Expr(:tuple, Any[ :($(syms[i])) for i = 1:N ]...)
            push!(ex.args, :(st = applyeach(style, KeyValStructClosure{$T}(style, $vals), source)))
            push!(ex.args, :(x = $(Expr(:new, :($T), Any[ :($(syms[i])[]) for i = 1:N ]...))))
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
        else
            f(T((vals[i][] for i = 1:fieldcount(T))...))
        end
        return st
    end
end

struct DictClosure{S, T}
    style::S
    dict::T
end

mutable struct DictKeyClosure{K}
    key::K
    DictKeyClosure{K}() where {K} = new{K}()
end

@inline (f::DictKeyClosure{K})(x) where {K} = setfield!(f, :key, x)

struct DictValClosure{D, K}
    dict::D # Dict instance
    key::DictKeyClosure{K}
end

@inline (f::DictValClosure{D, K})(x) where {D, K} = addkeyval!(f.dict, f.key.key, x)

@inline function (f::DictClosure{S, T})(k, v) where {S, T}
    KT = _keytype(f.dict)
    VT = _valtype(f.dict)
    kc = DictKeyClosure{KT}()
    make(kc, f.style, KT, k)
    return make(DictValClosure(f.dict, kc), f.style, VT, v)
end

struct ArrayClosure{S, T}
    style::S
    arr::T
end

struct ArrayPushClosure{T}
    arr::T
end

@inline (f::ArrayPushClosure{T})(x) where {T} = push!(f.arr, x)

function (f::ArrayClosure{S, T})(k, v) where {S, T}
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
        Structs.applyeach(lc, x)
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

struct MultiDimClosure{S, A}
    style::S
    arr::A
    dims::Vector{Int}
    cur_dim::Base.RefValue{Int}
end

@inline function (f::MultiDimClosure{S, A})(i, val) where {S, A}
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

struct MultiDimValFunc{S, A}
    style::S
    arr::A
    dims::Vector{Int}
end

@inline (f::MultiDimValFunc{S, A})(x) where {S, A} = setindex!(f.arr, x, f.dims...)

# programmatically construct T using noarg/kwdef/struct strategies and applyeach
make(::Type{T}, source; style::StructStyle=DefaultStyle(), tags=nothing) where {T} = make(style, T, source, tags)

mutable struct ValueClosure{T}
    x::T
    ValueClosure{T}() where {T} = new{T}()
end

@inline (f::ValueClosure{T})(x) where {T} = setfield!(f, :x, x)

struct MakeClosure{F}
    f::F
end

@inline (f::MakeClosure)(style::S, ::Type{T}, source::V, tags) where {S, T, V} = _make(f.f, style, T, source, tags)

@inline function make(style::StructStyle, ::Type{T}, source, tags=nothing) where {T}
    vc = ValueClosure{T}()
    choosetype(MakeClosure(vc), style, T, source, tags)
    return vc.x
end

@inline function make(f::F, style::StructStyle, ::Type{T}, source, tags=nothing) where {F, T}
    return choosetype(MakeClosure(f), style, T, source, tags)
end

# assume choosetype has been applied to T
# f is function to be applied to made value
# returns state from applyeach if any
function _make(f::F, style::StructStyle, ::Type{T}, source, tags=nothing) where {F, T}
    if dictlike(style, T)
        x = initialize(style, T)
        st = applyeach(style, DictClosure(style, x), source)
        f(x)
    elseif arraylike(style, T)
        if ndims(T) > 1
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

#TODO:
 # documentation: manual + docstrings

end
