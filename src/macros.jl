#TODO:
  # take from util.jl@kwdef to make more robust
  # enforce trailing field defaults for :defaults/:tags kind
  # take tests from base misc.jl
  # validate that tags is namedtuple
  # docs


# take a struct or mutable struct definition Expr
# and parse the type name (+ type parameters), potential subtyping
# field definitions, including: name, type, default value, and field tags
# if @noarg: generate noarg constructor applying field defaults + regular constructor
# if @kwdef: generate outer kwdef constructor applying field defaults
# if @defaults: generate outer constructor applying trailing field defaults, also check
  # for conforming trailing field defaults
# if @tags: generate fieldtags override if defaults/tags are present
# for all 3: generate fielddefaults and fieldtags overrides if defaults/tags are present
# Returns: original struct defintion expr + any constructors + fielddefaults + fieldtags
struct None end
const none = None()

struct FieldExpr
    isconst::Bool
    isatomic::Bool
    name::Symbol
    type::Any # none or Symbol or Expr
    default::Any # literal or Expr or none
    tags::Union{None, Expr}
end

function _expr(f::FieldExpr)
    nm_typ = f.type === none ? f.name : :($(f.name)::$(f.type))
    if f.isconst
        return Expr(:const, nm_typ)
    elseif f.isatomic
        return :(@atomic $nm_typ)
    else
        return nm_typ
    end
end

function _kw(f::FieldExpr)
    nm_typ = f.type === none ? f.name : :($(f.name)::$(f.type))
    return f.default === none ? nm_typ : Expr(:kw, nm_typ, f.default)
end

function FieldExpr(ex, isconst=false, isatomic=false)
    if ex isa Symbol
        # name
        return FieldExpr(isconst, isatomic, ex, none, none, none)
    elseif Meta.isexpr(ex, :(::))
        # name::type
        # Meta.unescape(ex.args[1])?
        return FieldExpr(isconst, isatomic, ex.args[1], ex.args[2], none, none)
    elseif Meta.isexpr(ex, :(=))
        arg1, arg2 = ex.args
        if arg1 isa Symbol
            if arg2 isa Expr && arg2.head == :call && arg2.args[1] == :&
                # name = default &tags
                return FieldExpr(isconst, isatomic, arg1, none, arg2.args[2], arg2.args[3])
            else
                # name = default
                return FieldExpr(isconst, isatomic, arg1, none, arg2, none)
            end
        elseif arg1.head == :(::)
            if arg2 isa Expr && arg2.head == :call && arg2.args[1] == :&
                # name::type = default &tags
                return FieldExpr(isconst, isatomic, arg1.args[1], arg1.args[2], arg2.args[2], arg2.args[3])
            else
                # name::type = default
                return FieldExpr(isconst, isatomic, arg1.args[1], arg1.args[2], arg2, none)
            end
        else
            throw(ArgumentError("unsupported field expression for Structs.jl macro: $ex"))
        end
    elseif Meta.isexpr(ex, :call)
        arg1 = ex.args[2]
        arg2 = ex.args[3]
        if arg1 isa Symbol
            # name &tags
            return FieldExpr(isconst, isatomic, arg1, none, none, arg2)
        elseif arg1.head == :(::)
            # name::type &tags
            return FieldExpr(isconst, isatomic, arg1.args[1], arg1.args[2], none, arg2)
        else
            throw(ArgumentError("unsupported field expression for Structs.jl macro: $ex"))
        end
    elseif Meta.isexpr(ex, :const)
        # const field_expr
        return FieldExpr(ex.args[1], true)
    elseif Meta.isexpr(ex, :atomic)
        # @atomic field_expr
        if length(ex.args) == 3
            return FieldExpr(ex.args[3], false, true)
        else
            arg1 = ex.args[3]
            tags = ex.args[4].args[1]
            if arg1 isa Symbol
                # @atomic name &tags
                return FieldExpr(false, true, arg1, none, none, tags)
            elseif arg1.head == :(::)
                # @atomic name::type &tags
                return FieldExpr(false, true, arg1.args[1], arg1.args[2], none, tags)
            elseif arg1.head == :(=)
                arg2 = arg1.args[2]
                arg1 = arg1.args[1]
                if arg1 isa Symbol
                    # @atomic name = default &tags
                    return FieldExpr(false, true, arg1, none, arg2, tags)
                elseif arg1.head == :(::)
                    # @atomic name::type = default &tags
                    return FieldExpr(false, true, arg1.args[1], arg1.args[2], arg2, tags)
                else
                    throw(ArgumentError("unsupported field expression for Structs.jl macro: $ex"))
                end
            else
                throw(ArgumentError("unsupported field expression for Structs.jl macro: $ex"))
            end
        end
    else
        throw(ArgumentError("unsupported field expression for Structs.jl macro: $ex"))
    end
end

function parse_struct_def(kind, mod, expr)
    expr = macroexpand(mod, expr)
    Meta.isexpr(expr, :struct) || throw(ArgumentError("Invalid usage of @$kind macro"))
    _, T, fieldsblock = expr.args
    if T isa Expr && T.head === :<:
        T = T.args[1]
    end
    # kind is: :noarg, :kwdef, :defaults, :tags
    ret = Expr(:block)
    # we always want to return original struct definition expression
    push!(ret.args, expr)
    # parse field exprs
    fields = [FieldExpr(ex) for ex in fieldsblock.args if !(ex isa LineNumberNode)]
    # replace field exprs w/ cleaned up; i.e. defaults & tags removed
    expr.args[3].args = [_expr(f) for f in fields]
    if kind == :noarg
        # generate noarg constructor
        cexpr = quote
            function $T()
                x = new()
                # we'll inject field defaults here
                return x
            end
        end
        # insert before last arg in cexpr.args[2].args[2].args
        defs = [:(setfield!(x, $(Meta.quot(f.name)), $(f.default))) for f in fields if f.default !== none]
        n = length(cexpr.args[2].args[2].args)
        splice!(cexpr.args[2].args[2].args, n:n-1, defs)
        # add inner constructor right after field definitions
        push!(expr.args[3].args, cexpr)
        # override Structs.noarg(::Type{nm}) = true and add outside struct definition
        push!(ret.args, :(Structs.noarg(::Type{<:$T}) = true))
        # generate fieldtags override if applicable
        if any(f.tags !== none for f in fields)
            tags_nt = Expr(:tuple, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== Structs.none]...)
            push!(ret.args, :(Structs.fieldtags(::Type{<:$T}) = $tags_nt))
        end
    elseif kind == :kwdef
        # generate outer kwdef constructor, like: Foo(; a=1, b=2, ...) = Foo(a, b, ...)
        params = Expr(:parameters, (_kw(fex) for fex in fields)...)
        fexpr = quote
            function $T()
                return $T($((f.name for f in fields)...))
            end
        end
        push!(fexpr.args[2].args[1].args, params)
        push!(ret.args, fexpr)
        # override Structs.kwdef(::Type{T}) = true and add outside struct definition
        push!(ret.args, :(Structs.kwdef(::Type{<:$T}) = true))
        # generate fielddefaults override
        defs_nt = Expr(:tuple, Expr(:parameters, [:(($(f.name)=$(f.default))) for f in fields if f.default !== none]...))
        push!(ret.args, :(Structs.fielddefaults(::Type{<:$T}) = $defs_nt))
        # generate fieldtags override if applicable
        if any(f.tags !== none for f in fields)
            tags_nt = Expr(:tuple, Expr(:parameters, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== none]...))
            push!(ret.args, :(Structs.fieldtags(::Type{<:$T}) = $tags_nt))
        end
    elseif kind == :defaults
        # generate fielddefaults override
        defs_nt = Expr(:tuple, Expr(:parameters, [:(($(f.name)=$(f.default))) for f in fields if f.default !== none]...))
        push!(ret.args, :(Structs.fielddefaults(::Type{<:$T}) = $defs_nt))
        # generate fieldtags override if applicable
        if any(f.tags !== none for f in fields)
            tags_nt = Expr(:tuple, Expr(:parameters, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== none]...))
            push!(ret.args, :(Structs.fieldtags(::Type{<:$T}) = $tags_nt))
        end
    elseif kind == :tags
        nm = expr.args[2]
        # generate fieldtags override
        tags_nt = Expr(:tuple, Expr(:parameters, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== Structs.none]...))
        push!(ret.args, :(Structs.fieldtags(::Type{<:$T}) = $tags_nt))
    else
        throw(ArgumentError("unsupported kind for Structs.jl macro: $kind"))
    end
    return esc(:($Base.@__doc__ $ret))
end

macro noarg(expr)
    parse_struct_def(:noarg, __module__, expr)
end

macro kwdef(expr)
    parse_struct_def(:kwdef, __module__, expr)
end

macro defaults(expr)
    parse_struct_def(:defaults, __module__, expr)
end

macro tags(expr)
    parse_struct_def(:tags, __module__, expr)
end