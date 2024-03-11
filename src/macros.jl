# take a struct or mutable struct definition Expr
# and parse the type name (+ type parameters), potential subtyping
# field definitions, including: name, type, default value, and field tags
# if @noarg: generate noarg constructor applying field defaults + regular constructor
# if @kwdef: generate outer kwdef constructor applying field defaults
# if @defaults: no constructor generation
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
        return FieldExpr(isconst, isatomic, ex, none, none, none)
    elseif ex.head == :(::)
        # name::type
        return FieldExpr(isconst, isatomic, ex.args[1], ex.args[2], none, none)
    elseif ex.head == :(=)
        arg1 = ex.args[1]
        arg2 = ex.args[2]
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
    elseif ex.head == :call
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
    elseif ex.head == :const
        # const field_expr
        return FieldExpr(ex.args[1], true)
    elseif ex.head == :macrocall && ex.args[1] == Symbol("@atomic")
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

function parse_struct_def(kind, expr)
    # kind is: :noarg, :kwdef, :tags
    ret = Expr(:block)
    # we always want to return original struct definition expression
    push!(ret.args, expr)
    # parse field exprs
    #TODO: probably need to not parse inner constructors
    fields = [FieldExpr(ex) for ex in expr.args[3].args if !(ex isa LineNumberNode)]
    # replace field exprs w/ cleaned up; i.e. defaults & tags removed
    expr.args[3].args = [_expr(f) for f in fields]
    if kind == :noarg
        # generate noarg constructor
        nm = expr.args[2]
        cexpr = quote
            function $nm()
                x = new()
                # apply field defaults here
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
        push!(ret.args, :(Structs.noarg(::Type{$nm}) = true))
        # generate fieldtags override if applicable
        if any(f.tags !== none for f in fields)
            tags_nt = Expr(:tuple, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== Structs.none]...)
            push!(ret.args, :(Structs.fieldtags(::Type{$nm}) = $tags_nt))
        end
        return esc(ret)
    elseif kind == :kwdef
        # generate outer kwdef constructor, like: Foo(; a=1, b=2, ...) = Foo(a, b, ...)
        nm = expr.args[2]
        params = Expr(:parameters, (_kw(fex) for fex in fields)...)
        fexpr = quote
            function $nm()
                return $nm($((f.name for f in fields)...))
            end
        end
        # dump(fexpr)
        push!(fexpr.args[2].args[1].args, params)
        push!(ret.args, fexpr)
        # override Structs.kwdef(::Type{nm}) = true and add outside struct definition
        push!(ret.args, :(Structs.kwdef(::Type{$nm}) = true))
        # generate fielddefaults override
        defs_nt = Expr(:tuple, Expr(:parameters, [:(($(f.name)=$(f.default))) for f in fields if f.default !== none]...))
        push!(ret.args, :(Structs.fielddefaults(::Type{$nm}) = $defs_nt))
        # generate fieldtags override if applicable
        if any(f.tags !== none for f in fields)
            tags_nt = Expr(:tuple, Expr(:parameters, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== Structs.none]...))
            push!(ret.args, :(Structs.fieldtags(::Type{$nm}) = $tags_nt))
        end
        return esc(ret)
    elseif kind == :defaults
        nm = expr.args[2]
        # generate fielddefaults override
        defs_nt = Expr(:tuple, Expr(:parameters, [:(($(f.name)=$(f.default))) for f in fields if f.default !== none]...))
        push!(ret.args, :(Structs.fielddefaults(::Type{$nm}) = $defs_nt))
        # generate fieldtags override if applicable
        if any(f.tags !== none for f in fields)
            tags_nt = Expr(:tuple, Expr(:parameters, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== Structs.none]...))
            push!(ret.args, :(Structs.fieldtags(::Type{$nm}) = $tags_nt))
        end
        return esc(ret)
    elseif kind == :tags
        nm = expr.args[2]
        # generate fieldtags override
        tags_nt = Expr(:tuple, Expr(:parameters, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== Structs.none]...))
        push!(ret.args, :(Structs.fieldtags(::Type{$nm}) = $tags_nt))
        return esc(ret)
    else
        throw(ArgumentError("unsupported kind for Structs.jl macro: $kind"))
    end
end

macro noarg(expr)
    parse_struct_def(:noarg, expr)
end

macro kwdef(expr)
    parse_struct_def(:kwdef, expr)
end

macro defaults(expr)
    parse_struct_def(:defaults, expr)
end

macro tags(expr)
    parse_struct_def(:tags, expr)
end