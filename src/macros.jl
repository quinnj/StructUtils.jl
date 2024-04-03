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
    # don't include type to allow for more flexible inputs
    # and we can rely on implicit convert call in setfield!
    nm_typ = f.name # f.type === none ? f.name : :($(f.name)::$(f.type))
    return f.default === none ? nm_typ : Expr(:kw, nm_typ, f.default)
end

function FieldExpr(ex)
    isconst = isatomic = false
    name = Symbol()
    type = none
    default = none
    tags = none
    if Meta.isexpr(ex, :const)
        isconst = true
        ex = ex.args[1]
    end
    if Meta.isexpr(ex, :(=))
        def_and_tags = ex.args[2]
        if Meta.isexpr(def_and_tags, :call) && def_and_tags.args[1] == :&
            default = def_and_tags.args[2]
            tags = def_and_tags.args[3]
        else
            default = def_and_tags
        end
        ex = ex.args[1]
    end
    if Meta.isexpr(ex, :call) && ex.args[1] == :&
        tags = ex.args[3]
        ex = ex.args[2]
    end
    if Meta.isexpr(ex, :atomic)
        isatomic = true
        ex = ex.args[1]
    end
    if ex isa Symbol
        name = ex
    elseif Meta.isexpr(ex, :(::))
        name, type = ex.args
    else
        return nothing
    end
    name = Meta.isexpr(name, :escape) ? name.args[1] : name
    return FieldExpr(isconst, isatomic, name, type, default, tags)
end

function parsefields!(field_exprs::Vector{Any})
    fields = FieldExpr[]
    for (i, fex) in enumerate(field_exprs)
        fex isa LineNumberNode && continue
        f = FieldExpr(fex)
        if f !== nothing
            push!(fields, f)
            field_exprs[i] = _expr(f)
        else
            # ignore lines that aren't fields
        end
    end
    return fields
end

function parse_struct_def(kind, src, mod, expr)
    expr = macroexpand(mod, expr)
    Meta.isexpr(expr, :struct) || throw(ArgumentError("Invalid usage of @$kind macro"))
    ismutable, T, fieldsblock = expr.args
    if Meta.isexpr(T, :<:)
        T = T.args[1]
    end
    if Meta.isexpr(T, :curly)
        T_with_typeparams = copy(T)
        # sanitize T_with_typeparams to remove any type param bounds
        for i = 2:length(T_with_typeparams.args)
            if T_with_typeparams.args[i] isa Expr
                T_with_typeparams.args[i] = T_with_typeparams.args[i].args[1]
            end
        end
        typeparams = T.args[2:end]
        T = T.args[1]
    end
    # kind is: :noarg, :kwdef, :defaults, :tags
    ret = Expr(:block)
    # we always want to return original struct definition expression
    push!(ret.args, :($Base.@__doc__ $expr))
    # parse field exprs and sanitize field definitions
    fields = parsefields!(fieldsblock.args)
    if kind == :noarg
        ismutable || throw(ArgumentError("@noarg structs must be mutable"))
        if any(f.isconst for f in fields)
            #TODO: we could allow non-trailing const fields if they have default values
            # by setting the default value in the initial new() call
            # or if all fields have default values, we could allow const fields
            throw(ArgumentError("const fields are not allowed in @noarg structs"))
        end
        # generate noarg constructor
        if @isdefined(T_with_typeparams)
            sig = Expr(:where, Expr(:call, T_with_typeparams), typeparams...)
            new_expr = Expr(:(=), :x, Expr(:call, Expr(:curly, :new, T_with_typeparams.args[2:end]...)))
        else
            sig = Expr(:call, T)
            new_expr = :(x = new())
        end
        cexpr = Expr(:function, sig, Expr(:block, src, new_expr))
        defs = [:(setfield!(x, $(Meta.quot(f.name)), $(f.default), $(f.isatomic ? Meta.quot(:sequentially_consistent) : Meta.quot(:not_atomic)))) for f in fields if f.default !== none]
        append!(cexpr.args[2].args, defs)
        push!(cexpr.args[2].args, Expr(:return, :x))
        # add inner constructor right after field definitions
        push!(expr.args[3].args, cexpr)
        # override Structs.noarg(::Type{nm}) = true and add outside struct definition
        push!(ret.args, :(Structs.noarg(::Type{<:$T}) = true))
        generate_field_defaults_and_tags!(ret, T, fields)
    elseif kind == :kwdef
        if !isempty(fields)
            # generate outer kwdef constructor, like: Foo(; a=1, b=2, ...) = Foo(a, b, ...)
            params = Expr(:parameters, (_kw(fex) for fex in fields)...)
            sig = Expr(:call, T, params)
            fexpr = Expr(:function, sig, Expr(:block, src, :(return $T($((f.name for f in fields)...)))))
            push!(ret.args, fexpr)
            if @isdefined(T_with_typeparams)
                # generate another kwdef constructor with type parameters
                sig = Expr(:where, Expr(:call, T_with_typeparams, params), typeparams...)
                fexpr = Expr(:function, sig, Expr(:block, src, :(return $T_with_typeparams($((f.name for f in fields)...)))))
                push!(ret.args, fexpr)
            end
        end
        # override Structs.kwdef(::Type{T}) = true and add outside struct definition
        push!(ret.args, :(Structs.kwdef(::Type{<:$T}) = true))
        generate_field_defaults_and_tags!(ret, T, fields)
    else
        generate_field_defaults_and_tags!(ret, T, fields)
    end
    return esc(ret)
end

function generate_field_defaults_and_tags!(ret, T, fields)
    # generate fielddefaults override if applicable
    if any(f.default !== none for f in fields)
        defs_nt = Expr(:tuple, Expr(:parameters, [:(($(f.name)=$(f.default))) for f in fields if f.default !== none]...))
        push!(ret.args, :(Structs.fielddefaults(::Type{<:$T}) = $defs_nt))
    end
    # generate fieldtags override if applicable
    if any(f.tags !== none for f in fields)
        tags_nt = Expr(:tuple, Expr(:parameters, [:($(f.name)=$(f.tags)) for f in fields if f.tags !== none]...))
        push!(ret.args, :(Structs.fieldtags(::Type{<:$T}) = $tags_nt))
    end
end

macro noarg(expr)
    parse_struct_def(:noarg, __source__, __module__, expr)
end

macro kwdef(expr)
    parse_struct_def(:kwdef, __source__, __module__, expr)
end

macro defaults(expr)
    parse_struct_def(:defaults, __source__, __module__, expr)
end

macro tags(expr)
    parse_struct_def(:tags, __source__, __module__, expr)
end