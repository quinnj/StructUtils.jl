using Test, Dates, UUIDs, Structs

struct TestStyle <: Structs.StructStyle end

include(joinpath(dirname(pathof(Structs)), "../test/struct.jl"))

@testset "Structs" begin

# Dict{Symbol, Int}, NamedTuple, A struct, Vector Pair
d = Dict(:a => 1, :b => 2, :c => 3, :d => 4)
ds = Dict("a" => 1, "b" => 2, "c" => 3, "d" => 4)
nt = (a = 1, b = 2, c = 3, d = 4)
a = A(1, 2, 3, 4)
aa = AA(1, 2, 3, 4, 5)
b = B(); b.a = 1; b.b = 2; b.c = 3; b.d = 4; b
bb = BB()
vp = [:a => 1, :b => 2, :c => 3, :d => 4]
v = [1, 2, 3, 4]
t = (1, 2, 3, 4)

# Dict{Symbol, Int}
@test Structs.make(Dict{Symbol, Int}, d) == d
@test Structs.make(Dict{Symbol, Int}, ds) == d
@test Structs.make(Dict{Symbol, Int}, nt) == d
@test Structs.make(Dict{Symbol, Int}, a) == d
@test Structs.make(Dict{Symbol, Int}, vp) == d
# @test Structs.make(Dict{Symbol, Int}, aa) == d # fails because of extra field
@test Structs.make(Dict{Symbol, Int}, v) == Dict(Symbol(1) => 1, Symbol(2) => 2, Symbol(3) => 3, Symbol(4) => 4)
@test Structs.make(Dict{Symbol, Int}, t) == Dict(Symbol(1) => 1, Symbol(2) => 2, Symbol(3) => 3, Symbol(4) => 4)
@test Structs.make(Dict{Symbol, Int}, b) == d
@test Structs.make(Dict{Symbol, Int}, bb) == d

# # NamedTuple
@test Structs.make(typeof(nt), d) == nt
@test Structs.make(typeof(nt), ds) == nt
@test Structs.make(typeof(nt), nt) == nt
@test Structs.make(typeof(nt), a) == nt
@test Structs.make(typeof(nt), vp) == nt
@test Structs.make(typeof(nt), v) == nt
@test Structs.make(typeof(nt), t) == nt
@test Structs.make(typeof(nt), aa) == nt # extra field is ignored
@test Structs.make(typeof(nt), b) == nt
@test Structs.make(typeof(nt), bb) == nt

# # A struct
@test Structs.make(A, d) == a
@test Structs.make(A, ds) == a # slower
@test Structs.make(A, nt) == a
@test Structs.make(A, a) == a
@test Structs.make(A, vp) == a
@test Structs.make(A, v) == a # relies on order of vector elements
@test Structs.make(A, t) == a # relies on order of tuple elements
@test Structs.make(A, aa) == a # extra field is ignored
@test Structs.make(A, b) == a
@test Structs.make(A, bb) == a

# # AA struct
@test Structs.make(AA, d) == aa # works because of AA field e default
@test Structs.make(AA, ds) == aa
@test Structs.make(AA, nt) == aa
@test Structs.make(AA, a) == aa
@test Structs.make(AA, vp) == aa
@test Structs.make(AA, v) == aa # relies on order of vector elements
@test Structs.make(AA, t) == aa # relies on order of tuple elements
@test Structs.make(AA, aa) == aa # extra field is ignored
@test Structs.make(AA, b) == aa
@test Structs.make(AA, bb) == aa

# # B struct
@test Structs.make(B, d) == b
@test Structs.make(B, ds) == b
@test Structs.make(B, nt) == b
@test Structs.make(B, a) == b
@test Structs.make(B, vp) == b
@test Structs.make(B, v) == b # relies on order of vector elements
@test Structs.make(B, t) == b # relies on order of tuple elements
@test Structs.make(B, aa) == b # extra field is ignored
@test Structs.make(B, b) == b
@test Structs.make(B, bb) == b

# # BB struct
@test Structs.make(BB, d) == bb
@test Structs.make(BB, ds) == bb
@test Structs.make(BB, nt) == bb
@test Structs.make(BB, a) == bb
@test Structs.make(BB, vp) == bb
@test Structs.make(BB, v) == bb # relies on order of vector elements
@test Structs.make(BB, t) == bb # relies on order of tuple elements
@test Structs.make(BB, aa) == bb # extra field is ignored
@test Structs.make(BB, b) == bb
@test Structs.make(BB, bb) == bb

# # Vector Pair
# @test Structs.make(typeof(vp), d) == vp # unordered Dict doesn't work
# @test Structs.make(typeof(vp), ds) == vp # unordered Dict doesn't work
@test Structs.make(typeof(vp), nt) == vp
@test Structs.make(typeof(vp), a) == vp
@test Structs.make(typeof(vp), vp) == vp

# # Vector
# @test Structs.make(typeof(v), d) == v # relies on order of Dict elements
# @test Structs.make(typeof(v), ds) == v # relies on order of Dict elements
@test Structs.make(typeof(v), nt) == v
@test Structs.make(typeof(v), a) == v
@test Structs.make(typeof(v), vp) == v
@test Structs.make(typeof(v), v) == v
@test Structs.make(typeof(v), t) == v
# @test Structs.make(typeof(v), aa) == v # fails because of extra field

# # Tuple
# @test Structs.make(typeof(t), d) == t # relies on order of Dict elements
# @test Structs.make(typeof(t), ds) == t # relies on order of Dict elements
@test Structs.make(typeof(t), nt) == t
@test Structs.make(typeof(t), a) == t
@test Structs.make(typeof(t), vp) == t
@test Structs.make(typeof(t), v) == t
@test Structs.make(typeof(t), t) == t
@test Structs.make(typeof(t), aa) == t

@test Structs.make(C, ()) == C()
@test Structs.make(C, (1,)) == C()

@test Structs.make(D, (1, 3.14, "hey")) == D(1, 3.14, "hey")

@test Structs.make(Wrapper, Dict("x" => Dict("a" => 1, "b" => "hey"))) == Wrapper((a=1, b="hey"))

x = Structs.make(UndefGuy, (id=1, name="2"))
@test x.id == 1 && x.name == "2"
x = Structs.make(UndefGuy, (id=1,))
@test x.id == 1 && !isdefined(x, :name)

@test Structs.make(E, Dict("id" => 1, "a" => (a=1, b=2, c=3, d=4))) == E(1, A(1, 2, 3, 4))

@test Structs.make(G, (id=1, rate=3.14, name="Jim", f=(id=2, rate=6.28, name="Bob"))) == G(1, 3.14, "Jim", F(2, 6.28, "Bob"))

x = Structs.make(H, (id=0, name="", properties=Dict("a" => 1), addresses=["a", "b", "c"]))
@test x.id == 0 && x.name == "" && x.properties == Dict("a" => 1) && x.addresses == ["a", "b", "c"]

@test Structs.make(I, (id=2, name="Aubrey", fruit=apple)) == I(2, "Aubrey", apple)

Structs.choosetype(::Type{Vehicle}, source) = source["type"] == "car" ? Car : Truck
x = Structs.make(Vehicle, Dict("type" => "car", "make" => "Toyota", "model" => "Corolla", "seatingCapacity" => 4, "topSpeed" => 120.5))
@test x == Car("Toyota", "Corolla", 4, 120.5)

@test Structs.make(J, (id=1, name=nothing, rate=3.14)) == J(1, nothing, 3.14)
@test Structs.make(J, (id=nothing, name=nothing, rate=3)) == J(nothing, nothing, 3)

@test Structs.make(Recurs, (id=0, value=(id=1, value=(id=2, value=(id=3, value=(id=4, value=nothing)))))) == Recurs(0, Recurs(1, Recurs(2, Recurs(3, Recurs(4, nothing)))))

@test Structs.make(O, (id=0, name=missing)) == O(0, missing)
@test Structs.make(O, (id=0, name=nothing)) == O(0, nothing)
Structs.choosetype(::Type{O}, key, ::Type{T}, val) where {T} = key == :name ? (isnothing(val) ? Nothing : ismissing(val) ? Missing : haskey(val, :fruit) ? I : L) : T
@test Structs.make(O, (id=0, name=(id=2, first_name="Jim", rate=3.14))) == O(0, L(2, "Jim", 3.14))
@test Structs.make(O, (id=0, name=(id=2, name="Jane", fruit=banana))) == O(0, I(2, "Jane", banana))
#     @testset "macros" begin

#         ex = :(mutable struct Foo
#             no_type
#             with_type::Int
#             with_default = 1
#             with_type_default::Int = 1
#             with_tag &(xml=(key="with-tag",),)
#             with_tag_type::Int &(xml=(key="with-tag-type",),)
#             with_tag_default = 1 &(xml=(key="with-tag-default",),)
#             with_tag_type_default::Int = 1 &(xml=(key="with-tag-default",),)
#             @atomic no_type_atomic
#             @atomic with_type_atomic::Int
#             @atomic with_default_atomic = 1
#             @atomic with_type_default_atomic::Int = 1
#             @atomic with_tag_atomic &(xml=(key="with-tag-atomic",),)
#             @atomic with_tag_type_atomic::Int &(xml=(key="with-tag-type-atomic",),)
#             @atomic with_tag_default_atomic = 1 &(xml=(key="with-tag-default-atomic",),)
#             @atomic with_tag_type_default_atomic::Int = 1 &(xml=(key="with-tag-default-atomic",),)
#             const no_type_const
#             const with_type_const::Int
#             const with_default_const = 1
#             const with_type_default_const::Int = 1
#             const with_tag_const &(xml=(key="with-tag-const",),)
#             const with_tag_type_const::Int &(xml=(key="with-tag-type-const",),)
#             const with_tag_default_const = 1 &(xml=(key="with-tag-default-const",),)
#             const with_tag_type_default_const::Int = 1 &(xml=(key="with-tag-default-const",),)
#         end)

#     end
end