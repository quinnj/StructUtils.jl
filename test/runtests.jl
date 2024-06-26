using Test, Dates, UUIDs, StructUtils

struct TestStyle <: StructUtils.StructStyle end

include(joinpath(dirname(pathof(StructUtils)), "../test/macros.jl"))
include(joinpath(dirname(pathof(StructUtils)), "../test/struct.jl"))

@testset "StructUtils" begin

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

println("Dict{Symbol, Int}")
@test StructUtils.make(Dict{Symbol, Int}, d) == d
@test StructUtils.make(Dict{Symbol, Int}, ds) == d
@test StructUtils.make(Dict{Symbol, Int}, nt) == d
@test StructUtils.make(Dict{Symbol, Int}, a) == d
@test StructUtils.make(Dict{Symbol, Int}, vp) == d
# @test StructUtils.make(Dict{Symbol, Int}, aa) == d # fails because of extra field
@test StructUtils.make(Dict{Symbol, Int}, v) == Dict(Symbol(1) => 1, Symbol(2) => 2, Symbol(3) => 3, Symbol(4) => 4)
@test StructUtils.make(Dict{Symbol, Int}, t) == Dict(Symbol(1) => 1, Symbol(2) => 2, Symbol(3) => 3, Symbol(4) => 4)
@test StructUtils.make(Dict{Symbol, Int}, b) == d
@test StructUtils.make(Dict{Symbol, Int}, bb) == d

println("NamedTuple")
@test StructUtils.make(typeof(nt), d) == nt
@test StructUtils.make(typeof(nt), ds) == nt
@test StructUtils.make(typeof(nt), nt) == nt
@test StructUtils.make(typeof(nt), a) == nt
@test StructUtils.make(typeof(nt), vp) == nt
@test StructUtils.make(typeof(nt), v) == nt
@test StructUtils.make(typeof(nt), t) == nt
@test StructUtils.make(typeof(nt), aa) == nt # extra field is ignored
@test StructUtils.make(typeof(nt), b) == nt
@test StructUtils.make(typeof(nt), bb) == nt

println("A struct")
@test StructUtils.make(A, d) == a
@test StructUtils.make(A, ds) == a # slower
@test StructUtils.make(A, nt) == a
@test StructUtils.make(A, a) == a
@test StructUtils.make(A, vp) == a
@test StructUtils.make(A, v) == a # relies on order of vector elements
@test StructUtils.make(A, t) == a # relies on order of tuple elements
@test StructUtils.make(A, aa) == a # extra field is ignored
@test StructUtils.make(A, b) == a
@test StructUtils.make(A, bb) == a

println("AA struct")
@test StructUtils.make(AA, d) == aa # works because of AA field e default
@test StructUtils.make(AA, ds) == aa
@test StructUtils.make(AA, nt) == aa
@test StructUtils.make(AA, a) == aa
@test StructUtils.make(AA, vp) == aa
@test StructUtils.make(AA, v) == aa # relies on order of vector elements
@test StructUtils.make(AA, t) == aa # relies on order of tuple elements
@test StructUtils.make(AA, aa) == aa # extra field is ignored
@test StructUtils.make(AA, b) == aa
@test StructUtils.make(AA, bb) == aa

println("B struct")
@test StructUtils.make(B, d) == b
@test StructUtils.make(B, ds) == b
@test StructUtils.make(B, nt) == b
@test StructUtils.make(B, a) == b
@test StructUtils.make(B, vp) == b
@test StructUtils.make(B, v) == b # relies on order of vector elements
@test StructUtils.make(B, t) == b # relies on order of tuple elements
@test StructUtils.make(B, aa) == b # extra field is ignored
@test StructUtils.make(B, b) == b
@test StructUtils.make(B, bb) == b

println("BB struct")
@test StructUtils.make(BB, d) == bb
@test StructUtils.make(BB, ds) == bb
@test StructUtils.make(BB, nt) == bb
@test StructUtils.make(BB, a) == bb
@test StructUtils.make(BB, vp) == bb
@test StructUtils.make(BB, v) == bb # relies on order of vector elements
@test StructUtils.make(BB, t) == bb # relies on order of tuple elements
@test StructUtils.make(BB, aa) == bb # extra field is ignored
@test StructUtils.make(BB, b) == bb
@test StructUtils.make(BB, bb) == bb

println("Vector Pair")
# @test StructUtils.make(typeof(vp), d) == vp # unordered Dict doesn't work
# @test StructUtils.make(typeof(vp), ds) == vp # unordered Dict doesn't work
@test StructUtils.make(typeof(vp), nt) == vp
@test StructUtils.make(typeof(vp), a) == vp
@test StructUtils.make(typeof(vp), vp) == vp

println("Vector")
# @test StructUtils.make(typeof(v), d) == v # relies on order of Dict elements
# @test StructUtils.make(typeof(v), ds) == v # relies on order of Dict elements
@test StructUtils.make(typeof(v), nt) == v
@test StructUtils.make(typeof(v), a) == v
@test StructUtils.make(typeof(v), vp) == v
@test StructUtils.make(typeof(v), v) == v
@test StructUtils.make(typeof(v), t) == v
# @test StructUtils.make(typeof(v), aa) == v # fails because of extra field

println("Tuple")
# @test StructUtils.make(typeof(t), d) == t # relies on order of Dict elements
# @test StructUtils.make(typeof(t), ds) == t # relies on order of Dict elements
@test StructUtils.make(typeof(t), nt) == t
@test StructUtils.make(typeof(t), a) == t
@test StructUtils.make(typeof(t), vp) == t
@test StructUtils.make(typeof(t), v) == t
@test StructUtils.make(typeof(t), t) == t
@test StructUtils.make(typeof(t), aa) == t

println("C")
@test StructUtils.make(C, ()) == C()
@test StructUtils.make(C, (1,)) == C()

println("D")
@test StructUtils.make(D, (1, 3.14, "hey")) == D(1, 3.14, "hey")

println("Wrapper")
@test StructUtils.make(Wrapper, Dict("x" => Dict("a" => 1, "b" => "hey"))) == Wrapper((a=1, b="hey"))

println("UndefGuy")
x = StructUtils.make(UndefGuy, (id=1, name="2"))
@test x.id == 1 && x.name == "2"
x = StructUtils.make(UndefGuy, (id=1,))
@test x.id == 1 && !isdefined(x, :name)

println("E")
@test StructUtils.make(E, Dict("id" => 1, "a" => (a=1, b=2, c=3, d=4))) == E(1, A(1, 2, 3, 4))

println("G")
@test StructUtils.make(G, Dict("id" => 1, "rate" => 3.14, "name" => "Jim", "f" => Dict("id" => 2, "rate" => 6.28, "name" => "Bob"))) == G(1, 3.14, "Jim", F(2, 6.28, "Bob"))

println("H")
x = StructUtils.make(H, (id=0, name="", properties=Dict("a" => 1), addresses=["a", "b", "c"]))
@test x.id == 0 && x.name == "" && x.properties == Dict("a" => 1) && x.addresses == ["a", "b", "c"]

println("I")
@test StructUtils.make(I, (id=2, name="Aubrey", fruit=apple)) == I(2, "Aubrey", apple)

println("Vehicle")
StructUtils.choosetype(::Type{Vehicle}, source) = source["type"] == "car" ? Car : Truck
x = StructUtils.make(Vehicle, Dict("type" => "car", "make" => "Toyota", "model" => "Corolla", "seatingCapacity" => 4, "topSpeed" => 120.5))
@test x == Car("Toyota", "Corolla", 4, 120.5)

println("J")
@test StructUtils.make(J, (id=1, name=nothing, rate=3.14)) == J(1, nothing, 3.14)
@test StructUtils.make(J, (id=nothing, name=nothing, rate=3)) == J(nothing, nothing, 3)

println("Recurs")
@test StructUtils.make(Recurs, (id=0, value=(id=1, value=(id=2, value=(id=3, value=(id=4, value=nothing)))))) == Recurs(0, Recurs(1, Recurs(2, Recurs(3, Recurs(4, nothing)))))

println("O")
@test StructUtils.make(O, (id=0, name=missing)) == O(0, missing)
@test StructUtils.make(O, (id=0, name=nothing)) == O(0, nothing)
@test StructUtils.make(O, (id=0, name=(id=2, first_name="Jim", rate=3.14))) == O(0, L(2, "Jim", 3.14))
@test StructUtils.make(O, (id=0, name=(id=2, name="Jane", fruit=banana))) == O(0, I(2, "Jane", banana))

println("P")
p = StructUtils.make(P, (id=0, name="Jane"))
@test p.id == 0 && p.name == "Jane"

end