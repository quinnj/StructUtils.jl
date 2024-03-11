struct A
    a::Int
    b::Int
    c::Int
    d::Int
end

struct AA
    a::Int
    b::Int
    c::Int
    d::Int
    e::Int
end

Structs.fielddefaults(::Type{AA}) = (e=5,)

mutable struct B
    a::Int
    b::Int
    c::Int
    d::Int
    B() = new()
end

Structs.noarg(::Type{B}) = true
Base.:(==)(b1::B, b2::B) = b1.a == b2.a && b1.b == b2.b && b1.c == b2.c && b1.d == b2.d

Base.@kwdef struct BB
    a::Int = 1
    b::Int = 2
    c::Int = 3
    d::Int = 4
end

Structs.kwdef(::Type{BB}) = true

struct C
end

struct D
    a::Int
    b::Float64
    c::String
end

struct LotsOfFields
    x1::String
    x2::String
    x3::String
    x4::String
    x5::String
    x6::String
    x7::String
    x8::String
    x9::String
    x10::String
    x11::String
    x12::String
    x13::String
    x14::String
    x15::String
    x16::String
    x17::String
    x18::String
    x19::String
    x20::String
    x21::String
    x22::String
    x23::String
    x24::String
    x25::String
    x26::String
    x27::String
    x28::String
    x29::String
    x30::String
    x31::String
    x32::String
    x33::String
    x34::String
    x35::String
end

struct Wrapper
    x::NamedTuple{(:a, :b), Tuple{Int, String}}
end

mutable struct UndefGuy
    id::Int
    name::String
    UndefGuy() = new()
end

Structs.noarg(::Type{UndefGuy}) = true

struct E
    id::Int
    a::A
end

Base.@kwdef struct F
    id::Int
    rate::Float64
    name::String
end

Structs.kwdef(::Type{F}) = true

Base.@kwdef struct G
    id::Int
    rate::Float64
    name::String
    f::F
end

Structs.kwdef(::Type{G}) = true

struct H
    id::Int
    name::String
    properties::Dict{String, Any}
    addresses::Vector{String}
end

@enum Fruit apple banana

struct I
    id::Int
    name::String
    fruit::Fruit
end

abstract type Vehicle end

struct Car <: Vehicle
    make::String
    model::String
    seatingCapacity::Int
    topSpeed::Float64
end

struct Truck <: Vehicle
    make::String
    model::String
    payloadCapacity::Float64
end

struct J
    id::Union{Int, Nothing}
    name::Union{String, Nothing}
    rate::Union{Int64, Float64}
end

struct K
    id::Int
    value::Union{Float64, Missing}
end

Base.@kwdef struct System
    duration::Real = 0 # mandatory
    cwd::Union{Nothing, String} = nothing
    environment::Union{Nothing, Dict} = nothing
    batch::Union{Nothing, Dict} = nothing
    shell::Union{Nothing, Dict} = nothing
end

Structs.kwdef(::Type{System}) = true

struct L
    id::Int
    first_name::String
    rate::Float64
end

# JSONBase.fields(::Type{L}) = (
#     first_name = (jsonkey="firstName",),
#     rate = (default=33.3,)
# )

struct ThreeDates
    date::Date
    datetime::DateTime
    time::Time
end

# function JSONBase.lift(::Type{ThreeDates}, key::Symbol, val)
#     if key == :date
#         return Date(val, dateformat"yyyy_mm_dd")
#     elseif key == :datetime
#         return DateTime(val, dateformat"yyyy/mm/dd HH:MM:SS")
#     elseif key == :time
#         return Time(val, dateformat"HH/MM/SS")
#     end
#     return val
# end

struct M
    id::Int
    value::Union{Nothing,K}
end

struct Recurs
    id::Int
    value::Union{Nothing,Recurs}
end

struct N
    id::Int
    uuid::UUID
end

struct O
    id::Int
    name::Union{I,L,Missing,Nothing}
end

# @testset "JSONBase.materialize" begin
#     obj = JSONBase.materialize("""{ "a": 1,"b": 2,"c": 3,"d": 4}""", A)
#     @test obj == A(1, 2, 3, 4)
#     # test order doesn't matter
#     obj2 = JSONBase.materialize("""{ "d": 1,"b": 2,"c": 3,"a": 4}""", A)
#     @test obj2 == A(4, 2, 3, 1)
#     # NamedTuple
#     obj = JSONBase.materialize("""{ "d": 1,"b": 2,"c": 3,"a": 4}""", NamedTuple{(:a, :b, :c, :d), Tuple{Int, Int, Int, Int}})
#     @test obj == (a = 4, b = 2, c = 3, d = 1)
#     @test JSONBase.materialize("{}", C) === C()
#     # we also support materializing singleton from JSONBase.json output
#     @test JSONBase.materialize("\"C()\"", C) === C()
#     obj = JSONBase.materialize!("""{ "a": 1,"b": 2,"c": 3,"d": 4}""", B)
#     @test obj.a == 1 && obj.b == 2 && obj.c == 3 && obj.d == 4
#     obj = JSONBase.materialize("""{ "a": 1,"b": 2,"c": 3,"d": 4}""", B)
#     @test obj.a == 1 && obj.b == 2 && obj.c == 3 && obj.d == 4
#     obj = B()
#     JSONBase.materialize!("""{ "a": 1,"b": 2,"c": 3,"d": 4}""", obj)
#     @test obj.a == 1 && obj.b == 2 && obj.c == 3 && obj.d == 4
#     # can materialize json array into struct assuming field order
#     obj = JSONBase.materialize("""[1, 2, 3, 4]""", A)
#     @test obj == A(1, 2, 3, 4)
#     # must be careful though because we don't check that the array is the same length as the struct
#     @test_throws Any JSONBase.materialize("""[1, 2, 3, 4, 5]""", A)
#     @test_throws Any JSONBase.materialize("""[1, 2, 3]""", A)
#     # materialize singleton from empty json array
#     @test JSONBase.materialize("""[]""", C) == C()
#     # materialize mutable from json array
#     obj = JSONBase.materialize("""[1, 2, 3, 4]""", B)
#     @test obj.a == 1 && obj.b == 2 && obj.c == 3 && obj.d == 4
#     obj = B()
#     JSONBase.materialize!("""[1, 2, 3, 4]""", obj)
#     @test obj.a == 1 && obj.b == 2 && obj.c == 3 && obj.d == 4
#     # materialize kwdef from json array
#     obj = JSONBase.materialize("""[1, 3.14, "hey there sailor"]""", F)
#     @test obj == F(1, 3.14, "hey there sailor")
#     # materialize NamedTuple from json array
#     obj = JSONBase.materialize("""[1, 3.14, "hey there sailor"]""", NamedTuple{(:id, :rate, :name), Tuple{Int, Float64, String}})
#     @test obj == (id = 1, rate = 3.14, name = "hey there sailor")
#     # materialize Tuple from json array
#     obj = JSONBase.materialize("""[1, 3.14, "hey there sailor"]""", Tuple{Int, Float64, String})
#     @test obj == (1, 3.14, "hey there sailor")
#     obj = JSONBase.materialize("""{ "a": 1,"b": 2.0,"c": "3"}""", D)
#     @test obj == D(1, 2.0, "3")
#     obj = JSONBase.materialize("""{ "x1": "1","x2": "2","x3": "3","x4": "4","x5": "5","x6": "6","x7": "7","x8": "8","x9": "9","x10": "10","x11": "11","x12": "12","x13": "13","x14": "14","x15": "15","x16": "16","x17": "17","x18": "18","x19": "19","x20": "20","x21": "21","x22": "22","x23": "23","x24": "24","x25": "25","x26": "26","x27": "27","x28": "28","x29": "29","x30": "30","x31": "31","x32": "32","x33": "33","x34": "34","x35": "35"}""", LotsOfFields)
#     @test obj == LotsOfFields("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35")
#     obj = JSONBase.materialize("""{ "x": {"a": 1, "b": "2"}}""", Wrapper)
#     @test obj == Wrapper((a=1, b="2"))
#     obj = JSONBase.materialize!("""{ "id": 1, "name": "2"}""", UndefGuy)
#     @test obj.id == 1 && obj.name == "2"
#     obj = JSONBase.materialize!("""{ "id": 1}""", UndefGuy)
#     @test obj.id == 1 && !isdefined(obj, :name)
#     obj = JSONBase.materialize("""{ "id": 1, "a": {"a": 1, "b": 2, "c": 3, "d": 4}}""", E)
#     @test obj == E(1, A(1, 2, 3, 4))
#     obj = JSONBase.materialize("""{ "id": 1, "rate": 2.0, "name": "3"}""", F)
#     @test obj == F(1, 2.0, "3")
#     obj = JSONBase.materialize("""{ "id": 1, "rate": 2.0, "name": "3", "f": {"id": 1, "rate": 2.0, "name": "3"}}""", G)
#     @test obj == G(1, 2.0, "3", F(1, 2.0, "3"))
#     # Dict/Array fields
#     obj = JSONBase.materialize("""{ "id": 1, "name": "2", "properties": {"a": 1, "b": 2}, "addresses": ["a", "b"]}""", H)
#     @test obj.id == 1 && obj.name == "2" && obj.properties == Dict("a" => 1, "b" => 2) && obj.addresses == ["a", "b"]
#     # Enum
#     @test JSONBase.materialize("\"apple\"", Fruit) == apple
#     @test JSONBase.materialize("""{"id": 1, "name": "2", "fruit": "banana"}  """, I) == I(1, "2", banana)
#     # abstract type
#     JSONBase.choosetype(::Type{Vehicle}, x) = x.type[] == "car" ? Car : Truck
#     @test JSONBase.materialize("""{"type": "car","make": "Mercedes-Benz","model": "S500","seatingCapacity": 5,"topSpeed": 250.1}""", Vehicle) == Car("car", "Mercedes-Benz", "S500", 5, 250.1)
#     @test JSONBase.materialize("""{"type": "truck","make": "Isuzu","model": "NQR","payloadCapacity": 7500.5}""", Vehicle) == Truck("truck", "Isuzu", "NQR", 7500.5)
#     # union
#     @test JSONBase.materialize("""{"id": 1, "name": "2", "rate": 3}""", J) == J(1, "2", Int64(3))
#     @test JSONBase.materialize("""{"id": null, "name": null, "rate": 3.14}""", J) == J(nothing, nothing, 3.14)
#     # test K
#     @test JSONBase.materialize("""{"id": 1, "value": null}""", K) == K(1, missing)
#     # Real
#     @test JSONBase.materialize("""{"duration": 3600.0}""", System) == System(duration=3600.0)
#     # struct + jsonlines
#     for raw in [
#         """
#         { "a": 1,  "b": 3.14,  "c": "hey" }
#         { "a": 2,  "b": 6.28,  "c": "hi"  }
#         """,
#         # No newline at end
#         """
#         { "a": 1,  "b": 3.14,  "c": "hey" }
#         { "a": 2,  "b": 6.28,  "c": "hi"  }""",
#         # No newline, extra whitespace at end
#         """
#         { "a": 1,  "b": 3.14,  "c": "hey" }
#         { "a": 2,  "b": 6.28,  "c": "hi"  }   """,
#         # Whitespace at start of line
#         """
#           { "a": 1,  "b": 3.14,  "c": "hey" }
#           { "a": 2,  "b": 6.28,  "c": "hi"  }
#         """,
#         # Extra whitespace at beginning, end of lines, end of string
#         " { \"a\": 1,  \"b\": 3.14,  \"c\": \"hey\" }  \n" *
#         "  { \"a\": 2,  \"b\": 6.28,  \"c\": \"hi\"  }  \n  ",
#     ]
#         for nl in ("\n", "\r", "\r\n")
#             jsonl = replace(raw, "\n" => nl)
#             dss = JSONBase.materialize(jsonl, Vector{D}, jsonlines=true)
#             @test length(dss) == 2
#             @test dss[1].a == 1
#             @test dss[1].b == 3.14
#             @test dss[1].c == "hey"
#             @test dss[2].a == 2
#             @test dss[2].b == 6.28
#             @test dss[2].c == "hi"
#         end
#     end
#     # test L
#     @test JSONBase.materialize("""{"id": 1, "firstName": "george", "first_name": "harry"}""", L) == L(1, "george", 33.3)
#     # test Char
#     @test JSONBase.materialize("\"a\"", Char) == 'a'
#     @test JSONBase.materialize("\"\u2200\"", Char) == '∀'
#     @test_throws ArgumentError JSONBase.materialize("\"ab\"", Char)
#     # test UUID
#     @test JSONBase.materialize("\"ffffffff-ffff-ffff-ffff-ffffffffffff\"", UUID) == UUID(typemax(UInt128))
#     # test Symbol
#     @test JSONBase.materialize("\"a\"", Symbol) == :a
#     # test VersionNumber
#     @test JSONBase.materialize("\"1.2.3\"", VersionNumber) == v"1.2.3"
#     # test Regex
#     @test JSONBase.materialize("\"1.2.3\"", Regex) == r"1.2.3"
#     # test Dates
#     @test JSONBase.materialize("\"2023-02-23T22:39:02\"", DateTime) == DateTime(2023, 2, 23, 22, 39, 2)
#     @test JSONBase.materialize("\"2023-02-23\"", Date) == Date(2023, 2, 23)
#     @test JSONBase.materialize("\"22:39:02\"", Time) == Time(22, 39, 2)
#     @test JSONBase.materialize("{\"date\":\"2023_02_23\",\"datetime\":\"2023/02/23 12:34:56\",\"time\":\"12/34/56\"}", ThreeDates) ==
#         ThreeDates(Date(2023, 2, 23), DateTime(2023, 2, 23, 12, 34, 56), Time(12, 34, 56))
#     # test Array w/ lifted value
#     @test isequal(JSONBase.materialize("[null,null]", Vector{Missing}), [missing, missing])
#     # test Matrix
#     @test JSONBase.materialize("[[1,3],[2,4]]", Matrix{Int}) == [1 2; 3 4]
#     @test JSONBase.materialize("{\"a\": [[1,3],[2,4]]}", NamedTuple{(:a,),Tuple{Matrix{Int}}}) == (a=[1 2; 3 4],)
#     # test Matrix w/ lifted value
#     @test isequal(JSONBase.materialize("[[null,null],[null,null]]", Matrix{Missing}), [missing missing; missing missing])
#     # test lift on Dict values
#     obj = JSONBase.materialize("""{\"ffffffff-ffff-ffff-ffff-ffffffffffff\": null,\"ffffffff-ffff-ffff-ffff-fffffffffffe\": null}""", Dict{UUID,Missing})
#     @test obj[UUID(typemax(UInt128))] === missing
#     @test obj[UUID(typemax(UInt128) - 0x01)] === missing
#     # materialize! with custom dicttype
#     obj = OrderedDict{String, Any}()
#     JSONBase.materialize!("""{"a": {"a": 1, "b": 2}, "b": {"a": 3, "b": 4}}""", obj; dicttype=OrderedDict{String, Any})
#     @test obj["a"] == OrderedDict("a" => 1, "b" => 2)
#     @test obj["b"] == OrderedDict("a" => 3, "b" => 4)
#     # nested union struct field
#     @test JSONBase.materialize("""{"id": 1, "value": {"id": 1, "value": null}}""", M) == M(1, K(1, missing))
#     # recusrive field materialization
#     JSONBase.materialize("""{ "id": 1, "value": { "id": 2 } }""", Recurs)
#     # multidimensional arrays
#     # "[[1.0],[2.0]]" => (1, 2)
#     m = Matrix{Float64}(undef, 1, 2)
#     m[1] = 1
#     m[2] = 2
#     @test JSONBase.materialize("[[1.0],[2.0]]", Matrix{Float64}) == m
#     # "[[1.0,2.0]]" => (2, 1)
#     m = Matrix{Float64}(undef, 2, 1)
#     m[1] = 1
#     m[2] = 2
#     @test JSONBase.materialize("[[1.0,2.0]]", Matrix{Float64}) == m
#     # "[[[1.0]],[[2.0]]]" => (1, 1, 2)
#     m = Array{Float64}(undef, 1, 1, 2)
#     m[1] = 1
#     m[2] = 2
#     @test JSONBase.materialize("[[[1.0]],[[2.0]]]", Array{Float64, 3}) == m
#     # "[[[1.0],[2.0]]]" => (1, 2, 1)
#     m = Array{Float64}(undef, 1, 2, 1)
#     m[1] = 1
#     m[2] = 2
#     @test JSONBase.materialize("[[[1.0],[2.0]]]", Array{Float64, 3}) == m
#     # "[[[1.0,2.0]]]" => (2, 1, 1)
#     m = Array{Float64}(undef, 2, 1, 1)
#     m[1] = 1
#     m[2] = 2
#     @test JSONBase.materialize("[[[1.0,2.0]]]", Array{Float64, 3}) == m

#     m = Array{Float64}(undef, 1, 2, 3)
#     m[1] = 1
#     m[2] = 2
#     m[3] = 3
#     m[4] = 4
#     m[5] = 5
#     m[6] = 6
#     @test JSONBase.materialize("[[[1.0],[2.0]],[[3.0],[4.0]],[[5.0],[6.0]]]", Array{Float64, 3}) == m
#     # 0-dimensional array
#     m = Array{Float64,0}(undef)
#     m[1] = 1.0
#     @test JSONBase.materialize("1.0", Array{Float64,0}) == m
#     # test custom JSONStyle
#     @test_throws MethodError JSONBase.materialize("340282366920938463463374607431768211455", UUID; style=CustomJSONStyle())
#     JSONBase.lift(::CustomJSONStyle, ::Type{UUID}, x) = UUID(UInt128(x))
#     @test JSONBase.materialize("340282366920938463463374607431768211455", UUID; style=CustomJSONStyle()) == UUID(typemax(UInt128))
#     JSONBase.lift(::CustomJSONStyle, ::Type{N}, key, val) = key == :uuid ? UUID(UInt128(val)) : JSONBase.lift(N, key, val)
#     @test JSONBase.materialize("{\"id\": 0, \"uuid\": 340282366920938463463374607431768211455}", N; style=CustomJSONStyle()) == N(0, UUID(typemax(UInt128)))
#     # tricky unions
#     @test JSONBase.materialize("{\"id\":0}", O) == O(0, nothing)
#     @test JSONBase.materialize("{\"id\":0,\"name\":null}", O) == O(0, missing)
#     JSONBase.choosetype(::Type{O}, key, ::Type{Union{I,L,Missing,Nothing}}, val) = JSONBase.gettype(val) == JSONBase.JSONTypes.NULL ? Missing : hasproperty(val, :fruit) ? I : L
#     @test JSONBase.materialize("{\"id\":0,\"name\":{\"id\":1,\"name\":\"jim\",\"fruit\":\"apple\"}}", O) == O(0, I(1, "jim", apple))
#     @test JSONBase.materialize("{\"id\":0,\"name\":{\"id\":1,\"firstName\":\"jim\",\"rate\":3.14}}", O) == O(0, L(1, "jim", 3.14))
# end
