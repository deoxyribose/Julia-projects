using Gen

include("types.jl")
include("typelang.jl")

abstract type DSLNode end

struct Primitive
    type::Union{Symbol,Expr,Vector{Expr}}
    expr::Function
end
# _trace = Primitive(
#     "Trace",
#     [:(G(:a => :b) × Symbol => :b),
#         :(D(:a => :b) × Symbol => :b)],
#     (generator::Expr, address::Symbol) -> Expr(:macrocall, Symbol("@trace"), Expr(:line), generator, address)
# )

_zero = Primitive(
    :(Real),
    () -> 0.0
)

_one = Primitive(
    :(PositiveReal),
    () -> 1.0
)

_add = Primitive(
    [:(Real × Real => Real),
        :(Integer × Integer => Integer)],
    (x, y) -> Expr(:call, :+, x, y)
)

_minus = Primitive(
    [:(Real => Real),
        :(Integer => Integer)],
    (x, y) -> Expr(:call, :-, x, y)
)

_multiply = Primitive(
    [:(Real × Real => Real),
        :(Integer × Integer => Integer)],
    (x, y) -> Expr(:call, :*, x, y)
)

_divide = Primitive(
    [:(Real × Real => Real),
        :(Integer × Integer => Integer)],
    (x, y) -> Expr(:call, :/, x, y)
)

_power = Primitive(
    [:(Real × Real => Real)],
    (x, y) -> Expr(:call, :^, x, y)
)


_normal = Primitive(
    :(D(Real × PositiveReal => Real)),
    (mean::Real, variance::PositiveReal) -> Expr(:call, :normal, mean, variance)
)

_exponential = Primitive(
    :(D(PositiveReal => PositiveReal)),
    (rate::PositiveReal) -> Expr(:call, :exponential, rate)
)

_map = Primitive(
    :(G(:a => :b) × [:a] => G([:a] => [:b])),
    (fun::Symbol, arr::Vector{Any}) -> Expr(:call, Expr(:call, :Map, fun), arr)
)

_exp = Primitive(
    :(Real => PositiveReal),
    x -> Expr(:call, :exp, x)
)

primitives = [_normal, _exponential, _map, _exp]
n_primitives = length(primitives)
theta = ones(n_primitives) / n_primitives

greek_alphabet = ['α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ο', 'ρ', 'ς', 'σ', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω']
latin_alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']


function synthesize(library, environment, type::GenFunTyp)#::Vector{Expr}
    varname = Symbol(pop!(greek_alphabet)) # get unused variable name
    argnames = [Symbol(pop!(latin_alphabet)) for arg in get_argtypes(type)]

    body = synthesize(library, environment, last(type.elems))

    genfun = quote
        @gen function $(varname)($(argnames...))
            return $body
        end
    end
    return genfun
end

function synthesize(library, environment, type)
    @show(type)
    primitives = filter(x -> !isnothing(@show(get_substitutions(yield(x.type), type))), library)
    n_primitives = length(primitives)
    theta = ones(n_primitives) / n_primitives

    e_idx = categorical(theta)
    e = primitives[e_idx]
    args = [synthesize(library, environment, @show(arg_type)) for arg_type in get_argtypes(e.type)]
    if is_traceable(e.type)
        address = Symbol(pop!(greek_alphabet))
        return Expr(:macrocall, Symbol("@trace"), Expr(:line), e.expr(args...), address)
    else
        return e.expr(args...)
    end
end

dump(synthesize(primitives, [], parseTypeExpr(:(G(Int64 × Int64 => [Float64])))))

type = parseTypeExpr(:(Int64 × Int64 => [Float64]))
type = parseTypeExpr(:(Int64 => [Float64]))