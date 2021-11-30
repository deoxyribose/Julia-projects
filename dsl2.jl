include("types.jl")

abstract type DSLNode end

struct Primitive
    name::String
    type::Union{Expr,Vector{Expr}}
    expr::Function
end


_trace = Primitive(
    "Trace",
    [:(G(:a => :b) × Symbol => :b),
    :(D(:a => :b) × Symbol => :b)],
    (generator::Expr, address::Symbol) -> Expr(:macrocall, Symbol("@trace"), Expr(:line), generator, address)
)

_normal = Primitive(
    "Normal",
    :(D(Real × PositiveReal => Real)),
    (mean::Real, variance::PositiveReal) -> Expr(:call, :normal, mean, variance)
)

_exponential = Primitive(
    "Exponential",
    :(D(PositiveReal => PositiveReal)),
    (rate::PositiveReal) -> Expr(:call, :exponential, rate)
)

_map = Primitive(
    "Map",
    :(G(:a => :b) => G([:a] => [:b])),
    fun::Symbol -> Expr(:call, :Map, fun)
)

_exp = Primitive(
    "exp",
    :(Real => PositiveReal),
    x -> Expr(:call, :exp, x)
)

# @gen function generate_single_weight(i)
#     #return @trace(normal(0.0, 3.0), :w_kd)
#     return 2+2
# end;

library = [_trace, _normal, _exponential, _map]

greek_alphabet = ['α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ο', 'ρ', 'ς', 'σ', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω']

function synthesize(library, environment, type)::Vector{Expr}
    varname = Symbol(pop!(greek_alphabet)) # get unused variable name

    body = synthesize(library, environment, last(type))
    return GenNode(varname, body)
end