using Gen

include("types.jl")
include("typelang.jl")

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

primitives = [_trace, _normal, _exponential, _map]
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
    @show(e)
    @show(get_argtypes(e.type))
    @show(type)
end

synthesize(primitives, [], parseTypeExpr(:(G(Int64 × Int64 => [Float64]))))

type = parseTypeExpr(:(Int64 × Int64 => [Float64]))
type = parseTypeExpr(:(Int64 => [Float64]))