using Distributions, Gen
using LinearAlgebra
using MacroTools

# Define DSLNodes for probabilistic program synthesis
abstract type DSLNode end

struct Num <: DSLNode
    expr::NumericType
end
struct Combinator
    name::Symbol
    type_signature # A function that takes a pair of types and returns a pair of types
end
struct CombinatorNode <: DSLNode
    combinator::Combinator
    kernel::Union{Int64, CombinatorNode} # the index of a generative function in the current scope
end
struct Generator <: DSLNode
    #=
    A distribution or generative function in the scope, e.g.
    Normal
    generate_latent
    =#
    name::Symbol
    support::Type
    argtypes::Union{Nothing, T, Vector{T}} where T <: Type
end

Generator(name, support) = Generator(name, support, nothing)
struct TraceNode <: DSLNode
    generator::Union{Generator, CombinatorNode}
    address::Symbol
    args::Array{DSLNode}
end

struct JuliaExpr <: DSLNode
    expr::Union{Symbol, Expr}
end

struct GenNode <: DSLNode
    #=
    Generative function
    =#
    name::Symbol
    args::Union{Nothing, Expr, Vector{Expr}} # an expression like i::Int64
    expr::Array{DSLNode}
end

GenNode(name, expr) = GenNode(name, nothing, expr)

mutable struct Scope
    variables::Vector{Symbol}
    generators::Vector{Generator}
end

distributions = [
    Generator(:normal, Real, [Real, PositiveReal]),
    Generator(:poisson, NonNegativeInteger, PositiveReal),
    Generator(:bernoulli, Bool, Probability),
    Generator(:beta, Probability, [PositiveReal, PositiveReal]),
    Generator(:exponential, PositiveReal, PositiveReal),
    #Generator(:categorical, Integer, [ProbabilityVector]),
    #Generator(:dirichlet, ProbabilityVector, [Vector{PositiveReal}])
]

combinators = [
    Combinator(:Map, S::(Pair{T, T} where T<:Type) -> Vector{first(S)} => Vector{last(S)})
    #:Unfold,
    #:Recurse,
    #:Switch
]