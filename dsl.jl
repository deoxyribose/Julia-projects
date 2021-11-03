using Distributions, Gen
using LinearAlgebra

# Define primitives for probabilistic program synthesis
@gen function generate_latent()
    return @trace(exponential(0.9), :x)
end;

foo = quote
    @gen function generate_latent()
        return @trace(exponential(0.9), :x)
    end;
end

for dist in (:exponential, :bernoulli, :poisson)
    @eval begin
        @gen function generate_latent()
            return @trace($dist(0.9), :x)
        end;
    end
    trace = simulate(generate_latent, ())
    @show trace[:x]
end

struct Generator # either a GenerativeFunction or Distribution
    generator::Symbol
    support::Type
    argtypes::Vector{Type}
end

abstract type Concept end

#struct TraceExpr <: Expr
struct TraceExpr
    generator::Generator
    address::Symbol
end

struct PositiveReal{Real}
    x::Real
    PositiveReal(x) = x > 0 ? new{Real}(x) : error("The given number must be positive")
end

struct Probability{Real}
    x::Real
    Probability(x) = 0. <= x <= 1. ? new{Real}(x) : error("The given number must be between 0 and 1")
end

# struct ProbabilityVector{Vector{Real}}
#     x::Vector{Real}
#     ProbabilityVector(x) = Distributions.isprobvec(x) ? new{Vector{Real}}(x) : error("The vector must sum to 1 and contain numbers between 0 and 1")
# end

struct NonNegativeInteger{Int64}
    x::Int64
    NonNegativeInteger(x) = x >= 0 ? new{Int64}(x) : error("The given number must be an integer greater or equal to 0")
end

struct PositiveInteger{Int64}
    x::Int64
    PositiveInteger(x) = x > 0 ? new{Int64}(x) : error("The given number must be an integer greater than 0")
end

struct Primitive <: Concept
    expr::Union{Symbol, TraceExpr, FunctionExpr, CombinatorExpr, ConstantExpr}
    assigned::Bool # whether the expression is assigned to a variable name, e.g. "z = @trace(dist, :z)" is true, but "@trace(dist, :z)" is false
end

mutable struct Scope
    variables::Vector{Symbol}
    generators::Vector{Generator}
end

distributions = [
    Generator(:normal, Real, [Real, PositiveReal]),
    Generator(:poisson, NonNegativeInteger, [PositiveReal]),
    Generator(:bernoulli, Bool, [Probability]),
    Generator(:beta, Probability, [PositiveReal, PositiveReal]),
    Generator(:exponential, PositiveReal, [PositiveReal]),
    #Generator(:categorical, Integer, [ProbabilityVector]),
    #Generator(:dirichlet, ProbabilityVector, [Vector{PositiveReal}])
]

function interpret(concept::Concept)

end

numeric_types = [Real, Bool, Integer, PositiveReal, PositiveInteger, Probability]

function get_compatible_types(observations, numeric_types)
    compatible_types = []
    for type in numeric_types
        try
            type(observations)
            push!(compatible_types, type)
        catch
            continue
        end
    end
    return compatible_types
end

function get_compatible_types(observations::Array)
    D = ndims(observations)
    @assert D > 0
    compatible_types = []
    compatible_elem_types = numeric_types
    for elem in Iterators.flatten(observations)
        # for each element in observations, we get compatible types
        # if a given element is only compatible with some types
        # only those types will be checked for compatibility with the next element
        compatible_elem_types = get_compatible_types(elem, compatible_elem_types)
    end
    for type in compatible_elem_types
        push!(compatible_types, Array{type, D})
    end
    return compatible_types
end

function synthesize(observations)
    # get types that are compatible with observations
    observation_types = get_compatible_types(observations)
    # get concepts that are compatible with observation_types
    
end

# to synthesize a probabilistic program for a given data-matrix X
# we do a top-down search
# pruning invalid programs along the way using types

scope = Scope([:X],[])