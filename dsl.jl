using Distributions, Gen
using LinearAlgebra
using MacroTools

# Define primitives for probabilistic program synthesis


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

NumericType = Union{Real, Bool, Integer, PositiveReal, PositiveInteger, Probability}
struct Generator # either a GenerativeFunction or Distribution
    name::Union{Symbol, CombinatorExpr}
    support::Type
    argtypes::Vector{Type}
end

abstract type Concept end
struct TraceExpr
    generator::Generator
    address::Symbol
    args::Array{Concept}
end

struct CombinatorExpr
    combinator::Symbol
    in_generator::Symbol
    args # args type depends on combinator and in_generator.
         # If combinator is Map, and in_generator has input type T
         # args has type Vector{T}
end

ConceptType = Union{NumericType, Symbol, Expr, TraceExpr, CombinatorExpr}
#struct Primitive <: Concept
struct Primitive <: Concept
    expr::ConceptType
    #assign_var::Union{Symbol, Nothing} # if symbol is :z, evaluate to "z = expr", if nothing then "expr"
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

combinators = [

]

function interpret(trace_expr::TraceExpr)
    argsExpr = [interpret(arg) for arg in trace_expr.args]
    distribution = Expr(:call, interpret(trace_expr.generator.name), argsExpr...)
    address = Expr(:quote, trace_expr.address)
    return Expr(:macrocall, Symbol("@trace"), Expr(:line), distribution, address)
end

function interpret(concept::Primitive)
    return interpret(concept.expr)
end

function interpret(expr::Union{NumericType, Symbol, Expr})
    return expr
end

numeric_types = [Real, Bool, Integer, PositiveReal, PositiveInteger, Probability]

function get_compatible_types(observations, numeric_types::Array{Type,1} = numeric_types)::Array{Type,1}
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

function get_compatible_types(observations::Array)::Array{Type,1}
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

function get_return_type(traceexpr::TraceExpr)::Type
    return traceexpr.generator.support
end

fun = quote
    function get_return_type(traceexpr::TraceExpr)::Type
        return traceexpr.generator.support
    end
end

function get_function_type(funexpr)
    fundef = splitdef(funexpr) 
    return eval(fundef[:args][1].args[2]) => eval(fundef[:rtype])
end

function synthesize(observations)
    # get types that are compatible with observations
    observation_types = get_compatible_types(observations)
    println(observation_types)
    # get primitive concepts that are compatible with observation_types
    
end

function wrap_in_generative_function(expr)
    genfun = quote
        @gen function generate()
            return $expr
        end;
    end
    return eval(genfun)
end
# to synthesize a probabilistic program for a given data-matrix X
# we do a top-down search
# pruning invalid programs along the way using types

scope = Scope([:X],[])
traceexpr = TraceExpr(distributions[end], :x, [Primitive(0.9)])
traceexpr = TraceExpr(distributions[1], :x, [Primitive(0.9),Primitive(0.9)])
foo = interpret(traceexpr)
tmp = wrap_in_generative_function(foo)
obs = tmp()

@gen function generate_latent(i::Int64)
    return @trace(exponential(0.9), :x)
end;
fun = quote
    @gen function generate_latent(i::Int64)
        return @trace(exponential(0.9), :x)
    end;
    # function generate(i::Int64)::Real
    #     return exponential(0.9)
    # end
end

combexpr = CombinatorExpr(Gen.Map, )