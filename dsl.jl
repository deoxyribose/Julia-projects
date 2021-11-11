using Distributions, Gen
using LinearAlgebra
using MacroTools

# Define DSLNodes for probabilistic program synthesis


@gen function generate_latent(i::Int64)
    return @trace(exponential(0.9), :x)
end;

@gen function single()
    return @trace(generate_latent(1), :x)
end

@gen function vector()
    return @trace(Map(generate_latent)(1:5), :x)
end

@gen function matrix()
    return @trace(Map(Map(generate_latent))([1:3,1:3,1:3]), :x)
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

struct Combinator
    name::Symbol
    type_signature # A function that takes a pair of types and returns a pair of types
end
struct CombinatorNode
    combinator::Combinator
    kernel::Union{Int64, CombinatorNode} # the index of a generative function in the current scope
    args # args type depends on combinator and kernel.
         # If combinator is Map, and kernel has input type T
         # args has type Vector{T}
end
struct Generator
    #=
    An expression that can be traced, e.g.
    @trace(Normal(0.,1.), :z)
    @trace(latent(1), :z)
    @trace(Map(latent)(1:K), :z)
    @trace(Map(Map(latent))([1:3,1:3,1:3]), :x)
    =#
    name::Union{Symbol, CombinatorNode}
    support::Type
    argtypes::Union{T, Vector{T}} where T <: Type
end
abstract type Concept end
struct TraceNode
    generator::Generator
    address::Symbol
    args::Array{Concept}
end

ConceptType = Union{NumericType, Symbol, Expr, TraceNode, CombinatorNode}
#struct DSLNode <: Concept
struct DSLNode <: Concept
    expr::ConceptType
    #assign_var::Union{Symbol, Nothing} # if symbol is :z, evaluate to "z = expr", if nothing then "expr"
end

struct GenNode
    #=
    Generative function
    =#
    name::Symbol
    args::Union{Expr, Vector{Expr}} # an expression like i::Int64
    expr::Array{Concept}
end

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

function interpret(trace_expr::TraceNode)
    argsExpr = [interpret(arg) for arg in trace_expr.args]
    distribution = Expr(:call, interpret(trace_expr.generator.name), argsExpr...)
    address = Expr(:quote, trace_expr.address)
    return Expr(:macrocall, Symbol("@trace"), Expr(:line), distribution, address)
end

function interpret(concept::DSLNode)
    return interpret(concept.expr)
end

function interpret(expr::Union{NumericType, Symbol, Expr})
    return expr
end

function interpret(gen_node::GenNode)
    #for expr in GenNode.expr
    return_expr = interpret(gen_node.expr[end])
    args_expr = interpret(gen_node.args)
    genfun = quote
        @gen function $(gen_node.name)($args_expr)
            return $return_expr
        end;
    end
    if gen_node.args isa Vector
        argtype = [eval(arg.args[2]) for arg in gen_node.args]
    else
        argtype = eval(gen_node.args.args[2])
    end
    push!(scope.generators, Generator(gen_node.name,get_return_type(gen_node),argtype))
    return genfun
end

function interpret(comb_node::CombinatorNode)
    # assert that kernel domain type matches input type
    arg_expr = interpret(comb_node.args)
    kernel = scope.generators[comb_node.kernel].name
    return Expr(:call, Expr(:call, comb_node.combinator.name, kernel), arg_expr)
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
    #=
    Assuming observations contains elements of the same type
    returns an array of all types that are compatible
    e.g.
    julia> get_compatible_types(rand(2,2))
    3-element Array{Type,1}:
     Array{Real,2}
     Array{PositiveReal,2}
     Array{Probability,2}
    =#
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

function get_return_type(trace_node::TraceNode)
    return trace_node.generator.support
end

function get_return_type(gen_node::GenNode)
    return get_return_type(gen_node.expr[end])
end

function get_return_type(dsl_node::DSLNode)
    return get_return_type(dsl_node.expr)
end

function get_return_type(comb_node::CombinatorNode)
    # assert kernel is in scope
    kernel_input_type = scope.generators[comb_node.kernel].argtypes
    kernel_output_type = scope.generators[comb_node.kernel].support
    return comb_node.combinator.type_signature(kernel_input_type => kernel_output_type)
end

function get_function_type(funexpr)
    fundef = splitdef(funexpr) 
    return eval(fundef[:args][1].args[2]) => eval(fundef[:rtype])
end

function synthesize(observations)
    # get types that are compatible with observations
    observation_types = get_compatible_types(observations)
    println(observation_types)
    # get DSLNode concepts that are compatible with observation_types
    
end

# to synthesize a probabilistic program for a given data-matrix X
# we do a top-down search
# pruning invalid programs along the way using types

global scope = Scope([:X],[])
sample_exp = TraceNode(distributions[end], :sigma, [DSLNode(0.9)])
sample_norm = TraceNode(distributions[1], :x, [DSLNode(0.9),DSLNode(sample_exp)])
gfe = GenNode(:generate_latent, :(i::Int64), [DSLNode(sample_norm)])

#foo = interpret(TraceNode2)
#tmp = wrap_in_generative_function(foo)
obs = eval(interpret(gfe))(1)

# Define a combinator struct
# that allows for inferring the type of the resulting GenerativeFunction
iid_norm = CombinatorNode(combinators[1], 1, :(1:3))
get_return_type(iid_norm)

# Generators should be automatically created from CombinatorNodes
# Too much redunancy?
#sample_iid = TraceNode(Generator())


#iid_norm.combinator.type_signature(Int64 => Float64)