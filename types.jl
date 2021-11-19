
struct PositiveReal{Real}
    x::Real
    PositiveReal(x) = x > 0 ? new{Real}(x) : error("The given number must be positive")
end

struct Probability{Real}
    x::Real
    Probability(x) = 0.0 <= x <= 1.0 ? new{Real}(x) : error("The given number must be between 0 and 1")
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

NumericType = Union{Real,Bool,Integer,PositiveReal,PositiveInteger,Probability}
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
        push!(compatible_types, Array{type,D})
    end
    return compatible_types
end

function get_return_type(trace_node::TraceNode)
    if isa(trace_node.generator, CombinatorNode)
        return last(get_return_type(trace_node.generator))
    elseif isa(trace_node.generator, Generator)
        return trace_node.generator.support
    end
end

function get_return_type(num::Num)
    return typeof(num.expr)
end

function get_return_type(gen_node::GenNode)
    return get_return_type(gen_node.expr[end])
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