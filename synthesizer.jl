include("types.jl")
include("dsl.jl")
include("intepreter.jl")


function synthesize(observations)
    # get types that are compatible with observations
    observation_types = get_compatible_types(observations)
    println(observation_types)
    # get DSLNode DSLNodes that are compatible with observation_types
    
end

# to synthesize a probabilistic program for a given data-matrix X
# we do a top-down search
# pruning invalid programs along the way using types

global scope = Scope([:X],[])
sample_exp = TraceNode(distributions[end], :sigma, [Num(0.9)])
sample_norm = TraceNode(distributions[1], :x, [Num(0.9),sample_exp])
gfe = GenNode(:generate_latent, :(i::Int64), [sample_norm])

#foo = interpret(TraceNode2)
#tmp = wrap_in_generative_function(foo)
obs = eval(interpret(gfe))(1)

# Define a combinator struct
# that allows for inferring the type of the resulting GenerativeFunction
iid_norm = CombinatorNode(combinators[1], 1)
get_return_type(iid_norm)

sample_iid = TraceNode(iid_norm, :xs, [JuliaExpr(:(1:3))])

gfe = GenNode(:generate_vector, [sample_iid])

obs = eval(interpret(gfe))()