include("dsl2.jl")

n_concepts = length(concepts)
theta = ones(n_concepts) / n_concepts

var_names = [string("x", i) for i in 1:10000]

function attention(library, type)
    """
    Filters the library for concepts that unify with the given type.
    Returns a vector of probabilities for each such primitive in the library.
    """
    substitutions = [get_substitutions(yield(x.type), type) for x in library]
    subs_and_concepts = filter(x -> !isnothing(x[1]), collect(zip(substitutions, library)))
    n_concepts = length(subs_and_concepts)
    if n_concepts == 0
        error("No concepts found for type $type")
    end
    subs, concepts = collect(zip(subs_and_concepts...))
    theta = ones(n_concepts) / n_concepts
    terminals = filter(x -> isempty(get_argtypes(x.type)), concepts)
    p_terminal = 0.6
    # assuming all terminals occur before all non-terminals
    if 0 < length(terminals) < length(concepts)
        theta[1:length(terminals)] .= p_terminal / length(terminals)
        theta[length(terminals)+1:end] .= (1 - p_terminal) / (n_concepts - length(terminals))
    else
        theta .= 1 / n_concepts
    end
    return concepts, theta, subs
end

function synthesize(library, type)
    """
    Synthesizes a program of the given type from the given library.
    """
    @show(type)
    concepts, theta, subs = attention(library, type)
    e_idx = categorical(@show(theta))
    e = concepts[e_idx]
    sub = subs[e_idx]
    @show(e.type)
    @show(sub[1])
    inferred_type = replace_in_type(e.type, sub[1])
    @show(inferred_type)
    # infer types of arguments
    arg_types = [arg_type for arg_type in get_argtypes(inferred_type)]
    @show(arg_types)
    args = [synthesize(library, arg_type) for arg_type in get_argtypes(inferred_type)]
    return AST(e, args)
end

function compile_as_generative_function(AST)
    """
    Compiles an AST into a generative function.
    """
    varname = Symbol(popfirst!(var_names)) # get unused variable name

    body = compile(AST)

    genfun = quote
        @gen function $(varname)()
            return $body
        end
    end
    return genfun
end

function compile(AST)
    """
    Compiles an AST into a Julia expression.
    """
    e = AST.op
    args = [compile(arg) for arg in AST.args]
    if is_traceable(e.type)
        address = popfirst!(var_names)
        return Expr(:macrocall, Symbol("@trace"), Expr(:line), e.expr(args...), :(Symbol($address)))
    else
        return e.expr(args...)
    end
end

#foo_AST = synthesize(concepts, parseTypeExpr(:(PositiveFloat)))
#foo_AST = synthesize(concepts, parseTypeExpr(:(Integer)))
#foo_AST = synthesize(concepts, parseTypeExpr(:(Probability)))
#foo_AST = synthesize(concepts, parseTypeExpr(:(PositiveInteger)))
#foo_AST = synthesize(concepts, parseTypeExpr(:([Real])))
#foo_AST = synthesize(concepts, parseTypeExpr(:([AbstractFloat])))
foo_AST = synthesize(concepts, parseTypeExpr(:([Probability])))
#foo_AST = synthesize(concepts, parseTypeExpr(:([PositiveInteger])))
foo_expr = compile_as_generative_function(foo_AST)
synthesized_gen_fun = eval(foo_expr)

# generate a trace from synthesized_gen_fun
trace = simulate(synthesized_gen_fun, ())
get_choices(trace)
get_retval(trace)
