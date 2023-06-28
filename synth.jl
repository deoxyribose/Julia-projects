include("dsl2.jl")

n_concepts = length(concepts)
theta = ones(n_concepts) / n_concepts

var_names = [string("x", i) for i in 1:10000]

function attention(library, type)
    """
    Filters the library for concepts that unify with the given type.
    Returns a vector of probabilities for each such primitive in the library.
    """
    concepts = filter(x -> !isnothing(get_substitutions(yield(x.type), type)), library)
    n_concepts = length(concepts)
    if n_concepts == 0
        error("No concepts found for type $type")
    end
    theta = ones(n_concepts) / n_concepts
    # if _zero or _one or both are in concepts, let the sum of their probabilities be 0.5
    p_terminal = 0.6
    if _zero in concepts && _one in concepts
        theta[1] = p_terminal / 2
        theta[2] = p_terminal / 2
        theta[3:end] .= (1 - p_terminal) / (n_concepts - 2)
    elseif _zero in concepts || _one in concepts
        theta[1] = p_terminal
        theta[2:end] .= (1 - p_terminal) / (n_concepts - 1)
    else
        theta = ones(n_concepts) / n_concepts
    end
    return concepts, theta
end

function synthesize(library, type)
    """
    Synthesizes a program of the given type from the given library.
    """
    concepts, theta = attention(library, type)
    e_idx = categorical(@show(theta))
    e = concepts[e_idx]
    args = [synthesize(library, arg_type) for arg_type in get_argtypes(e.type)]
    if isa(e, Compound)
        return e.op
    elseif isa(e, Primitive)
        return AST(e, args)
    else
        error("Unknown concept type: $e")
    end
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
foo_AST = synthesize(concepts, parseTypeExpr(:([Real])))
#foo_AST = synthesize(concepts, [], parseTypeExpr(:([Float64])))
#foo_AST = synthesize(concepts, parseTypeExpr(:([PositiveInteger])))
foo_expr = compile_as_generative_function(foo_AST)
synthesized_gen_fun = eval(foo_expr)

# generate a trace from synthesized_gen_fun
trace = simulate(synthesized_gen_fun, ())
get_choices(trace)
get_retval(trace)
