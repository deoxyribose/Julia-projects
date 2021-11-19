
function interpret(trace_expr::TraceNode)
    argsExpr = [interpret(arg) for arg in trace_expr.args]
    distribution = Expr(:call, interpret(trace_expr.generator), argsExpr...)
    address = Expr(:quote, trace_expr.address)
    return Expr(:macrocall, Symbol("@trace"), Expr(:line), distribution, address)
end

function interpret(generator::Generator)
    return generator.name
end

function interpret(expr::Union{Num, JuliaExpr})
    return expr.expr
end

function interpret(expr::Expr)
    return expr
end

function interpret(gen_node::GenNode)
    #for expr in GenNode.expr
    return_expr = interpret(gen_node.expr[end])
    if isnothing(gen_node.args)
        genfun = quote
            @gen function $(gen_node.name)()
                return $return_expr
            end;
        end
        push!(scope.generators, Generator(gen_node.name,get_return_type(gen_node)))
    else
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
    end
    return genfun
end

# function interpret(comb_node::CombinatorNode)
#     # assert that kernel domain type matches input type
#     arg_expr = interpret(comb_node.args)
#     kernel = scope.generators[comb_node.kernel].name
#     return Expr(:call, Expr(:call, comb_node.combinator.name, kernel), arg_expr)
# end

function interpret(comb_node::CombinatorNode)
    # assert that kernel domain type matches input type
    kernel = scope.generators[comb_node.kernel].name
    return Expr(:call, comb_node.combinator.name, kernel)
end