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
    return get_return_type(gen_node.expr)
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



function get_args(generator::Generator)
    return generator.argtypes
end

function get_args(dslnode::DSLNode)
    return fieldtypes(dslnode)
end