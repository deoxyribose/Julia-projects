module Factor
export N,K,D

using Gen, Distributions
using PyPlot
using Debugger

## generate syntheic dataset x
N = 1000
K = 1
D = 2

@gen (static) function generate_single_weight()
    return @trace(normal(0., 3.), :w_kd)
end;

generate_weight_vector = Map(generate_single_weight);

@gen (static) function generate_weight_vector_()
    return @trace(generate_weight_vector(1:D), :w_d)
end;

generate_weight_matrix = Map(generate_weight_vector_); 

function convert_persistent_nested_array_to_matrix(nested_array)
    N, M = length(nested_array), length(nested_array[1])
    X = Array{Float64}(undef, N, M)
    for n in 1:N
        for m in 1:M
            X[n,m] = nested_array[n][m]
        end
    end
    return X
end

@gen (static) function generate_latent()
    return @trace(normal(0., 1.), :z_nk)
end;

generate_latent_vector = Map(generate_latent);

@gen (static) function generate_latent_variables_()
    return @trace(generate_latent_vector(1:K), :z_n)
end;

generate_latent_variables = Map(generate_latent_variables_);

@gen (static) function factor_model(N)
    weights = @trace(generate_weight_matrix(1:K), :W)
    zs = @trace(generate_latent_variables(1:N), :Z)
    W = convert_persistent_nested_array_to_matrix(weights)
    Z = convert_persistent_nested_array_to_matrix(zs)
    X = @trace(broadcasted_normal(Z * W, 1.), :X)
end


@gen function factor_model2(N)
    Z = Array{Float64}(undef, N, K)
    W = Array{Float64}(undef, K, D)
    for k in 1:K
        for n in 1:N
            Z[n,k] = @trace(normal(0.,1.), :z => n => k)
        end
        for d in 1:D
            W[k,d] = @trace(normal(0.,3.), :w => k => d)
        end
    end
    X = @trace(broadcasted_normal(Z * W, 1.), :X)
end

@gen function factor_model3(N)
    Z = @trace(broadcasted_normal(zeros(N,K),1.), :Z)
    W = @trace(broadcasted_normal(zeros(K,D),3.), :W)
    X = @trace(broadcasted_normal(Z * W, 1.), :X)
end

Gen.load_generated_functions()

new_trace = simulate(factor_model, (N,));
#new_trace = simulate(factor_model2, (N,));

get_choices(new_trace)

X = new_trace[:X];

plt.figure()
plt.scatter(X[:,1],X[:,2])
plt.axis("equal");
display(gcf())

function ppc(model, args, trace)
	params = Gen.choicemap()
	params[:Z] = trace[:Z]
	params[:W] = trace[:W]
    ppc, = generate(model, args, params);
    ppc
end

function inference(n_iterations)
    observations = choicemap()
    observations[:X] = X
    selection = DynamicSelection()
    push!(selection, :Z)
    push!(selection, :W)

    new_trace, = generate(factor_model3, (N,), observations)
    for rep in 1:n_iterations
        new_trace = Gen.map_optimize(new_trace, selection, max_step_size=1., min_step_size=1e-10)
    end
    new_trace
end



end