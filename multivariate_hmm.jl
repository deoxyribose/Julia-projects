using Gen, Distributions
using PyPlot


struct State
    z::Int64
    x::Vector{Float64}
end

n_states = 3
n_dims = 2
transition_matrix = [0.9 0.05 0.05; 0.15 0.8 0.05; 0.2 0.1 0.7]
means = [0.2 1.3; 0.9 -3.; 4.1 -1.2]

@gen (static) function g_kernel(t::Int, prev_state::State)
    z = @trace(categorical(transition_matrix[prev_state.z,:]), :z)
    x = @trace(broadcasted_normal(means[z,:], 0.1), :x)
    next_state = State(z, x)
    return next_state
end

chain = Gen.Unfold(g_kernel)

# Normal HMM
@gen (static) function g_sim(t::Int)
    z_0 = @trace(categorical(vec([1/3., 1/3., 1/3.])), :z0)
    init_state = State(z_0, means[z_0, :])
    states = @trace(chain(t, init_state), :chain)
    return (init_state, states)
end

Gen.load_generated_functions()

trace = simulate(g_sim, (100,))

choices = get_choices(trace)

obs = [trace[:chain => i => :x] for i in 1:100]
obs = hcat(obs...)


plt.figure()
plt.scatter(obs[1,:],obs[2,:])
display(gcf())
