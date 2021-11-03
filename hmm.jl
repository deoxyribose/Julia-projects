using Gen, Distributions
using PyPlot


struct State
    z::Int64
    x::Float64
end

n_states = 3
n_dims = 1
transition_matrix = [0.9 0.05 0.05; 0.15 0.8 0.05; 0.2 0.1 0.7]

@gen (static) function state_transition_and_emission(t::Int, prev_state::State)
    z = @trace(categorical(transition_matrix[prev_state.z,:]), :z)
    x = @trace(normal(z, 0.1), :x)
    next_state = State(z, x)
    return next_state
end

chain = Gen.Unfold(state_transition_and_emission)

# Normal HMM
@gen (static) function hidden_markov_model(t::Int)
    z_0 = @trace(categorical(vec([1/3., 1/3., 1/3.])), :z0)
    init_state = State(z_0, 0.0)
    states = @trace(chain(t, init_state), :chain)
    return (init_state, states)
end

Gen.load_generated_functions()

trace = simulate(hidden_markov_model, (100,))

choices = get_choices(trace)

[trace[:chain => i => :x] for i in 1:100]

plt.figure()
plt.scatter(1:100, [trace[:chain => i => :x] for i in 1:100])
display(gcf())

plt.figure()
plt.scatter(ones(100), [trace[:chain => i => :x] for i in 1:100])
display(gcf())

