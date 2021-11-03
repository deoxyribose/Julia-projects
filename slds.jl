using Gen, Distributions
using PyPlot

struct State
    z::Int64
    u::Vector{Float64}
    x::Float64
end

n_states = 3
n_dims = 1
transition_matrix = [0.99 0.005 0.005; 0.005 0.99 0.005; 0.05 0.05 0.9]
theta = 2*pi/100
function rot_mat(theta)
    return [cos(theta) -sin(theta); sin(theta) cos(theta)]
end

function random_mat()
    return rand(2,2)
end

rot_mats = cat(dims=3,rot_mat(theta),rot_mat(theta*10),rot_mat(theta*0.1))
#rot_mats = cat(dims=3,random_mat(),random_mat(),random_mat())

u_0 = [1.; 0.]

@gen (static) function phase_vector(t::Int, prev_state::State)
    z = @trace(categorical(transition_matrix[prev_state.z,:]), :z)
    u = rot_mats[:,:,prev_state.z] * prev_state.u
    x = @trace(normal(u[1], 0.1), :x)
    next_state = State(z, u, x)
    return next_state
end

chain = Gen.Unfold(phase_vector)

# Normal HMM
@gen (static) function g_sim(t::Int)
    z_0 = @trace(categorical(vec([1/3., 1/3., 1/3.])), :z0)
    init_state = State(z_0, u_0, 0.0)
    states = @trace(chain(t, init_state), :chain)
    return (init_state, states)
end

Gen.load_generated_functions()

N = 1000

trace = simulate(g_sim, (N,))

choices = get_choices(trace)

z = [trace[:chain => i => :z] for i in 1:N]
obs = [trace[:chain => i => :x] for i in 1:N]

plt.figure()
plt.scatter(1:N, [trace[:chain => i => :x] for i in 1:N])
display(gcf())