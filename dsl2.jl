using Gen

include("types.jl")
include("typelang.jl")

abstract type Node end
abstract type LeafNode <: Node end
abstract type BinaryOpNode <: Node end

struct Primitive
    type::Union{Symbol,Expr,Vector{Expr}}
    expr::Function
end

struct Compound
    type::Union{Symbol,Expr,Vector{Expr}}
    expr::Function
end

struct AST
    op::Union{Primitive, Compound}
    args::Vector{AST}
end

_zero_float = Primitive(
    :(AbstractFloat),
    () -> 0.0
)

_one_float = Primitive(
    :(PositiveFloat),
    () -> 1.0
)

_zero_int = Primitive(
    :(Integer),
    () -> 0
)

_one_int = Primitive(
    :(PositiveInteger),
    () -> 1
)

_add = Primitive(
    :(Real × Real => Real),
    (x, y) -> Expr(:call, :+, x, y)
)

_minus = Primitive(
    :(Real => Real),
    (x, y) -> Expr(:call, :-, x, y)
)

_multiply = Primitive(
    :(Real × Real => Real),
    (x, y) -> Expr(:call, :*, x, y)
)

_divide = Primitive(
    :(Real × PositiveFloat => Real),
    (x, y) -> Expr(:call, :/, x, y)
)

_power = Primitive(
    [:(Real × Real => Real)],
    (x, y) -> Expr(:call, :^, x, y)
)

_normal = Primitive(
    :(D(Real × PositiveFloat => AbstractFloat)),
    (mean, variance) -> Expr(:call, :normal, mean, variance)
)

_exponential = Primitive(
    :(D(PositiveFloat => PositiveFloat)),
    (rate) -> Expr(:call, :exponential, rate)
)

_poisson = Primitive(
    :(D(PositiveFloat => Integer)),
    (rate) -> Expr(:call, :poisson, rate)
)

_beta = Primitive(
    :(D(PositiveFloat × PositiveFloat => Probability)),
    (alpha, beta) -> Expr(:call, :beta, alpha, beta)
)

_fill = Primitive(
    :(:a × PositiveInteger => [:a]),
    (a, n) -> Expr(:call, :fill, a, n)
)

_map = Primitive(
    :(G(:a => :b) × [:a] => G([:a] => [:b])),
    (fun, arr) -> Expr(:call, Expr(:call, :Map, fun), arr)
)

_exp = Primitive(
    :(Real => PositiveFloat),
    x -> Expr(:call, :exp, x)
)

_log = Primitive(
    :(PositiveFloat => Real),
    x -> Expr(:call, :log, x)
)

_one_plus_poisson = Compound(
    :(Real => PositiveInteger),
    x -> _add.expr(_one_int.expr(), _poisson.expr(x))
)

concepts = [_zero_float, _one_float, _zero_int, _one_int, _add, _multiply, _normal, _exponential, _poisson, _beta, _fill, _map, _exp, _one_plus_poisson]

#replace_in_type(:(:a × PositiveInteger => [:a]), TypeVar(:a) => DataTyp(Real))