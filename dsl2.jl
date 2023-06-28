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

struct AST
    op::Primitive
    args::Vector{AST}
end

struct Compound
    type::Union{Symbol,Expr,Vector{Expr}}
    op::AST
end

_zero = Primitive(
    :(Real),
    () -> 0.0
)

_one = Primitive(
    :(PositiveFloat),
    () -> 1.0
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
    :(D(Real × PositiveFloat => Real)),
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
    :(PositiveInteger),
    AST(
        _add,
        [
            AST(
                _one,
                []
            ),
            AST(
                _poisson,
                [
                    AST(
                        _zero,
                        []
                    )
                ]
            )
        ]
    )
)

concepts = [_zero, _one, _add, _multiply, _normal, _exponential, _poisson, _beta, _fill, _map, _exp, _one_plus_poisson]
