abstract type DSLNode end

struct Primitive
    name
    type
    expr
end

_trace = Primitive(
    "Trace",
    (:a => :b) => Symbol => :b,
    (generator::Expr, address::Symbol) -> Expr(:macrocall, Symbol("@trace"), Expr(:line), generator, address)
)

_normal = Primitive(
    "Normal",
    Real => PositiveReal => Real,
    (mean::Real, variance::PositiveReal) -> Expr(:call, :Normal, mean, variance)
)

_map = Primitive(
    "Map",
    (:a => :b) => ([:a] => [:b]),
    fun::Symbol -> Expr(:call, :Map, fun)
)