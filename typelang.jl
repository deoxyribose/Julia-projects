import Base.iterate

abstract type Typ end

struct TypeVar <: Typ
    name::Symbol
end

struct DataTyp <: Typ
    name::Type
end

struct FunTyp <: Typ
    elems::Pair{Typ,Typ}
end

struct GenFunTyp <: Typ
    elems::Pair{Typ,Typ}
end

struct DistFunTyp <: Typ
    elems::Pair{Typ,Typ}
end

struct ListTyp <: Typ
    name::Typ
end

struct ProdTyp <: Typ
    elems::Pair{Typ,Typ}
end


function parseTypeExpr(x)
    if x isa QuoteNode
        return TypeVar(eval(x))
    elseif x isa Symbol
        return DataTyp(eval(x))
    elseif x isa Expr && x.head == :call
        if x.args[1] == :(=>)
            arguments = [parseTypeExpr(y) for y in x.args[2:end]]
            return FunTyp(arguments[1] => arguments[2])
        elseif x.args[1] == :×
            arguments = [parseTypeExpr(y) for y in x.args[2:end]]
            return ProdTyp(arguments[1] => arguments[2])
        elseif x.args[1] == :G
            arguments = [parseTypeExpr(y) for y in x.args[2].args[2:end]]
            return GenFunTyp(arguments[1] => arguments[2])
        elseif x.args[1] == :D
            arguments = [parseTypeExpr(y) for y in x.args[2].args[2:end]]
            return DistFunTyp(arguments[1] => arguments[2])
        else
            println("x is a ", typeof(x))
        end
    elseif x isa Expr && x.head == :vect
        @assert length(x.args) == 1
        return ListTyp(parseTypeExpr(x.args[1]))
    else
        println("x is a ", typeof(x))
    end
end

macro parseTypeExpr(x)
    # return :($(parseTypeExpr(x)))
    return esc(:($(parseTypeExpr(x))))
end

function yield(expr::Union{Expr, Symbol})
    yield(parseTypeExpr(expr))
end

function yield(expr::Typ)
    if hasproperty(expr, :elems)
        return yield(last(expr.elems))
    else
        return expr
    end
end

function yield(exprs::Vector{Union{Expr, Symbol}})
    yields = []
    for expr in exprs
        push!(yields, yield(expr))
    end
    if all([y == yields[1] for y in yields])
        return yields[1]
    else
        error("Type expression has multiple inconsistent yields")
    end
end

function isasubtype(x::DataTyp, y::DataTyp)
    return x.name <: y.name || y.name <: x.name
end

function binding(x, s)
    if haskey(s, x)
        return s[x]
    else
        return x
    end
end

function unify(x, y, s)
    x = binding(x, s)
    y = binding(y, s)
    if x == y || (x isa DataTyp && y isa DataTyp && isasubtype(x,y))
        return s
    end
    if x isa TypeVar
        s[x] = y
        return s
    end
    s[y] = x
    if y isa TypeVar
        return s
    end
    if typeof(x) == typeof(y)
        if x isa ListTyp || x isa DataTyp
            if unify(x, y, s) === nothing
                return nothing
            end
        else
            for (x_, y_) in zip(x.elems, y.elems)
                if unify(x_, y_, s) === nothing
                    return nothing
                end
            end
        end
        return s
    else
        return nothing
    end
end

unify(x, y) = unify(x, y, Dict())

# unify(@parseTypeExpr((:a => :b) => ([:a] => [:b])), @parseTypeExpr((Int64 => Float64) => ([Int64] => [Float64])))

function get_substitutions(expr1::Union{Expr, Symbol}, expr2::Union{Expr, Symbol})
    return get_substitutions(parseTypeExpr(expr1), parseTypeExpr(expr2))
end

function get_substitutions(expr1::Union{Expr, Symbol}, expr2::Typ)
    return get_substitutions(parseTypeExpr(expr1), expr2)
end

get_substitutions(expr1::Typ, expr2::Union{Expr, Symbol}) = get_substitutions(expr2, expr1)

function get_substitutions(expr1s::Vector{Union{Expr, Symbol}}, expr2)
    d = Dict()
    for expr1 in expr1s
        d2 = get_substitutions(expr1, expr2)
        if !isnothing(d2)
            d = merge(d, d2)
        end
    end
    return !isempty(d) ? d : nothing
end

get_substitutions(expr1, expr2s::Vector{Union{Expr, Symbol}}) = get_substitutions(expr2s, expr1)

function get_substitutions(expr1::Typ, expr2::Typ)
    d = unify(expr1, expr2)
    if !isnothing(d) && isempty(d)
        return true
    elseif !isnothing(d)
        #return [k => v for (k, v) in d if k isa TypeVar]
        return [k => v for (k, v) in d if hasproperty(k, :name)]
    else
        return nothing
    end
end

function get_argtypes(expr::Union{Expr, Symbol})
    return get_argtypes(parseTypeExpr(expr))
end

function get_argtypes(expr::Union{FunTyp,GenFunTyp,DistFunTyp})
    if expr.elems[1] isa ProdTyp
        return expr.elems[1].elems
    else
        return [expr.elems[1]]
    end
end

function get_argtypes(expr::DataTyp)
    return []
end

function is_traceable(expr::Union{Expr, Symbol})
    return is_traceable(parseTypeExpr(expr))
end

function is_traceable(expr::Typ)
    if expr isa FunTyp
        return_type = last(expr.elems)
    else
        return_type = expr
    end
    if return_type isa GenFunTyp || return_type isa DistFunTyp
        return true
    else
        return false
    end
end

