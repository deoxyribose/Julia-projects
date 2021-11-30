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
    elems::Typ
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
    if x == y
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
        if x isa ListTyp
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

function get_subtitutions(expr1, expr2)
    d = unify(parseTypeExpr((expr1)), parseTypeExpr(expr2))
    if !isnothing(d)
        return [k => v for (k, v) in d if k isa TypeVar]
    else
        return nothing
    end
end


# occur_check(x::TypeVar, y::FunTyp, s) = any(occur_check(x, a, s) for a in y.elems)

# function occur_check(x::TypeVar, y::TypeVar, s)
#     if x == y
#         return True
#     elseif haskey(s, y)
#         return occur_check(x, s[y], s)
#     else
#         return nothing
#     end
# end


# function unify(x::TypeVar, y::Union{TypeVar,Typ}, s)
#     if x == y
#         return s
#     elseif haskey(s, x)
#         return unify(s[x], y, s)
#     elseif haskey(s, y) # This is the norvig twist
#         return unify(x, s[y], s)
#     elseif occur_check(x, y, s)
#         return nothing
#     else
#         s[x] = y
#         return s
#     end
# end

# unify(x::Typ, y::TypeVar, s) = unify(y, x, s)

# function unify(x::Typ, y::Typ, s)
#     if typeof(x) == typeof(y)
#         for (x1, y1) in zip(x.elems, y.elems)
#             if unify(x1, y1, s) === nothing
#                 return nothing
#             end
#         end
#         return s
#     else
#         return nothing
#     end
# end

# unify(x, y) = unify(x, y, Dict())
