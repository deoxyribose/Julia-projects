abstract type Typ end

struct TypeVar <: Typ
    name::Symbol
end

struct DataTyp <: Typ
    name::Type
end

struct TypeExpr <: Typ
    terms::Pair{Typ,Typ}
end

#TypeExpr(TypeExpr(TypeExpr(TypeVar(:a) => TypeVar(:b)) => DataTyp(Symbol)) => TypeVar(:b))


function parseTypeExpr(x)
    if x isa QuoteNode 
        return TypeVar(eval(x))
    elseif x isa Symbol
        return DataTyp(eval(x))
    elseif x isa Expr
        @assert(x.head == :call)
        arguments = [parseTypeExpr(y) for y in x.args[2:end]]
        return TypeExpr(arguments[1] => arguments[2])
    else
        println("x is a ", typeof(x))
    end
end

macro parseTypeExpr(x)
    return :($(parseTypeExpr(x)))
end