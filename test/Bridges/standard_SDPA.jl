# Model similar to SDPA format, it gives a good example because it does not
# support a lot hence need a lot of bridges
MOI.Utilities.@model(
    StandardSDPAModel,
    (),
    (MOI.EqualTo,),
    (MOI.Nonnegatives, MOI.PositiveSemidefiniteConeTriangle),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

function MOI.supports_constraint(
    ::StandardSDPAModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{
        <:Union{
            MOI.GreaterThan{T},
            MOI.LessThan{T},
            MOI.EqualTo{T},
            MOI.Interval{T},
            MOI.ZeroOne,
            MOI.Integer,
        },
    },
) where {T}
    return false
end

function MOI.supports_constraint(
    ::StandardSDPAModel{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Reals},
) where {T}
    return false
end

function MOI.supports_add_constrained_variables(
    ::StandardSDPAModel,
    ::Type{<:Union{MOI.Nonnegatives,MOI.PositiveSemidefiniteConeTriangle}},
)
    return true
end

function MOI.supports_add_constrained_variables(
    ::StandardSDPAModel,
    ::Type{MOI.Reals},
)
    return false
end

function MOI.supports(
    ::StandardSDPAModel{T},
    ::MOI.ObjectiveFunction{
        <:Union{MOI.VariableIndex,MOI.ScalarQuadraticFunction{T}},
    },
) where {T}
    return false
end
