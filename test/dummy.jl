import MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

abstract type AbstractDummyModel <: MOI.ModelLike end

function MOI.empty!(::AbstractDummyModel) end

function MOI.copy_to(
    dest::AbstractDummyModel,
    src::MOI.ModelLike;
    copy_names::Bool = true,
)
    return MOIU.default_copy_to(dest, src, copy_names)
end

MOI.supports_incremental_interface(::AbstractDummyModel, ::Bool) = true

MOI.supports(::AbstractDummyModel, ::MOI.ObjectiveSense) = true

function MOI.supports(
    ::AbstractDummyModel,
    ::MOI.ConstraintPrimalStart,
    ::Type{<:MOI.ConstraintIndex},
)
    return true
end
function MOI.supports_constraint(
    ::AbstractDummyModel,
    ::Type{MOI.SingleVariable},
    ::Type{MOI.EqualTo{Float64}},
)
    return true
end
function MOI.supports_constraint(
    ::AbstractDummyModel,
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Zeros},
)
    return true
end

struct DummyModel <: AbstractDummyModel end

# Implements add_variable and add_constraint
struct DummyModelWithAdd <: AbstractDummyModel end
MOI.add_variable(::DummyModelWithAdd) = MOI.VariableIndex(0)
MOI.add_variables(::DummyModelWithAdd, n) = fill(MOI.VariableIndex(0), n)
function MOI.add_constraint(
    ::DummyModelWithAdd,
    ::MOI.SingleVariable,
    ::MOI.EqualTo{Float64},
)
    return MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{Float64}}(0)
end
