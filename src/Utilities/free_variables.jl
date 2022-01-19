"""
    mutable struct FreeVariables <: MOI.ModelLike
        n::Int64
        FreeVariables() = new(0)
    end

A struct for storing free variables that can be used as the `variables` field
of [`GenericModel`](@ref) or [`GenericModel`](@ref). It represents a model
that does not support any constraint nor objective function.

## Example

The following model type represents a conic model in geometric form. As opposed
to [`VariablesContainer`](@ref), `FreeVariables` does not support constraint
bounds so they are bridged into an affine constraint in the
[`MathOptInterface.Nonnegatives`](@ref) cone as expected for the geometric
conic form.
```jldocstest
julia> MOI.Utilities.@product_of_sets(
    Cones,
    MOI.Zeros,
    MOI.Nonnegatives,
    MOI.SecondOrderCone,
    MOI.PositiveSemidefiniteConeTriangle,
);

julia> const ConicModel{T} = MOI.Utilities.GenericOptimizer{
    T,
    MOI.Utilities.ObjectiveContainer{T},
    MOI.Utilities.FreeVariables,
    MOI.Utilities.MatrixOfConstraints{
        T,
        MOI.Utilities.MutableSparseMatrixCSC{
            T,
            Int,
            MOI.Utilities.OneBasedIndexing,
        },
        Vector{T},
        Cones{T},
    },
};

julia> model = MOI.instantiate(ConicModel{Float64}, with_bridge_type=Float64);

julia> x = MOI.add_variable(model)
MathOptInterface.VariableIndex(1)

julia> c = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.GreaterThan{Float64}}(1)

julia> MOI.Bridges.is_bridged(model, c)
true

julia> bridge = MOI.Bridges.bridge(model, c)
MathOptInterface.Bridges.Constraint.VectorizeBridge{Float64, MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Nonnegatives, MathOptInterface.VariableIndex}(MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Nonnegatives}(1), 1.0)

julia> bridge.vector_constraint
MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Nonnegatives}(1)

julia> MOI.Bridges.is_bridged(model, bridge.vector_constraint)
false
```
"""
mutable struct FreeVariables <: MOI.ModelLike
    n::Int64
    FreeVariables() = new(0)
end

function MOI.empty!(model::FreeVariables)
    model.n = 0
    return
end

MOI.is_empty(model::FreeVariables) = iszero(model.n)

function MOI.add_variable(model::FreeVariables)
    model.n += 1
    return MOI.VariableIndex(model.n)
end

function MOI.get(model::FreeVariables, ::MOI.ListOfVariableIndices)
    return MOI.VariableIndex[MOI.VariableIndex(i) for i in 1:model.n]
end

function MOI.is_valid(model::FreeVariables, vi::MOI.VariableIndex)
    return 1 <= vi.value <= model.n
end

MOI.get(model::FreeVariables, ::MOI.NumberOfVariables) = model.n

function MOI.get(model::FreeVariables, ::MOI.ListOfConstraintTypesPresent)
    return Tuple{Type,Type}[]
end
