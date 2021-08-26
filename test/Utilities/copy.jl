module TestCopy

using Test

import MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

const DoubleDicts = MathOptInterface.Utilities.DoubleDicts

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

###
### Test helpers
###

abstract type AbstractDummyModel <: MOI.ModelLike end

function MOI.empty!(::AbstractDummyModel) end

function MOI.copy_to(dest::AbstractDummyModel, src::MOI.ModelLike)
    return MOIU.default_copy_to(dest, src)
end

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
    ::Type{MOI.VariableIndex},
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
    ::MOI.VariableIndex,
    ::MOI.EqualTo{Float64},
)
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}}(0)
end

struct DummyEvaluator <: MOI.AbstractNLPEvaluator end

###
### The tests
###

function _test_identity_index_map(::Type{T}) where {T}
    model = MOI.Utilities.Model{T}()
    x, y = MOI.add_variables(model, 2)
    cx = MOI.add_constraint(model, x, MOI.EqualTo(one(T)))
    cy = MOI.add_constraint(model, y, MOI.EqualTo(zero(T)))
    c = MOI.add_constraint(model, one(T) * x + y, MOI.LessThan(zero(T)))
    index_map = MOI.Utilities.identity_index_map(model)
    @test x == index_map[x]
    @test y == index_map[y]
    @test cx == index_map[cx]
    @test cy == index_map[cy]
    @test c == index_map[c]
    return
end

function test_identity_index_map()
    _test_identity_index_map(Int)
    _test_identity_index_map(Float64)
    return
end

function test_IndexMap()
    map = MOIU.IndexMap()
    @test length(map) == 0
    @test occursin("Utilities.IndexMap()", sprint(show, map))
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    cx = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(1)
    cy = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(2)
    map = MOIU.IndexMap(Dict(x => y), DoubleDicts.IndexDoubleDict())
    map[cx] = cy
    @test length(map) == 2
    return
end

function test_AUTOMATIC()
    src = DummyModel()
    dest = DummyModel()
    @test_throws ErrorException MOIU.default_copy_to(dest, src)
    try
        @test_throws ErrorException MOIU.default_copy_to(dest, src)
    catch err
        @test sprint(showerror, err) ==
              "Model DummyModel does not support copy with names."
    end
end

"""
    test_issue_849()

Create variables in same ordering when NLPBlock is used (#849)
"""
function test_issue_849()
    model = MOIU.UniversalFallback(MOIU.Model{Float64}())
    a = MOI.add_variable(model)
    b, c = MOI.add_variables(model, 2)
    x, cx = MOI.add_constrained_variable(model, MOI.GreaterThan(0.0))
    y, cy = MOI.add_constrained_variables(model, MOI.Nonnegatives(1))
    nlp_data = MOI.NLPBlockData(
        [MOI.NLPBoundsPair(0.0, 1.0) for i in 1:5],
        DummyEvaluator(),
        false,
    )
    MOI.set(model, MOI.NLPBlock(), nlp_data)
    copy = MOIU.UniversalFallback(MOIU.Model{Float64}())
    index_map = MOIU.default_copy_to(copy, model)
    for vi in [a, b, c, x, y[1]]
        @test index_map[vi] == vi
    end
    for ci in [cx, cy]
        @test index_map[ci] == ci
    end
    return
end

###
### ConstrainedVariablesModel
###

"""
    ConstrainedVariablesModel

Model that distinguish variables created constrained
"""
struct ConstrainedVariablesModel <: MOI.ModelLike
    added_constrained::Vector{Bool}
end

MOI.empty!(model::ConstrainedVariablesModel) = empty!(model.added_constrained)

MOI.supports_incremental_interface(::ConstrainedVariablesModel) = true

function MOI.copy_to(
    dest::ConstrainedVariablesModel,
    src::MOI.ModelLike;
    kwargs...,
)
    return MOIU.default_copy_to(dest, src; kwargs...)
end

function MOI.add_variables(model::ConstrainedVariablesModel, n)
    m = length(model.added_constrained)
    for _ in 1:n
        push!(model.added_constrained, false)
    end
    return MOI.VariableIndex.(m .+ (1:n))
end

function MOI.add_constrained_variables(
    model::ConstrainedVariablesModel,
    set::MOI.AbstractVectorSet,
)
    m = length(model.added_constrained)
    for _ in 1:MOI.dimension(set)
        push!(model.added_constrained, true)
    end
    ci = MOI.ConstraintIndex{MOI.VectorOfVariables,typeof(set)}(m + 1)
    return MOI.VariableIndex.(m .+ (1:MOI.dimension(set))), ci
end

function MOI.add_constraint(
    ::ConstrainedVariablesModel,
    func::MOI.VectorOfVariables,
    set::MOI.AbstractVectorSet,
)
    return MOI.ConstraintIndex{typeof(func),typeof(set)}(
        func.variables[1].value,
    )
end

function test_ConstrainedVariablesModel()
    src = MOIU.Model{Int}()
    x = MOI.add_variables(src, 3)
    cx = MOI.add_constraint(src, [x[1], x[3], x[1], x[2]], MOI.Nonnegatives(4))
    y, cy = MOI.add_constrained_variables(src, MOI.Nonpositives(3))
    dest = ConstrainedVariablesModel(Bool[])
    idxmap = MOI.copy_to(dest, src)
    for vi in x
        @test !dest.added_constrained[idxmap[vi].value]
    end
    for vi in y
        @test dest.added_constrained[idxmap[vi].value]
    end
    return
end

###
### AbstractConstrainedVariablesModel
###

abstract type AbstractConstrainedVariablesModel <: MOI.ModelLike end

mutable struct OrderConstrainedVariablesModel <:
               AbstractConstrainedVariablesModel
    constraintIndices::Array{MOI.ConstraintIndex}
    inner::MOIU.Model{Float64}
    function OrderConstrainedVariablesModel()
        return new(MOI.ConstraintIndex[], MOIU.Model{Float64}())
    end
end
mutable struct ReverseOrderConstrainedVariablesModel <:
               AbstractConstrainedVariablesModel
    constraintIndices::Array{MOI.ConstraintIndex}
    inner::MOIU.Model{Float64}
    function ReverseOrderConstrainedVariablesModel()
        return new(MOI.ConstraintIndex[], MOIU.Model{Float64}())
    end
end

function MOI.add_variables(model::AbstractConstrainedVariablesModel, n)
    return MOI.add_variables(model.inner, n)
end

function MOI.add_variable(model::AbstractConstrainedVariablesModel)
    return MOI.add_variable(model.inner)
end

function MOI.add_constraint(
    model::AbstractConstrainedVariablesModel,
    f::MOI.AbstractFunction,
    s::MOI.AbstractSet,
)
    ci = MOI.add_constraint(model.inner, f, s)
    push!(model.constraintIndices, ci)
    return ci
end

function MOI.copy_to(
    dest::AbstractConstrainedVariablesModel,
    src::MOI.ModelLike;
    kwargs...,
)
    return MOIU.default_copy_to(dest, src; kwargs...)
end

MOI.supports_incremental_interface(::AbstractConstrainedVariablesModel) = true

function MOI.empty!(model::AbstractConstrainedVariablesModel)
    model.constraintIndices = MOI.ConstraintIndex[]
    MOI.empty!(model.inner)
    return
end

function MOI.supports_constraint(
    ::OrderConstrainedVariablesModel,
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Nonnegatives},
)
    return false
end

function MOI.supports_add_constrained_variables(
    ::OrderConstrainedVariablesModel,
    ::Type{MOI.Nonnegatives},
)
    return true
end

function MOI.supports_constraint(
    ::OrderConstrainedVariablesModel,
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Nonnegatives},
)
    return true
end

function MOI.supports_add_constrained_variables(
    ::OrderConstrainedVariablesModel,
    ::Type{MOI.Nonpositives},
)
    return false
end

function MOI.supports_constraint(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Nonnegatives},
)
    return true
end

function MOI.supports_add_constrained_variables(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{MOI.Nonnegatives},
)
    return false
end

function MOI.supports_constraint(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Nonnegatives},
)
    return false
end

function MOI.supports_add_constrained_variables(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{MOI.Nonpositives},
)
    return true
end

function MOI.supports_constraint(
    ::OrderConstrainedVariablesModel,
    ::Type{MOI.VectorAffineFunction{Float64}},
    ::Type{MOI.Nonnegatives},
)
    return true
end

function MOI.supports_constraint(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{MOI.VectorAffineFunction{Float64}},
    ::Type{MOI.Nonpositives},
)
    return true
end

function MOI.supports_constraint(
    ::OrderConstrainedVariablesModel,
    ::Type{MOI.VariableIndex},
    ::Type{<:MOI.GreaterThan},
)
    return true
end

function MOI.supports_add_constrained_variable(
    ::OrderConstrainedVariablesModel,
    ::Type{<:MOI.GreaterThan},
)
    return false
end

function MOI.supports_constraint(
    ::OrderConstrainedVariablesModel,
    ::Type{MOI.VariableIndex},
    ::Type{<:MOI.LessThan},
)
    return false
end

function MOI.supports_add_constrained_variable(
    ::OrderConstrainedVariablesModel,
    ::Type{<:MOI.LessThan},
)
    return true
end

function MOI.supports_constraint(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{MOI.VariableIndex},
    ::Type{<:MOI.GreaterThan},
)
    return false
end

function MOI.supports_add_constrained_variable(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{<:MOI.GreaterThan},
)
    return true
end

function MOI.supports_constraint(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{MOI.VariableIndex},
    ::Type{<:MOI.LessThan},
)
    return true
end

function MOI.supports_add_constrained_variable(
    ::ReverseOrderConstrainedVariablesModel,
    ::Type{<:MOI.LessThan},
)
    return false
end

"""
    test_create_variables_using_supports_add_constrained_variable()

From Issue #987
"""
function test_create_variables_using_supports_add_constrained_variable()
    # With vectors
    src = MOIU.Model{Float64}()
    a, c1 = MOI.add_constrained_variables(src, MOI.Nonpositives(3))
    c2 = MOI.add_constraint(src, a, MOI.Nonnegatives(3))

    dest = OrderConstrainedVariablesModel()
    index_map = MOI.copy_to(dest, src)
    @test typeof(c1) == typeof(dest.constraintIndices[2])
    @test typeof(c2) == typeof(dest.constraintIndices[1])

    dest = ReverseOrderConstrainedVariablesModel()
    index_map = MOI.copy_to(dest, src)
    @test typeof(c1) == typeof(dest.constraintIndices[1])
    @test typeof(c2) == typeof(dest.constraintIndices[2])

    b, cb = MOI.add_constrained_variables(src, MOI.Nonnegatives(2))
    c3 = MOI.add_constraint(src, b, MOI.Zeros(2))

    d, cd = MOI.add_constrained_variables(src, MOI.Zeros(2))
    c4 = MOI.add_constraint(src, d, MOI.Nonpositives(2))

    dest = OrderConstrainedVariablesModel()
    bridged_dest = MOI.Bridges.full_bridge_optimizer(dest, Float64)
    @test MOIU.sorted_variable_sets_by_cost(bridged_dest, src) ==
          Type[MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives]
    @test MOI.supports_add_constrained_variables(bridged_dest, MOI.Nonnegatives)
    @test MOI.get(bridged_dest, MOI.VariableBridgingCost{MOI.Nonnegatives}()) ==
          0.0
    @test MOI.supports_constraint(
        bridged_dest,
        MOI.VectorOfVariables,
        MOI.Nonnegatives,
    )
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    ) == 0.0
    @test MOI.supports_add_constrained_variables(bridged_dest, MOI.Nonpositives)
    @test MOI.get(bridged_dest, MOI.VariableBridgingCost{MOI.Nonpositives}()) ==
          1.0
    @test MOI.supports_constraint(
        bridged_dest,
        MOI.VectorOfVariables,
        MOI.Nonpositives,
    )
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VectorOfVariables,MOI.Nonpositives}(),
    ) == 1.0
    @test MOI.supports_add_constrained_variables(bridged_dest, MOI.Zeros)
    @test MOI.get(bridged_dest, MOI.VariableBridgingCost{MOI.Zeros}()) == 1.0
    @test MOI.supports_constraint(
        bridged_dest,
        MOI.VectorOfVariables,
        MOI.Zeros,
    )
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VectorOfVariables,MOI.Zeros}(),
    ) == 2.0
    index_map = MOI.copy_to(bridged_dest, src)
    @test length(dest.constraintIndices) == 4

    dest = ReverseOrderConstrainedVariablesModel()
    bridged_dest = MOI.Bridges.full_bridge_optimizer(dest, Float64)
    @test MOIU.sorted_variable_sets_by_cost(bridged_dest, src) ==
          Type[MOI.Zeros, MOI.Nonpositives, MOI.Nonnegatives]
    @test MOI.supports_add_constrained_variables(bridged_dest, MOI.Nonnegatives)
    @test MOI.get(bridged_dest, MOI.VariableBridgingCost{MOI.Nonnegatives}()) ==
          2.0
    @test MOI.supports_constraint(
        bridged_dest,
        MOI.VectorOfVariables,
        MOI.Nonnegatives,
    )
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    ) == 1.0
    @test MOI.supports_add_constrained_variables(bridged_dest, MOI.Nonpositives)
    @test MOI.get(bridged_dest, MOI.VariableBridgingCost{MOI.Nonpositives}()) ==
          0.0
    @test MOI.supports_constraint(
        bridged_dest,
        MOI.VectorOfVariables,
        MOI.Nonpositives,
    )
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VectorOfVariables,MOI.Nonpositives}(),
    ) == 1.0
    @test MOI.supports_add_constrained_variables(bridged_dest, MOI.Zeros)
    @test MOI.get(bridged_dest, MOI.VariableBridgingCost{MOI.Zeros}()) == 1.0
    @test MOI.supports_constraint(
        bridged_dest,
        MOI.VectorOfVariables,
        MOI.Zeros,
    )
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VectorOfVariables,MOI.Zeros}(),
    ) == 3.0
    index_map = MOI.copy_to(bridged_dest, src)
    @test length(dest.constraintIndices) == 4

    # With single variables
    src = MOIU.Model{Float64}()
    a, c1 = MOI.add_constrained_variable(src, MOI.GreaterThan{Float64}(5.0))
    c2 = MOI.add_constraint(src, a, MOI.LessThan{Float64}(1.0))

    dest = OrderConstrainedVariablesModel()
    index_map = MOI.copy_to(dest, src)
    @test typeof(c1) == typeof(dest.constraintIndices[2])
    @test typeof(c2) == typeof(dest.constraintIndices[1])

    dest = ReverseOrderConstrainedVariablesModel()
    index_map = MOI.copy_to(dest, src)
    @test typeof(c1) == typeof(dest.constraintIndices[1])
    @test typeof(c2) == typeof(dest.constraintIndices[2])

    dest = OrderConstrainedVariablesModel()
    bridged_dest = MOI.Bridges.full_bridge_optimizer(dest, Float64)
    @test MOI.get(
        bridged_dest,
        MOI.VariableBridgingCost{MOI.LessThan{Float64}}(),
    ) == 0.0
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VariableIndex,MOI.LessThan{Float64}}(),
    ) == 2.0
    @test MOI.get(
        bridged_dest,
        MOI.VariableBridgingCost{MOI.GreaterThan{Float64}}(),
    ) == 1.0
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VariableIndex,MOI.GreaterThan{Float64}}(),
    ) == 0.0
    index_map = MOI.copy_to(bridged_dest, src)
    @test typeof(c1) == typeof(dest.constraintIndices[2])
    @test typeof(c2) == typeof(dest.constraintIndices[1])

    dest = ReverseOrderConstrainedVariablesModel()
    bridged_dest = MOI.Bridges.full_bridge_optimizer(dest, Float64)
    @test MOI.get(
        bridged_dest,
        MOI.VariableBridgingCost{MOI.LessThan{Float64}}(),
    ) == 1.0
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VariableIndex,MOI.LessThan{Float64}}(),
    ) == 0.0
    @test MOI.get(
        bridged_dest,
        MOI.VariableBridgingCost{MOI.GreaterThan{Float64}}(),
    ) == 0.0
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{MOI.VariableIndex,MOI.GreaterThan{Float64}}(),
    ) == 2.0
    index_map = MOI.copy_to(bridged_dest, src)
    @test typeof(c1) == typeof(dest.constraintIndices[1])
    @test typeof(c2) == typeof(dest.constraintIndices[2])
end

function test_filtering_copy()
    # Create a basic model.
    src = MOIU.Model{Float64}()
    x = MOI.add_variable(src)
    c1 = MOI.add_constraint(src, x, MOI.LessThan{Float64}(1.0))
    c2 = MOI.add_constraint(src, x, MOI.GreaterThan{Float64}(0.0))

    # Filtering function: the default case where this function always returns
    # true is already well-tested by the above cases.
    # Only keep the constraint c1.
    f(::Any) = true
    f(cidx::MOI.ConstraintIndex) = cidx == c1

    # Perform the copy.
    dst = OrderConstrainedVariablesModel()
    index_map = MOI.copy_to(dst, MOI.Utilities.FilterModel(f, src))

    @test typeof(c1) == typeof(dst.constraintIndices[1])
    @test length(dst.constraintIndices) == 1
    return
end

###
### BoundModel
###
mutable struct BoundModel <: MOI.ModelLike
    # Type of model that only supports ≤ bound constraints. In particular, it
    # does not support integrality constraints.
    inner::MOIU.Model{Float64}
    BoundModel() = new(MOIU.Model{Float64}())
end

MOI.add_variable(model::BoundModel) = MOI.add_variable(model.inner)

function MOI.add_constraint(
    model::BoundModel,
    f::MOI.AbstractFunction,
    s::MOI.LessThan{Float64},
)
    return MOI.add_constraint(model.inner, f, s)
end

function MOI.supports_constraint(
    ::BoundModel,
    ::Type{MOI.VariableIndex},
    ::MOI.LessThan{Float64},
)
    return true
end

MOI.supports_incremental_interface(::BoundModel) = true

function MOI.copy_to(dest::BoundModel, src::MOI.ModelLike; kwargs...)
    return MOIU.default_copy_to(dest, src; kwargs...)
end

MOI.empty!(model::BoundModel) = MOI.empty!(model.inner)

MOI.supports(::BoundModel, ::Type{MOI.NumberOfConstraints}) = true

function MOI.get(model::BoundModel, attr::MOI.NumberOfConstraints)
    return MOI.get(model.inner, attr)
end

function test_BoundModel_filtering_copy()
    # Create a basic model.
    src = MOIU.Model{Float64}()
    x = MOI.add_variable(src)
    c1 = MOI.add_constraint(src, x, MOI.LessThan{Float64}(10.0))
    c2 = MOI.add_constraint(src, x, MOI.Integer())

    # Filtering function: get rid of integrality constraint.
    f(::Any) = true
    f(cidx::MOI.ConstraintIndex) =
        MOI.get(src, MOI.ConstraintSet(), cidx) != MOI.Integer()

    # Perform the unfiltered copy. This should throw an error (i.e. the
    # implementation of BoundModel
    # should be correct).
    dst = BoundModel()
    @test_throws(
        MOI.UnsupportedConstraint{MOI.VariableIndex,MOI.Integer},
        MOI.copy_to(dst, src),
    )

    # Perform the filtered copy. This should not throw an error.
    dst = BoundModel()
    MOI.copy_to(dst, MOI.Utilities.FilterModel(f, src))
    @test MOI.get(
        dst,
        MOI.NumberOfConstraints{MOI.VariableIndex,MOI.LessThan{Float64}}(),
    ) == 1
    @test MOI.get(
        dst,
        MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Integer}(),
    ) == 0
end

# We create a `OnlyCopyConstraints` that don't implement `add_constraint` but
# implements `pass_nonvariable_constraints` to check that this is passed accross
# all layers without falling back to `pass_nonvariable_constraints_fallback`
# which calls `add_constraint`.

struct OnlyCopyConstraints{F,S} <: MOI.ModelLike
    constraints::MOIU.VectorOfConstraints{F,S}
    function OnlyCopyConstraints{F,S}() where {F,S}
        return new{F,S}(MOIU.VectorOfConstraints{F,S}())
    end
end

function MOI.Utilities._add_variable(::OnlyCopyConstraints) end

MOI.empty!(model::OnlyCopyConstraints) = MOI.empty!(model.constraints)

function MOI.supports_constraint(
    model::OnlyCopyConstraints,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOI.supports_constraint(model.constraints, F, S)
end

function MOIU.pass_nonvariable_constraints(
    dest::OnlyCopyConstraints,
    src::MOI.ModelLike,
    idxmap::MOIU.IndexMap,
    constraint_types,
)
    return MOIU.pass_nonvariable_constraints(
        dest.constraints,
        src,
        idxmap,
        constraint_types,
    )
end

function _test_copy_of_constraints_passed_as_copy_accross_layers(
    ::Type{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    S = MOI.EqualTo{T}
    S2 = MOI.GreaterThan{T}
    src = MOIU.Model{T}()
    x = MOI.add_variable(src)
    MOI.add_constraint(src, T(1) * x, MOI.EqualTo(T(1)))
    MOI.add_constraint(src, T(2) * x, MOI.EqualTo(T(2)))
    MOI.add_constraint(src, T(3) * x, MOI.GreaterThan(T(3)))
    MOI.add_constraint(src, T(4) * x, MOI.GreaterThan(T(4)))
    dest = MOIU.CachingOptimizer(
        MOI.Bridges.full_bridge_optimizer(
            MOIU.UniversalFallback(
                MOIU.GenericOptimizer{
                    T,
                    MOI.Utilities.ObjectiveContainer{T},
                    MOI.Utilities.VariablesContainer{T},
                    OnlyCopyConstraints{F,S},
                }(),
            ),
            T,
        ),
        MOIU.AUTOMATIC,
    )
    MOI.copy_to(dest, src)
    voc = dest.model_cache.model.model.constraints.constraints
    @test MOI.get(voc, MOI.NumberOfConstraints{F,S}()) == 2
    @test !haskey(dest.model_cache.model.constraints, (F, S))
    @test MOI.get(dest, MOI.NumberOfConstraints{F,S2}()) == 2
    @test haskey(dest.model_cache.model.constraints, (F, S2))
    return
end

function test_copy_of_constraints_passed_as_copy_accross_layers()
    _test_copy_of_constraints_passed_as_copy_accross_layers(Int)
    _test_copy_of_constraints_passed_as_copy_accross_layers(Float64)
    return
end

end  # module

TestCopy.runtests()
