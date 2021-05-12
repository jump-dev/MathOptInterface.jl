using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

const DoubleDicts = MathOptInterface.Utilities.DoubleDicts

include("../dummy.jl")

remove_moi(x::String) = replace(x, "MathOptInterface." => "")
function compare_without_moi(x::String, y::String)
    @test remove_moi(x) == remove_moi(y)
end

function identity_index_map_test(::Type{T}) where {T}
    model = MOI.Utilities.Model{T}()
    x, y = MOI.add_variables(model, 2)
    fx = MOI.SingleVariable(x)
    fy = MOI.SingleVariable(y)
    cx = MOI.add_constraint(model, fx, MOI.EqualTo(one(T)))
    cy = MOI.add_constraint(model, fy, MOI.EqualTo(zero(T)))
    c = MOI.add_constraint(model, one(T) * fx + fy, MOI.LessThan(zero(T)))
    index_map = MOI.Utilities.identity_index_map(model)
    @test x == index_map[x]
    @test y == index_map[y]
    @test cx == index_map[cx]
    @test cy == index_map[cy]
    @test c == index_map[c]
end

@testset "IndexMap" begin
    map = MOIU.IndexMap()
    @test length(map) == 0
    compare_without_moi(sprint(show, map), "Utilities.IndexMap()")
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    cx = MOI.ConstraintIndex{MOI.SingleVariable,MOI.Integer}(1)
    cy = MOI.ConstraintIndex{MOI.SingleVariable,MOI.Integer}(2)
    map = MOIU.IndexMap(Dict(x => y), DoubleDicts.IndexDoubleDict())
    map[cx] = cy
    @test length(map) == 2
end

@testset "AUTOMATIC" begin
    src = DummyModel()
    dest = DummyModel()
    @test_throws ErrorException MOIU.automatic_copy_to(dest, src)
    try
        @test_throws ErrorException MOIU.automatic_copy_to(dest, src)
    catch err
        @test sprint(showerror, err) ==
              "Model DummyModel does not" * " support copy with names."
    end
end
@testset "Default" begin
    @test !MOI.supports_incremental_interface(DummyModel(), false)
    @test !MOI.supports_incremental_interface(DummyModel(), true)
    model = MOIU.Model{Float64}()
    MOIT.failcopytestc(model)
    MOIT.failcopytestia(model)
    MOIT.failcopytestva(model)
    MOIT.failcopytestca(model)
    MOIT.copytest(model, MOIU.Model{Float64}(), copy_names = false)
    MOIT.copytest(model, MOIU.Model{Float64}(), copy_names = true)
end
@testset "Allocate-Load" begin
    @test !MOIU.supports_allocate_load(DummyModel(), false)
    @test !MOIU.supports_allocate_load(DummyModel(), true)
    mock = MOIU.MockOptimizer(MOIU.Model{Float64}(), needs_allocate_load = true)
    MOIT.failcopytestc(mock)
    MOIT.failcopytestia(mock)
    MOIT.failcopytestva(mock)
    MOIT.failcopytestca(mock)
    MOIT.copytest(mock, MOIU.Model{Float64}(), copy_names = false)
    MOIT.copytest(mock, MOIU.Model{Float64}(), copy_names = true)
end

struct DummyEvaluator <: MOI.AbstractNLPEvaluator end

@testset "Create variables in same ordering when NLPBlock is used (#849)" begin
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
    index_map = MOIU.default_copy_to(copy, model, true)
    for vi in [a, b, c, x, y[1]]
        @test index_map[vi] == vi
    end
    for ci in [cx, cy]
        @test index_map[ci] == ci
    end
end

# Model that distinguish variables created constrained
struct ConstrainedVariablesModel <: MOI.ModelLike
    allocate_load::Bool
    added_constrained::Vector{Bool}
end
function ConstrainedVariablesModel(allocate_load::Bool)
    return ConstrainedVariablesModel(allocate_load, Bool[])
end
function MOI.empty!(model::ConstrainedVariablesModel)
    return empty!(model.added_constrained)
end

function MOI.supports_incremental_interface(
    model::ConstrainedVariablesModel,
    ::Bool,
)
    return !model.allocate_load
end
function MOIU.supports_allocate_load(model::ConstrainedVariablesModel, ::Bool)
    return model.allocate_load
end
function MOI.copy_to(
    dest::ConstrainedVariablesModel,
    src::MOI.ModelLike;
    kws...,
)
    return MOIU.automatic_copy_to(dest, src; kws...)
end

function MOI.add_variables(model::ConstrainedVariablesModel, n)
    m = length(model.added_constrained)
    for i in 1:n
        push!(model.added_constrained, false)
    end
    return MOI.VariableIndex.(m .+ (1:n))
end
function MOIU.allocate_variables(model::ConstrainedVariablesModel, n)
    return MOI.add_variables(model, n)
end
function MOIU.load_variables(model::ConstrainedVariablesModel, n) end
function MOI.add_constrained_variables(
    model::ConstrainedVariablesModel,
    set::MOI.AbstractVectorSet,
)
    m = length(model.added_constrained)
    for i in 1:MOI.dimension(set)
        push!(model.added_constrained, true)
    end
    ci = MOI.ConstraintIndex{MOI.VectorOfVariables,typeof(set)}(m + 1)
    return MOI.VariableIndex.(m .+ (1:MOI.dimension(set))), ci
end
function MOIU.allocate_constrained_variables(
    model::ConstrainedVariablesModel,
    set::MOI.AbstractVectorSet,
)
    return MOI.add_constrained_variables(model, set)
end
function MOIU.load_constrained_variables(
    model::ConstrainedVariablesModel,
    vis::Vector{MOI.VariableIndex},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
    set::MOI.AbstractVectorSet,
) end
function MOI.add_constraint(
    model::ConstrainedVariablesModel,
    func::MOI.VectorOfVariables,
    set::MOI.AbstractVectorSet,
)
    return MOI.ConstraintIndex{typeof(func),typeof(set)}(
        func.variables[1].value,
    )
end
function MOIU.allocate_constraint(
    model::ConstrainedVariablesModel,
    func::MOI.VectorOfVariables,
    set::MOI.AbstractVectorSet,
)
    return MOI.add_constraint(model, func, set)
end
function MOIU.load_constraint(
    model::ConstrainedVariablesModel,
    ::MOI.ConstraintIndex{MOI.VectorOfVariables},
    ::MOI.VectorOfVariables,
    ::MOI.AbstractVectorSet,
) end

@testset "Duplicates in VectorOfVariables: $allocate_load" for allocate_load in
                                                               [false, true]
    src = MOIU.Model{Int}()
    x = MOI.add_variables(src, 3)
    cx = MOI.add_constraint(src, [x[1], x[3], x[1], x[2]], MOI.Nonnegatives(4))
    y, cy = MOI.add_constrained_variables(src, MOI.Nonpositives(3))
    dest = ConstrainedVariablesModel(allocate_load)
    idxmap = MOI.copy_to(dest, src)
    for vi in x
        @test !dest.added_constrained[idxmap[vi].value]
    end
    for vi in y
        @test dest.added_constrained[idxmap[vi].value]
    end
end

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
    kws...,
)
    return MOIU.automatic_copy_to(dest, src; kws...)
end

function MOI.supports_incremental_interface(
    model::AbstractConstrainedVariablesModel,
    ::Bool,
)
    return true
end

function MOI.empty!(model::AbstractConstrainedVariablesModel)
    model.constraintIndices = MOI.ConstraintIndex[]
    return MOI.empty!(model.inner)
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
    ::Type{MOI.SingleVariable},
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
    ::Type{MOI.SingleVariable},
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
    ::Type{MOI.SingleVariable},
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
    ::Type{MOI.SingleVariable},
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

@testset "Create variables using supports_add_constrained_variable(s) (#987)" begin
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
    @test MOIU.sorted_variable_sets_by_cost(bridged_dest, src) == [
        (MOI.VectorOfVariables, MOI.Zeros),
        (MOI.VectorOfVariables, MOI.Nonnegatives),
        (MOI.VectorOfVariables, MOI.Nonpositives),
    ]
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
    @test MOIU.sorted_variable_sets_by_cost(bridged_dest, src) == [
        (MOI.VectorOfVariables, MOI.Zeros),
        (MOI.VectorOfVariables, MOI.Nonpositives),
        (MOI.VectorOfVariables, MOI.Nonnegatives),
    ]
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
        MOI.ConstraintBridgingCost{MOI.SingleVariable,MOI.LessThan{Float64}}(),
    ) == 2.0
    @test MOI.get(
        bridged_dest,
        MOI.VariableBridgingCost{MOI.GreaterThan{Float64}}(),
    ) == 1.0
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{
            MOI.SingleVariable,
            MOI.GreaterThan{Float64},
        }(),
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
        MOI.ConstraintBridgingCost{MOI.SingleVariable,MOI.LessThan{Float64}}(),
    ) == 0.0
    @test MOI.get(
        bridged_dest,
        MOI.VariableBridgingCost{MOI.GreaterThan{Float64}}(),
    ) == 0.0
    @test MOI.get(
        bridged_dest,
        MOI.ConstraintBridgingCost{
            MOI.SingleVariable,
            MOI.GreaterThan{Float64},
        }(),
    ) == 2.0
    index_map = MOI.copy_to(bridged_dest, src)
    @test typeof(c1) == typeof(dest.constraintIndices[1])
    @test typeof(c2) == typeof(dest.constraintIndices[2])
end

@testset "Filtering copy: check based on index" begin
    # Create a basic model.
    src = MOIU.Model{Float64}()
    x = MOI.add_variable(src)
    c1 = MOI.add_constraint(src, x, MOI.LessThan{Float64}(1.0))
    c2 = MOI.add_constraint(src, x, MOI.GreaterThan{Float64}(0.0))

    # Filtering function: the default case where this function always returns
    # true is already well-tested by the above cases.
    # Only keep the constraint c1.
    f = (cidx) -> cidx == c1

    # Perform the copy.
    dst = OrderConstrainedVariablesModel()
    index_map = MOI.copy_to(dst, src, filter_constraints = f)

    @test typeof(c1) == typeof(dst.constraintIndices[1])
    @test length(dst.constraintIndices) == 1
end

mutable struct BoundModel <: MOI.ModelLike
    # Type of model that only supports ≤ bound constraints. In particular, it does not support integrality constraints.
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
    ::Type{MOI.SingleVariable},
    ::MOI.LessThan{Float64},
)
    return true
end

MOI.supports_incremental_interface(::BoundModel, ::Bool) = true
function MOI.copy_to(dest::BoundModel, src::MOI.ModelLike; kws...)
    return MOIU.automatic_copy_to(dest, src; kws...)
end
MOI.empty!(model::BoundModel) = MOI.empty!(model.inner)

MOI.supports(::BoundModel, ::Type{MOI.NumberOfConstraints}) = true
function MOI.get(model::BoundModel, attr::MOI.NumberOfConstraints)
    return MOI.get(model.inner, attr)
end

@testset "Filtering copy: check based on constraint type" begin
    # Create a basic model.
    src = MOIU.Model{Float64}()
    x = MOI.add_variable(src)
    c1 = MOI.add_constraint(src, x, MOI.LessThan{Float64}(10.0))
    c2 = MOI.add_constraint(src, x, MOI.Integer())

    # Filtering function: get rid of integrality constraint.
    f = (cidx) -> MOI.get(src, MOI.ConstraintSet(), cidx) != MOI.Integer()

    # Perform the unfiltered copy. This should throw an error (i.e. the implementation of BoundModel
    # should be correct).
    dst = BoundModel()
    @test_throws MOI.UnsupportedConstraint{MOI.SingleVariable,MOI.Integer} MOI.copy_to(
        dst,
        src,
    )

    # Perform the filtered copy. This should not throw an error.
    dst = BoundModel()
    MOI.copy_to(dst, src, filter_constraints = f)
    @test MOI.get(
        dst,
        MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}(),
    ) == 1
    @test MOI.get(
        dst,
        MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Integer}(),
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
    pass_cons;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    return MOIU.pass_nonvariable_constraints(
        dest.constraints,
        src,
        idxmap,
        constraint_types,
        pass_cons;
        filter_constraints = filter_constraints,
    )
end

function test_pass_copy(::Type{T}) where {T}
    F = MOI.ScalarAffineFunction{T}
    S = MOI.EqualTo{T}
    S2 = MOI.GreaterThan{T}
    src = MOIU.Model{T}()
    x = MOI.add_variable(src)
    fx = MOI.SingleVariable(x)
    MOI.add_constraint(src, T(1) * fx, MOI.EqualTo(T(1)))
    MOI.add_constraint(src, T(2) * fx, MOI.EqualTo(T(2)))
    MOI.add_constraint(src, T(3) * fx, MOI.GreaterThan(T(3)))
    MOI.add_constraint(src, T(4) * fx, MOI.GreaterThan(T(4)))
    dest = MOIU.CachingOptimizer(
        MOI.Bridges.full_bridge_optimizer(
            MOIU.UniversalFallback(
                MOIU.GenericOptimizer{T,OnlyCopyConstraints{F,S}}(),
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
end

@testset "copy of constraints passed as copy accross layers" begin
    test_pass_copy(Int)
    test_pass_copy(Float64)
end

@testset "identity_index_map" begin
    identity_index_map_test(Int)
    identity_index_map_test(Float64)
end
