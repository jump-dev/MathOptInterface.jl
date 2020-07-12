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

@testset "IndexMap" begin
    map = MOIU.IndexMap()
    @test length(map) == 0
    compare_without_moi(sprint(show, map), "Utilities.IndexMap()")
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    cx = MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}(1)
    cy = MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}(2)
    map = MOIU.IndexMap(Dict(x => y), DoubleDicts.IndexDoubleDict())
    map.conmap[cx] = cy
    @test length(map) == 2
    # `x=>y` in Julia <= 1.1 and `x => y` in Julia >= 1.2
    if VERSION < v"1.2"
        x_y = string(x) *  "=>" * string(y)
    else
        x_y = string(x => y)
    end
    compare_without_moi(sprint(show, map), "Utilities.IndexMap($x_y,$cx => $cy)")
end

@testset "AUTOMATIC" begin
    src = DummyModel()
    dest = DummyModel()
    @test_throws ErrorException MOIU.automatic_copy_to(dest, src)
    try
        @test_throws ErrorException MOIU.automatic_copy_to(dest, src)
    catch err
        @test sprint(showerror, err) == "Model DummyModel does not" *
        " support copy with names."
    end
end
@testset "Default" begin
    @test !MOIU.supports_default_copy_to(DummyModel(), false)
    @test !MOIU.supports_default_copy_to(DummyModel(), true)
    model = MOIU.Model{Float64}()
    MOIT.failcopytestc(model)
    MOIT.failcopytestia(model)
    MOIT.failcopytestva(model)
    MOIT.failcopytestca(model)
    MOIT.copytest(model, MOIU.Model{Float64}())
end
@testset "Allocate-Load" begin
    @test !MOIU.supports_allocate_load(DummyModel(), false)
    @test !MOIU.supports_allocate_load(DummyModel(), true)
    mock = MOIU.MockOptimizer(MOIU.Model{Float64}(), needs_allocate_load=true)
    MOIT.failcopytestc(mock)
    MOIT.failcopytestia(mock)
    MOIT.failcopytestva(mock)
    MOIT.failcopytestca(mock)
    MOIT.copytest(mock, MOIU.Model{Float64}())
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
        DummyEvaluator(), false)
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
    empty!(model.added_constrained)
end

function MOIU.supports_default_copy_to(model::ConstrainedVariablesModel, ::Bool)
    return !model.allocate_load
end
function MOIU.supports_allocate_load(model::ConstrainedVariablesModel, ::Bool)
    return model.allocate_load
end
function MOI.copy_to(dest::ConstrainedVariablesModel, src::MOI.ModelLike; kws...)
    MOIU.automatic_copy_to(dest, src; kws...)
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
function MOIU.load_variables(model::ConstrainedVariablesModel, n)
end
function MOI.add_constrained_variables(model::ConstrainedVariablesModel,
                                       set::MOI.AbstractVectorSet)
    m = length(model.added_constrained)
    for i in 1:MOI.dimension(set)
        push!(model.added_constrained, true)
    end
    ci = MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}(m + 1)
    return MOI.VariableIndex.(m .+ (1:MOI.dimension(set))), ci
end
function MOIU.allocate_constrained_variables(model::ConstrainedVariablesModel, set::MOI.AbstractVectorSet)
    return MOI.add_constrained_variables(model, set)
end
function MOIU.load_constrained_variables(model::ConstrainedVariablesModel, vis::Vector{MOI.VariableIndex}, ci::MOI.ConstraintIndex{MOI.VectorOfVariables}, set::MOI.AbstractVectorSet)
end
function MOI.add_constraint(model::ConstrainedVariablesModel,
                            func::MOI.VectorOfVariables,
                            set::MOI.AbstractVectorSet)
    return MOI.ConstraintIndex{typeof(func), typeof(set)}(func.variables[1].value)
end
function MOIU.allocate_constraint(model::ConstrainedVariablesModel, func::MOI.VectorOfVariables, set::MOI.AbstractVectorSet)
    return MOI.add_constraint(model, func, set)
end
function MOIU.load_constraint(model::ConstrainedVariablesModel, ::MOI.ConstraintIndex{MOI.VectorOfVariables}, ::MOI.VectorOfVariables, ::MOI.AbstractVectorSet)
end

@testset "Duplicates in VectorOfVariables: $allocate_load" for allocate_load in [false, true]
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
mutable struct OrderConstrainedVariablesModel <: AbstractConstrainedVariablesModel
    constraintIndices       ::Array{MOI.ConstraintIndex}
    inner                   ::MOIU.Model{Float64}
    OrderConstrainedVariablesModel() = new(MOI.ConstraintIndex[], MOIU.Model{Float64}())
end
mutable struct ReverseOrderConstrainedVariablesModel <: AbstractConstrainedVariablesModel
    constraintIndices       ::Array{MOI.ConstraintIndex}
    inner                   ::MOIU.Model{Float64}
    ReverseOrderConstrainedVariablesModel() = new(MOI.ConstraintIndex[], MOIU.Model{Float64}())
end



MOI.add_variables(model::AbstractConstrainedVariablesModel, n) = MOI.add_variables(model.inner, n)
MOI.add_variable(model::AbstractConstrainedVariablesModel) = MOI.add_variable(model.inner)

function MOI.add_constraint(model::AbstractConstrainedVariablesModel, f::F, s::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    ci = MOI.add_constraint(model.inner, f, s)
    push!(model.constraintIndices, ci)
    return ci
end

function MOI.copy_to(dest::AbstractConstrainedVariablesModel, src::MOI.ModelLike; kws...)
    MOIU.automatic_copy_to(dest, src; kws...)
end

MOIU.supports_default_copy_to(model::AbstractConstrainedVariablesModel, ::Bool) = true

function MOI.empty!(model::AbstractConstrainedVariablesModel)
    model.constraintIndices = MOI.ConstraintIndex[]
    MOI.empty!(model.inner)
end


MOI.supports_constraint(::OrderConstrainedVariablesModel, ::Type{MOI.VectorOfVariables}, ::Type{MOI.Nonnegatives}) = false
MOI.supports_add_constrained_variables(::OrderConstrainedVariablesModel, ::Type{MOI.Nonnegatives}) = true
MOI.supports_constraint(::OrderConstrainedVariablesModel, ::Type{MOI.VectorOfVariables}, ::Type{MOI.Nonnegatives}) = true
MOI.supports_add_constrained_variables(::OrderConstrainedVariablesModel, ::Type{MOI.Nonpositives}) = false

MOI.supports_constraint(::ReverseOrderConstrainedVariablesModel, ::Type{MOI.VectorOfVariables}, ::Type{MOI.Nonnegatives}) = true
MOI.supports_add_constrained_variables(::ReverseOrderConstrainedVariablesModel, ::Type{MOI.Nonnegatives}) = false
MOI.supports_constraint(::ReverseOrderConstrainedVariablesModel, ::Type{MOI.VectorOfVariables}, ::Type{MOI.Nonnegatives}) = false
MOI.supports_add_constrained_variables(::ReverseOrderConstrainedVariablesModel, ::Type{MOI.Nonpositives}) = true


MOI.supports_constraint(::OrderConstrainedVariablesModel, ::Type{MOI.SingleVariable}, ::Type{<:MOI.GreaterThan}) = true
MOI.supports_add_constrained_variable(::OrderConstrainedVariablesModel, ::Type{<:MOI.GreaterThan}) = false
MOI.supports_constraint(::OrderConstrainedVariablesModel, ::Type{MOI.SingleVariable}, ::Type{<:MOI.LessThan}) = false
MOI.supports_add_constrained_variable(::OrderConstrainedVariablesModel, ::Type{<:MOI.LessThan}) = true

MOI.supports_constraint(::ReverseOrderConstrainedVariablesModel, ::Type{MOI.SingleVariable}, ::Type{<:MOI.GreaterThan}) = false
MOI.supports_add_constrained_variable(::ReverseOrderConstrainedVariablesModel, ::Type{<:MOI.GreaterThan}) = true
MOI.supports_constraint(::ReverseOrderConstrainedVariablesModel, ::Type{MOI.SingleVariable}, ::Type{<:MOI.LessThan}) = true
MOI.supports_add_constrained_variable(::ReverseOrderConstrainedVariablesModel, ::Type{<:MOI.LessThan}) = false


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
end
