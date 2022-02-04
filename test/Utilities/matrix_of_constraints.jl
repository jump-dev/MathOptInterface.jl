module TestMatrixOfConstraints

using Test

import MathOptInterface
import SparseArrays

const MOI = MathOptInterface

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

MOI.Utilities.@product_of_sets(
    ScalarSets,
    MOI.EqualTo{T},
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.Interval{T},
)

function _new_ScalarSets()
    return MOI.Utilities.GenericOptimizer{
        Float64,
        MOI.Utilities.ObjectiveContainer{Float64},
        MOI.Utilities.VariablesContainer{Float64},
        MOI.Utilities.MatrixOfConstraints{
            Float64,
            MOI.Utilities.MutableSparseMatrixCSC{
                Float64,
                Int,
                MOI.Utilities.ZeroBasedIndexing,
            },
            MOI.Utilities.Hyperrectangle{Float64},
            ScalarSets{Float64},
        },
    }()
end

MOI.Utilities.@product_of_sets(
    VectorSets,
    MOI.Nonnegatives,
    MOI.Nonpositives,
    MOI.SecondOrderCone,
)

function _new_VectorSets()
    return MOI.Utilities.GenericOptimizer{
        Int,
        MOI.Utilities.ObjectiveContainer{Int},
        MOI.Utilities.FreeVariables,
        MOI.Utilities.MatrixOfConstraints{
            Int,
            MOI.Utilities.MutableSparseMatrixCSC{
                Int,
                Int,
                MOI.Utilities.OneBasedIndexing,
            },
            Vector{Int},
            VectorSets{Int},
        },
    }()
end

function test_ScalarSets_basic()
    model = _new_ScalarSets()
    @test MOI.is_empty(model)
    src = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(
        src,
        """
variables: x, y
minobjective: x + y
x >= 1.0
y == 2.1
c: x + 2.0 * y <= 3.0
""",
    )
    index_map = MOI.copy_to(model, src)
    @test MOI.is_empty(model) == false
    for (k, v) in index_map
        @test MOI.is_valid(src, k)
        @test MOI.is_valid(model, v)
    end
    @test length(MOI.get(src, MOI.ListOfConstraintTypesPresent())) == 3
    MOI.empty!(model)
    @test MOI.is_empty(model)
    return
end

function test_VectorSets_basic()
    model = _new_VectorSets()
    @test MOI.is_empty(model)
    src = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(src, 2)
    c = MOI.add_constraint(
        src,
        MOI.VectorAffineFunction{Int}(
            MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.(1, x)),
            [1, 3],
        ),
        MOI.SecondOrderCone(2),
    )
    index_map = MOI.copy_to(model, src)
    @test MOI.is_empty(model) == false
    for (k, v) in index_map
        @test MOI.is_valid(src, k)
        @test MOI.is_valid(model, v)
    end
    @test length(MOI.get(model, MOI.ListOfConstraintTypesPresent())) == 1
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    @test MOI.get(model, MOI.ListOfVariableIndices()) == MOI.VariableIndex.(1:2)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    return
end

function test_ScalarSets_supports_constraint()
    model = _new_ScalarSets()
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test !MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonnegatives,
    )
    return
end

function test_VectorSets_supports_constraint()
    model = _new_VectorSets()
    @test !MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Int},
        MOI.Nonnegatives,
    )
    return
end

function test_delete()
    model = _new_ScalarSets()
    x = MOI.add_variable(model)
    @test_throws MOI.DeleteNotAllowed MOI.delete(model, x)
    @test_throws MOI.DeleteNotAllowed MOI.delete(model, [x])
    return
end

function test_ScalarSets_get_ConstraintFunction()
    model = _new_ScalarSets()
    src = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(
        src,
        """
variables: x, y
minobjective: x + y
c: x + 2.0 * y <= 3.0
""",
    )
    index_map = MOI.copy_to(model, src)
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    c_f = MOI.get(model, MOI.ConstraintFunction(), c)
    @test MOI.Utilities.map_indices(c_f) do x
        return index_map[x]
    end ≈ MOI.get(src, MOI.ConstraintFunction(), index_map[c])
    return
end

function test_VectorSets_get_ConstraintFunction()
    model = _new_VectorSets()
    src = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(src, 2)
    c = MOI.add_constraint(
        src,
        MOI.VectorAffineFunction{Int}(
            MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.(1, x)),
            [1, 3],
        ),
        MOI.SecondOrderCone(2),
    )
    index_map = MOI.copy_to(model, src)
    c_f = MOI.get(model, MOI.ConstraintFunction(), c)
    @test MOI.Utilities.map_indices(c_f) do x
        return index_map[x]
    end ≈ MOI.get(src, MOI.ConstraintFunction(), index_map[c])
    return
end

function test_ScalarSets_get_ConstraintSet()
    model = _new_ScalarSets()
    src = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(
        src,
        """
variables: x, y
minobjective: x + y
c: 2.0 * x + y >= 4.0
""",
    )
    index_map = MOI.copy_to(model, src)
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    c_set = MOI.get(model, MOI.ConstraintSet(), c)
    @test c_set == MOI.get(src, MOI.ConstraintSet(), index_map[c])
    return
end

function test_VectorSets_get_ConstraintSet()
    model = _new_VectorSets()
    src = MOI.Utilities.Model{Int}()
    x = MOI.add_variables(src, 2)
    c = MOI.add_constraint(
        src,
        MOI.VectorAffineFunction{Int}(
            MOI.VectorAffineTerm.(1, MOI.ScalarAffineTerm.(1, x)),
            [1, 3],
        ),
        MOI.SecondOrderCone(2),
    )
    index_map = MOI.copy_to(model, src)
    c_set = MOI.get(model, MOI.ConstraintSet(), c)
    @test c_set == MOI.get(src, MOI.ConstraintSet(), index_map[c])
    return
end

function test_add_after_final_touch()
    model = _new_ScalarSets()
    @test MOI.is_empty(model)
    src = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(
        src,
        """
variables: x, y
minobjective: x + y
x >= 1.0
y == 2.1
c: x + 2.0 * y <= 3.0
""",
    )
    index_map = MOI.copy_to(model, src)
    x = MOI.get(src, MOI.VariableIndex, "x")
    @test_throws(
        MOI.AddConstraintNotAllowed,
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                [MOI.ScalarAffineTerm(1.0, index_map[x])],
                0.0,
            ),
            MOI.LessThan(2.0),
        ),
    )
    return
end

function test_UnsupportedConstraint()
    model = _new_ScalarSets()
    src = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(
        src,
        """
variables: x, y
c: [x, y] in Nonnegatives(2)
""",
    )
    @test_throws MOI.UnsupportedConstraint MOI.copy_to(model, src)
    return
end

function test_ScalarSets()
    optimizer = MOI.Utilities.GenericOptimizer{
        Float64,
        MOI.Utilities.ObjectiveContainer{Float64},
        MOI.Utilities.VariablesContainer{Float64},
        MOI.Utilities.MatrixOfConstraints{
            Float64,
            MOI.Utilities.MutableSparseMatrixCSC{
                Float64,
                Int,
                MOI.Utilities.ZeroBasedIndexing,
            },
            MOI.Utilities.Hyperrectangle{Float64},
            ScalarSets{Float64},
        },
    }()
    MOI.Test.runtests(
        optimizer,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        include = ["test_model_", "test_objective_", "test_variable_"],
        exclude = [
            "test_model_ScalarFunctionConstantNotZero",
            "test_model_default_DualStatus",
            "test_model_default_PrimalStatus",
            "test_model_default_TerminationStatus",
        ],
    )
    return
end

function test_modify()
    model = MOI.Utilities.GenericOptimizer{
        Int,
        MOI.Utilities.ObjectiveContainer{Int},
        MOI.Utilities.VariablesContainer{Int},
        MOI.Utilities.MatrixOfConstraints{
            Int,
            MOI.Utilities.MutableSparseMatrixCSC{
                Int,
                Int,
                MOI.Utilities.OneBasedIndexing,
            },
            MOI.Utilities.Hyperrectangle{Int},
            ScalarSets{Int},
        },
    }()
    x = MOI.add_variable(model)
    func = 2x
    set = MOI.EqualTo(1)
    c = MOI.add_constraint(model, func, set)
    MOI.Utilities.final_touch(model, nothing)
    @test_throws MOI.DeleteNotAllowed(c) MOI.delete(model, c)
    err = MOI.AddConstraintNotAllowed{typeof(func),typeof(set)}(
        MOI.Utilities._MATRIXOFCONSTRAINTS_MODIFY_NOT_ALLOWED_ERROR_MESSAGE,
    )
    @test_throws err MOI.add_constraint(model, func, set)
    return
end

MOI.Utilities.@struct_of_constraints_by_set_types(
    ZerosOrNot,
    MOI.Zeros,
    Union{MOI.Nonnegatives,MOI.Nonpositives},
)

MOI.Utilities.@product_of_sets(Zeros, MOI.Zeros)

function test_multicone()
    T = Int
    Indexing = MOI.Utilities.OneBasedIndexing
    model = MOI.Utilities.GenericOptimizer{
        T,
        MOI.Utilities.ObjectiveContainer{T},
        MOI.Utilities.VariablesContainer{T},
        ZerosOrNot{T}{
            MOI.Utilities.MatrixOfConstraints{
                T,
                MOI.Utilities.MutableSparseMatrixCSC{T,Int,Indexing},
                Vector{T},
                Zeros{T},
            },
            MOI.Utilities.MatrixOfConstraints{
                T,
                MOI.Utilities.MutableSparseMatrixCSC{T,Int,Indexing},
                Vector{T},
                VectorSets{T},
            },
        },
    }()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.Utilities.vectorize([T(5) * x + T(2)]),
        MOI.Zeros(1),
    )
    MOI.add_constraint(
        model,
        MOI.Utilities.vectorize([T(3) * y + T(1)]),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOI.Utilities.vectorize([T(6), T(7) * x, T(4)]),
        MOI.Nonpositives(1),
    )
    MOI.Utilities.final_touch(model, nothing)
    @test convert(
        SparseArrays.SparseMatrixCSC{T,Int},
        model.constraints.moi_zeros.coefficients,
    ) == [T(5) zero(T)]
    @test model.constraints.moi_zeros.constants == T[2]
    @test convert(
        SparseArrays.SparseMatrixCSC{T,Int},
        model.constraints.moi_nonnegatives.coefficients,
    ) == [zero(T) T(3); zero(T) zero(T); T(7) zero(T); zero(T) zero(T)]
    @test model.constraints.moi_nonnegatives.constants == T[1, 6, 0, 4]
    return
end

MOI.Utilities.@product_of_sets(
    MatrixSets,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.PositiveSemidefiniteConeSquare,
    MOI.LogDetConeTriangle,
    MOI.LogDetConeSquare,
    MOI.RootDetConeTriangle,
    MOI.RootDetConeSquare,
)

function test_matrix_sets(::Type{T} = Int) where {T}
    model = MOI.Utilities.GenericOptimizer{
        T,
        MOI.Utilities.ObjectiveContainer{T},
        MOI.Utilities.VariablesContainer{T},
        MOI.Utilities.MatrixOfConstraints{
            T,
            MOI.Utilities.MutableSparseMatrixCSC{
                T,
                T,
                MOI.Utilities.OneBasedIndexing,
            },
            Vector{T},
            MatrixSets{T},
        },
    }()
    function test_set(set)
        MOI.empty!(model)
        func = MOI.Utilities.zero_with_output_dimension(
            MOI.VectorAffineFunction{T},
            MOI.dimension(set),
        )
        ci = MOI.add_constraint(model, func, set)
        MOI.Utilities.final_touch(model, nothing)
        @test MOI.get(model, MOI.ConstraintSet(), ci) == set
    end
    test_set(MOI.PositiveSemidefiniteConeTriangle(2))
    test_set(MOI.PositiveSemidefiniteConeSquare(2))
    test_set(MOI.LogDetConeTriangle(2))
    test_set(MOI.LogDetConeSquare(2))
    test_set(MOI.RootDetConeTriangle(2))
    test_set(MOI.RootDetConeSquare(2))
    return
end

MOI.Utilities.@product_of_sets(
    PowerSets,
    MOI.PowerCone{T},
    MOI.DualPowerCone{T},
)

function test_power_cone_error()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variables(model, 3)
    f = MOI.Utilities.operate(vcat, Float64, 1.0 .* x...)
    s = MOI.PowerCone(0.2)
    MOI.add_constraint(model, f, s)
    cache = MOI.Utilities.GenericOptimizer{
        Float64,
        MOI.Utilities.ObjectiveContainer{Float64},
        MOI.Utilities.VariablesContainer{Float64},
        MOI.Utilities.MatrixOfConstraints{
            Float64,
            MOI.Utilities.MutableSparseMatrixCSC{
                Float64,
                Int,
                MOI.Utilities.OneBasedIndexing,
            },
            Vector{Float64},
            PowerSets{Float64},
        },
    }()
    @test_throws(
        ErrorException(
            "`$(typeof(s))` cannot be used with `Vector` as the set type in " *
            "MatrixOfConstraints",
        ),
        MOI.copy_to(cache, model),
    )
    return
end

function test_dual_power_cone_error()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variables(model, 3)
    f = MOI.Utilities.operate(vcat, Float64, 1.0 .* x...)
    s = MOI.DualPowerCone(0.2)
    MOI.add_constraint(model, f, s)
    cache = MOI.Utilities.GenericOptimizer{
        Float64,
        MOI.Utilities.ObjectiveContainer{Float64},
        MOI.Utilities.VariablesContainer{Float64},
        MOI.Utilities.MatrixOfConstraints{
            Float64,
            MOI.Utilities.MutableSparseMatrixCSC{
                Float64,
                Int,
                MOI.Utilities.OneBasedIndexing,
            },
            Vector{Float64},
            PowerSets{Float64},
        },
    }()
    @test_throws(
        ErrorException(
            "`$(typeof(s))` cannot be used with `Vector` as the set type in " *
            "MatrixOfConstraints",
        ),
        MOI.copy_to(cache, model),
    )
    return
end

struct _SetConstants{T}
    b::Vector{T}
    power_coefficients::Dict{Int,T}
    _SetConstants{T}() where {T} = new{T}(T[], Dict{Int,T}())
end

function Base.empty!(x::_SetConstants)
    empty!(x.b)
    empty!(x.power_coefficients)
    return x
end

Base.resize!(x::_SetConstants, n) = resize!(x.b, n)

function MOI.Utilities.load_constants(x::_SetConstants, offset, f)
    MOI.Utilities.load_constants(x.b, offset, f)
    return
end

function MOI.Utilities.load_constants(
    x::_SetConstants{T},
    offset,
    set::Union{MOI.PowerCone{T},MOI.DualPowerCone{T}},
) where {T}
    x.power_coefficients[offset+1] = set.exponent
    return
end

function MOI.Utilities.function_constants(x::_SetConstants, rows)
    return MOI.Utilities.function_constants(x.b, rows)
end

function MOI.Utilities.set_from_constants(x::_SetConstants, S, rows)
    return MOI.Utilities.set_from_constants(x.b, S, rows)
end

function MOI.Utilities.set_from_constants(
    x::_SetConstants{T},
    ::Type{S},
    rows,
) where {T,S<:Union{MOI.PowerCone{T},MOI.DualPowerCone{T}}}
    @assert length(rows) == 3
    return S(x.power_coefficients[first(rows)])
end

function test_dual_power_cone()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variables(model, 3)
    f = MOI.Utilities.operate(vcat, Float64, 1.0 .* x...)
    p_cone = MOI.PowerCone(0.1)
    p_ref = MOI.add_constraint(model, f, p_cone)
    d_cone = MOI.DualPowerCone(0.2)
    d_ref = MOI.add_constraint(model, f, d_cone)
    cache = MOI.Utilities.GenericOptimizer{
        Float64,
        MOI.Utilities.ObjectiveContainer{Float64},
        MOI.Utilities.VariablesContainer{Float64},
        MOI.Utilities.MatrixOfConstraints{
            Float64,
            MOI.Utilities.MutableSparseMatrixCSC{
                Float64,
                Int,
                MOI.Utilities.OneBasedIndexing,
            },
            _SetConstants{Float64},
            PowerSets{Float64},
        },
    }()
    map = MOI.copy_to(cache, model)
    @test MOI.get(cache, MOI.ConstraintSet(), map[p_ref]) == p_cone
    @test MOI.get(cache, MOI.ConstraintSet(), map[d_ref]) == d_cone
    return
end

end

TestMatrixOfConstraints.runtests()
