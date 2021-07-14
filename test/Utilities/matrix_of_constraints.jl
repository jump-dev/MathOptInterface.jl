module TestMatrixOfConstraints

using Test

import MathOptInterface
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
    ProductOfSetsLP,
    MOI.EqualTo{T},
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.Interval{T},
)

function test_ProductLP()
    optimizer = MOI.Utilities.GenericOptimizer{
        Float64,
        MOI.Utilities.MatrixOfConstraints{
            Float64,
            MOI.Utilities.MutableSparseMatrixCSC{
                Float64,
                Int,
                MOI.Utilities.ZeroBasedIndexing,
            },
            MOI.Utilities.Box{Float64},
            ProductOfSetsLP{Float64},
        },
    }()
    MOI.Test.runtests(
        optimizer,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        exclude = [
            # These tests require `get` before `final_touch` is called.
            "test_basic_",
            "test_linear_integration",
            "test_linear_modify_GreaterThan_and_LessThan_constraints",
            "test_linear_transform",
            "test_quadratic_duplicate_terms",
            "test_quadratic_integration",
            # Can safely ignore this one.
            "test_model_ScalarFunctionConstantNotZero",
            # Unsupported Attributes
            "test_attribute_SolverName",
            "test_model_default_DualStatus",
            "test_model_default_PrimalStatus",
            "test_model_default_TerminationStatus",
        ],
    )
    return
end

MOI.Utilities.@product_of_sets(
    ProductOfSetsConic,
    MOI.Nonnegatives,
    MOI.Nonpositives,
    MOI.SecondOrderCone,
)

function test_ProductOfSetsConic()
    optimizer = MOI.Utilities.GenericOptimizer{
        Float64,
        MOI.Utilities.MatrixOfConstraints{
            Float64,
            MOI.Utilities.MutableSparseMatrixCSC{
                Float64,
                Int,
                MOI.Utilities.OneBasedIndexing,
            },
            MOI.Utilities.Box{Float64},
            ProductOfSetsConic{Float64},
        },
    }()
    MOI.Test.runtests(
        optimizer,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        exclude = [
            # These tests require `get` before `final_touch` is called.
            "test_basic_",
            # Can safely ignore this one.
            "test_model_ScalarFunctionConstantNotZero",
            # Unsupported Attributes
            "test_attribute_SolverName",
            "test_constraint_ConstraintDualStart",
            "test_constraint_ConstraintPrimalStart",
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
        MOI.Utilities.MatrixOfConstraints{
            Int,
            MOI.Utilities.MutableSparseMatrixCSC{
                Int,
                Int,
                MOI.Utilities.OneBasedIndexing,
            },
            MOI.Utilities.Box{Int},
            ProductOfSetsLP{Int},
        },
    }()
    x = MOI.add_variable(model)
    fx = MOI.SingleVariable(x)
    func = 2fx
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

MOIU.@struct_of_constraints_by_set_types(
    ZerosOrNot,
    MOI.Zeros,
    Union{MOI.Nonnegatives,MOI.Nonpositives},
)

function test_multicone()
    T = Int
    Indexing = MOIU.OneBasedIndexing
    model = MOIU.GenericOptimizer{
        T,
        ZerosOrNot{T}{
            MOIU.MatrixOfConstraints{
                T,
                MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
                Vector{T},
                Zeros{T},
            },
            MOIU.MatrixOfConstraints{
                T,
                MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
                Vector{T},
                NonnegNonpos{T},
            },
        },
    }()
    #return model
    x = MOI.add_variable(model)
    fx = MOI.SingleVariable(x)
    y = MOI.add_variable(model)
    fy = MOI.SingleVariable(y)
    MOI.add_constraint(model, MOIU.vectorize([T(5) * fx + T(2)]), MOI.Zeros(1))
    MOI.add_constraint(
        model,
        MOIU.vectorize([T(3) * fy + T(1)]),
        MOI.Nonnegatives(1),
    )
    MOI.add_constraint(
        model,
        MOIU.vectorize([T(6), T(7) * fx, T(4)]),
        MOI.Nonpositives(1),
    )
    MOIU.final_touch(model, nothing)
    _test_matrix_equal(
        model.constraints.moi_zeros.coefficients,
        sparse([1], [1], T[5], 1, 2),
    )
    @test model.constraints.moi_zeros.constants == T[2]
    _test_matrix_equal(
        model.constraints.moi_nonnegatives.coefficients,
        sparse([1, 3], [2, 1], T[3, 7], 4, 2),
    )
    @test model.constraints.moi_nonnegatives.constants == T[1, 6, 0, 4]
end

end

TestMatrixOfConstraints.runtests()
