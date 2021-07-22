module TestMatrixOfConstraints

using SparseArrays, Test

import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.DeprecatedTest
const MOIU = MOI.Utilities

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

function _test_matrix_equal(A::SparseMatrixCSC, B::SparseMatrixCSC)
    @test A.m == B.m
    @test A.n == B.n
    @test A.nzval == B.nzval
    @test A.rowval == B.rowval
    @test A.colptr == B.colptr
end
function _test_matrix_equal(
    A::MOIU.MutableSparseMatrixCSC{Tv,Ti,I},
    B::SparseMatrixCSC,
) where {Tv,Ti,I}
    @test A.m == B.m
    @test A.n == B.n
    @test A.nzval == B.nzval
    if I <: MOIU.OneBasedIndexing
        @test A.rowval == B.rowval
        @test A.colptr == B.colptr
    else
        @test A.rowval == B.rowval .- 1
        @test A.colptr == B.colptr .- 1
    end
    sA = convert(typeof(B), A)
    @test typeof(sA) == typeof(B)
    return _test_matrix_equal(sA, B)
end

function _test(
    query_test,
    test,
    ConstantsType::Type,
    ProductOfSetsType::Type,
    A,
    b,
    bridged::Bool,
    Indexing,
)
    optimizer =
        matrix_instance(Float64, ConstantsType, ProductOfSetsType, Indexing)
    _inner(model::MOI.Bridges.LazyBridgeOptimizer) = _inner(model.model)
    _inner(model::MOI.Utilities.CachingOptimizer) = _inner(model.optimizer)
    _inner(model::MOI.Utilities.MockOptimizer) = _inner(model.inner_model)
    _inner(model::MOI.Utilities.UniversalFallback) = _inner(model.model)
    _inner(model::MOI.Utilities.AbstractModel) = model
    _A(model::MOIU.AbstractModel) = model.constraints.coefficients
    _b(model::MOIU.AbstractModel) = model.constraints.constants
    if bridged
        optimizer = MOI.Bridges.full_bridge_optimizer(optimizer, Float64)
    end
    config = MOIT.Config(solve = false, query_number_of_constraints = false)
    test(optimizer, config)
    MOI.Utilities.final_touch(optimizer, MOI.Utilities.IdentityMap())
    _test_matrix_equal(_A(_inner(optimizer)), A)
    @test _b(_inner(optimizer)) == b
    query_test(_inner(optimizer))

    # Use mock to have xor indices to check that they are mapped
    cache = MOIU.MockOptimizer(MOIU.Model{Float64}())

    MOI.empty!(cache)
    MOI.empty!(optimizer)
    model = MOIU.CachingOptimizer(cache, optimizer)
    model.mode = MOIU.MANUAL
    MOIU.reset_optimizer(model)
    @test MOIU.state(model) == MOIU.EMPTY_OPTIMIZER
    test(model, config)
    MOIU.attach_optimizer(model)
    _test_matrix_equal(_A(_inner(model)), A)
    @test _b(_inner(model)) == b
    query_test(_inner(optimizer))

    MOI.empty!(cache)
    MOI.empty!(optimizer)
    model = MOIU.CachingOptimizer(
        cache,
        MOIU.MockOptimizer(MOIU.UniversalFallback(optimizer)),
    )
    model.mode = MOIU.MANUAL
    MOIU.reset_optimizer(model)
    @test MOIU.state(model) == MOIU.EMPTY_OPTIMIZER
    test(model, config)
    MOIU.attach_optimizer(model)
    _test_matrix_equal(_A(_inner(model)), A)
    @test _b(_inner(model)) == b
    query_test(_inner(optimizer))

    if !bridged
        MOI.empty!(cache)
        MOI.empty!(optimizer)
        model = MOIU.CachingOptimizer(
            cache,
            MOI.Bridges.full_bridge_optimizer(
                MOIU.CachingOptimizer(optimizer, MOIU.MANUAL),
                Float64,
            ),
        )
        model.mode = MOIU.MANUAL
        MOIU.reset_optimizer(model)
        @test MOIU.state(model) == MOIU.EMPTY_OPTIMIZER
        test(model, config)
        MOIU.attach_optimizer(model)
        inner = model.optimizer.model.model_cache
        _test_matrix_equal(_A(inner), A)
        @test _b(inner) == b
        query_test(inner)

        MOI.empty!(cache)
        MOI.empty!(optimizer)
        model = MOIU.CachingOptimizer(
            cache,
            MOI.Bridges.full_bridge_optimizer(optimizer, Float64),
        )
        model.mode = MOIU.MANUAL
        MOIU.reset_optimizer(model)
        @test MOIU.state(model) == MOIU.EMPTY_OPTIMIZER
        test(model, config)
        MOIU.attach_optimizer(model)
        _test_matrix_equal(_A(_inner(model)), A)
        @test _b(_inner(model)) == b
        query_test(_inner(optimizer))
    end
    return
end

function _lp(model, ::MOI.DeprecatedTest.Config{T}) where {T}
    MOI.empty!(model)
    x = MOI.add_variable(model)
    fx = one(T) * MOI.SingleVariable(x)
    y = MOI.add_variable(model)
    fy = one(T) * MOI.SingleVariable(y)
    MOI.add_constraint(model, 3fx + 2fy, MOI.EqualTo(T(5)))
    MOI.add_constraint(model, fx, MOI.GreaterThan(zero(T)))
    MOI.add_constraint(model, -fy, MOI.LessThan(zero(T)))
    return MOI.add_constraint(model, 5fx - 4fy, MOI.Interval(T(6), T(7)))
end

function matrix_instance(
    T::Type,
    ConstantsType,
    ProductOfSetsType::Type,
    Indexing,
)
    return MOIU.GenericOptimizer{
        T,
        MOIU.MatrixOfConstraints{
            T,
            MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
            ConstantsType,
            ProductOfSetsType,
        },
    }()
end

MOIU.@mix_of_scalar_sets(
    MixLP,
    MOI.EqualTo{T},
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.Interval{T},
)

MOIU.@product_of_sets(
    OrdLP,
    MOI.EqualTo{T},
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.Interval{T},
)

function test_contlinear()
    test_contlinear(MOIU.OneBasedIndexing)
    return test_contlinear(MOIU.ZeroBasedIndexing)
end
function test_contlinear(Indexing)
    A2 = sparse([1, 1], [1, 2], ones(2))
    b2 = MOI.Utilities.Hyperrectangle([-Inf], [1.0])
    Alp = sparse(
        [1, 1, 2, 3, 4, 4],
        [1, 2, 1, 2, 1, 2],
        Float64[3, 2, 1, -1, 5, -4],
    )
    blp = MOI.Utilities.Hyperrectangle([5, 0, -Inf, 6], [5, Inf, 0, 7])
    F = MOI.ScalarAffineFunction{Float64}
    @testset "$SetType" for SetType in [MixLP{Float64}, OrdLP{Float64}]
        _test(
            MOIT.linear2test,
            MOI.Utilities.Hyperrectangle{Float64},
            SetType,
            A2,
            b2,
            false,
            Indexing,
        ) do optimizer
            S = MOI.LessThan{Float64}
            @test [(F, S), (MOI.SingleVariable, MOI.GreaterThan{Float64})] ==
                  MOI.get(optimizer, MOI.ListOfConstraintTypesPresent())
            @test 1 == MOI.get(optimizer, MOI.NumberOfConstraints{F,S}())
            cis = MOI.get(optimizer, MOI.ListOfConstraintIndices{F,S}())
            @test 1 == length(collect(cis))
            for ci in cis
                @test MOI.is_valid(optimizer, ci)
                @test 1 == MOI.Utilities.rows(optimizer.constraints, ci)
            end
        end
        _test(
            _lp,
            MOI.Utilities.Hyperrectangle{Float64},
            SetType,
            Alp,
            blp,
            false,
            Indexing,
        ) do optimizer
            vis = MOI.get(optimizer, MOI.ListOfVariableIndices())
            @test_throws MOI.DeleteNotAllowed(first(vis)) MOI.delete(
                optimizer,
                vis,
            )
            for vi in vis
                @test_throws MOI.DeleteNotAllowed(vi) MOI.delete(optimizer, vi)
            end
            con_types = [
                (F, MOI.EqualTo{Float64}),
                (F, MOI.GreaterThan{Float64}),
                (F, MOI.LessThan{Float64}),
                (F, MOI.Interval{Float64}),
            ]
            @test con_types ==
                  MOI.get(optimizer, MOI.ListOfConstraintTypesPresent())
            for i in eachindex(con_types)
                F, S = con_types[i]
                @test 1 == MOI.get(optimizer, MOI.NumberOfConstraints{F,S}())
                cis = MOI.get(optimizer, MOI.ListOfConstraintIndices{F,S}())
                @test 1 == length(collect(cis))
                for ci in cis
                    @test MOI.is_valid(optimizer, ci)
                    @test i == MOI.Utilities.rows(optimizer.constraints, ci)
                    @test_throws MOI.DeleteNotAllowed(ci) MOI.delete(
                        optimizer,
                        ci,
                    )
                end
            end
        end
    end
end

MOIU.@product_of_sets(Nonneg, MOI.Nonnegatives)
MOIU.@product_of_sets(NonposNonneg, MOI.Nonpositives, MOI.Nonnegatives)
MOIU.@product_of_sets(NonnegNonpos, MOI.Nonnegatives, MOI.Nonpositives)

function test_contconic()
    test_contconic(MOIU.OneBasedIndexing)
    return test_contconic(MOIU.ZeroBasedIndexing)
end

function test_contconic(Indexing)
    function _lin3_query(optimizer, con_types)
        @test con_types ==
              MOI.get(optimizer, MOI.ListOfConstraintTypesPresent())
        k = 0
        for (F, S) in con_types
            function bad_types(F, S)
                @test 0 == @inferred MOI.get(
                    optimizer,
                    MOI.NumberOfConstraints{F,S}(),
                )
                @test isempty(
                    @inferred MOI.get(
                        optimizer,
                        MOI.ListOfConstraintIndices{F,S}(),
                    )
                )
                @test !MOI.is_valid(optimizer, MOI.ConstraintIndex{F,S}(1))
            end
            BadF = MOI.ScalarAffineFunction{Int}
            BadS = MOI.EqualTo{Int}
            bad_types(F, BadS)
            bad_types(BadF, S)
            bad_types(BadF, BadS)
            n = div(2, length(con_types))
            @test n ==
                  @inferred MOI.get(optimizer, MOI.NumberOfConstraints{F,S}())
            cis = MOI.get(optimizer, MOI.ListOfConstraintIndices{F,S}())
            @test n == length(collect(cis))
            for ci in cis
                @test MOI.is_valid(optimizer, ci)
                @test !MOI.is_valid(optimizer, typeof(ci)(-1))
                k += 1
                @test k:k == MOI.Utilities.rows(optimizer.constraints, ci)
            end
        end
    end
    # We test here that the constraints are reordered by defining the sets
    # in two different orders and check that it affects `b`.
    A = sparse([1, 2], [1, 1], ones(2))
    b = [-1.0, 1.0]
    F = MOI.VectorAffineFunction{Float64}
    _test(
        MOIT.lin3test,
        Vector{Float64},
        NonnegNonpos{Float64},
        A,
        b,
        false,
        Indexing,
    ) do optimizer
        return _lin3_query(
            optimizer,
            [(F, MOI.Nonnegatives), (F, MOI.Nonpositives)],
        )
    end
    b = [1.0, -1.0]
    _test(
        MOIT.lin3test,
        Vector{Float64},
        NonposNonneg{Float64},
        A,
        b,
        false,
        Indexing,
    ) do optimizer
        return _lin3_query(
            optimizer,
            [(F, MOI.Nonpositives), (F, MOI.Nonnegatives)],
        )
    end
    # Here, we test that it works of some constraints are bridged but not all.
    A = sparse([1, 2], [1, 1], [1.0, -1.0])
    b = -ones(2)
    _test(
        MOIT.lin3test,
        Vector{Float64},
        Nonneg{Float64},
        A,
        b,
        true,
        Indexing,
    ) do optimizer
        return _lin3_query(optimizer, [(F, MOI.Nonnegatives)])
    end
end

function test_get_by_name(T::Type, SetsType::Type)
    model = matrix_instance(
        T,
        MOI.Utilities.Hyperrectangle{T},
        SetsType,
        MOI.Utilities.OneBasedIndexing,
    )
    MOI.empty!(model)
    x = MOI.add_variable(model)
    fx = MOI.SingleVariable(x)
    c = MOI.add_constraint(model, one(T) * fx, MOI.EqualTo(one(T)))
    MOI.set(model, MOI.ConstraintName(), c, "c")
    @test "c" == MOI.get(model, MOI.ConstraintName(), c)
    @test c == MOI.get(model, MOI.ConstraintIndex, "c")
    @test c == MOI.get(model, typeof(c), "c")
end
function test_get_by_name()
    for T in [Int, Float64]
        for SetsType in [MixLP{T}, OrdLP{T}]
            test_get_by_name(T, SetsType)
        end
    end
end

MOIU.@struct_of_constraints_by_function_types(
    VoVorSAff,
    MOI.VectorOfVariables,
    MOI.ScalarAffineFunction{T},
)

function test_nametest()
    T = Float64
    Indexing = MOIU.OneBasedIndexing
    ConstantsType = MOIU.Hyperrectangle{T}
    for ProductOfSetsType in [MixLP{Float64}, OrdLP{Float64}]
        model = MOIU.GenericOptimizer{
            T,
            VoVorSAff{T}{
                MOIU.VectorOfConstraints{
                    MOI.VectorOfVariables,
                    MOI.Nonpositives,
                },
                MOIU.MatrixOfConstraints{
                    T,
                    MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
                    ConstantsType,
                    ProductOfSetsType,
                },
            },
        }()
        MOI.DeprecatedTest.nametest(model, delete = false)
    end
end

MOIU.@struct_of_constraints_by_function_types(
    VoVorVAff,
    MOI.VectorOfVariables,
    MOI.VectorAffineFunction{T},
)

MOIU.@product_of_sets(Zeros, MOI.Zeros)

function test_empty()
    T = Float64
    Indexing = MOIU.OneBasedIndexing
    model = MOIU.GenericOptimizer{
        T,
        VoVorVAff{T}{
            MOIU.VectorOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives},
            MOIU.MatrixOfConstraints{
                T,
                MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
                Vector{T},
                Zeros{Float64},
            },
        },
    }()
    return MOI.DeprecatedTest.emptytest(model)
end

function test_valid()
    T = Float64
    Indexing = MOIU.OneBasedIndexing
    ConstantsType = MOIU.Hyperrectangle{T}
    for ProductOfSetsType in [MixLP{Float64}, OrdLP{Float64}]
        model = matrix_instance(T, ConstantsType, ProductOfSetsType, Indexing)
        MOI.DeprecatedTest.validtest(model, delete = false)
    end
end

function test_supports_constraint(T::Type = Float64, BadT::Type = Float32)
    Indexing = MOIU.OneBasedIndexing
    ConstantsType = MOIU.Hyperrectangle{T}
    for ProductOfSetsType in [MixLP{Float64}, OrdLP{Float64}]
        model = MOIU.GenericOptimizer{
            T,
            VoVorSAff{T}{
                MOIU.VectorOfConstraints{MOI.VectorOfVariables,MOI.Zeros},
                MOIU.MatrixOfConstraints{
                    T,
                    MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
                    ConstantsType,
                    ProductOfSetsType,
                },
            },
        }()
        MOI.DeprecatedTest.supports_constrainttest(model, T, BadT)
    end
end

MOIU.@struct_of_constraints_by_function_types(
    VoVorSAfforVAff,
    MOI.VectorOfVariables,
    MOI.ScalarAffineFunction{T},
    MOI.VectorAffineFunction{T},
)

function test_copy(Indexing)
    T = Float64
    for ScalarSetsType in [MixLP{T}, OrdLP{T}]
        model = MOIU.GenericOptimizer{
            T,
            VoVorSAfforVAff{T}{
                MOIU.VectorOfConstraints{
                    MOI.VectorOfVariables,
                    MOI.Nonnegatives,
                },
                MOIU.MatrixOfConstraints{
                    T,
                    MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
                    MOIU.Hyperrectangle{T},
                    ScalarSetsType,
                },
                MOIU.MatrixOfConstraints{
                    T,
                    MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
                    Vector{T},
                    Zeros{T},
                },
            },
        }()
        MOI.DeprecatedTest.copytest(model, MOIU.Model{T}())
    end
end

function test_copy()
    test_copy(MOIU.ZeroBasedIndexing)
    return test_copy(MOIU.OneBasedIndexing)
end

function test_modif()
    model = matrix_instance(
        Int,
        MOIU.Hyperrectangle{Int},
        OrdLP{Int},
        MOIU.OneBasedIndexing,
    )
    x = MOI.add_variable(model)
    fx = MOI.SingleVariable(x)
    func = 2fx
    set = MOI.EqualTo(1)
    c = MOI.add_constraint(model, func, set)
    MOIU.final_touch(model, nothing)
    @test_throws MOI.DeleteNotAllowed(c) MOI.delete(model, c)
    err = MOI.AddConstraintNotAllowed{typeof(func),typeof(set)}(
        MOIU._MATRIXOFCONSTRAINTS_MODIFY_NOT_ALLOWED_ERROR_MESSAGE,
    )
    @test_throws err MOI.add_constraint(model, func, set)
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
