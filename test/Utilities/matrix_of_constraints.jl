using SparseArrays, Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

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
    optimizer = MOIU.GenericOptimizer{
        Float64,
        MOIU.MatrixOfConstraints{
            Float64,
            MOIU.MutableSparseMatrixCSC{Float64,Int,Indexing},
            ConstantsType,
            ProductOfSetsType,
        },
    }()
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
    config = MOIT.TestConfig(solve = false, query_number_of_constraints = false)
    test(optimizer, config)
    MOI.Utilities.final_touch(optimizer, MOI.Utilities.IdentityMap())
    _test_matrix_equal(_A(_inner(optimizer)), A)
    @test _b(_inner(optimizer)) == b
    query_test(_inner(optimizer))

    MOI.empty!(optimizer)
    model = MOIU.CachingOptimizer(MOIU.Model{Float64}(), optimizer)
    model.mode = MOIU.MANUAL
    MOIU.reset_optimizer(model)
    @test MOIU.state(model) == MOIU.EMPTY_OPTIMIZER
    test(model, config)
    MOIU.attach_optimizer(model)
    _test_matrix_equal(_A(_inner(model)), A)
    @test _b(_inner(model)) == b
    query_test(_inner(optimizer))

    MOI.empty!(optimizer)
    model = MOIU.CachingOptimizer(
        MOIU.Model{Float64}(),
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
        MOI.empty!(optimizer)
        model = MOIU.CachingOptimizer(
            MOIU.Model{Float64}(),
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

        MOI.empty!(optimizer)
        model = MOIU.CachingOptimizer(
            MOIU.Model{Float64}(),
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

function _lp(model, ::MOI.Test.TestConfig{T}) where {T}
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

function matrix_lp(T::Type, ProductOfSetsType::Type)
    optimizer = MOIU.GenericOptimizer{
        T,
        MOIU.MatrixOfConstraints{
            T,
            MOIU.MutableSparseMatrixCSC{T,Int,Indexing},
            MOI.Utilities.Box{T},
            ProductOfSetsType,
        },
    }()
    return MOI.Utilities.final_touch(optimizer)
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

@testset "contlinear $Indexing" for Indexing in [
    MOIU.OneBasedIndexing,
    MOIU.ZeroBasedIndexing,
]
    A2 = sparse([1, 1], [1, 2], ones(2))
    b2 = MOI.Utilities.Box([-Inf], [1.0])
    Alp = sparse(
        [1, 1, 2, 3, 4, 4],
        [1, 2, 1, 2, 1, 2],
        Float64[3, 2, 1, -1, 5, -4],
    )
    blp = MOI.Utilities.Box([5, 0, -Inf, 6], [5, Inf, 0, 7])
    F = MOI.ScalarAffineFunction{Float64}
    @testset "$SetType" for SetType in [MixLP{Float64}, OrdLP{Float64}]
        _test(
            MOIT.linear2test,
            MOI.Utilities.Box{Float64},
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
            MOI.Utilities.Box{Float64},
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

@testset "contconic $Indexing" for Indexing in [
    MOIU.OneBasedIndexing,
    MOIU.ZeroBasedIndexing,
]
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
