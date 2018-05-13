"""
    test_basic_constraint_functionality(f::Function, model::MOI.ModelLike, config::TestConfig, set::MOI.AbstractSet, N::Int;
        delete::Bool                  = true,
        get_constraint_function::Bool = true,
        get_constraint_set::Bool      = true
    )

Test some basic constraint tests.

See also `basic_constraint_tests`.

`f` is a function that takes a vector of `N` variables and returns a constraint
function.

If `delete = true`, it will test the deletion of constraints.
If `get_constraint_function = true`, it will test the getting of `ConstraintFunction`.
If `get_constraint_set = true`, it will test the getting of `ConstraintSet`.

### Example

    test_basic_constraint_functionality(model, config, MOI.LessThan(1.0), 1; delete=false) do x
        MOI.ScalarAffineFunction(model, [x], [1.0], 0.0)
    end
"""
function test_basic_constraint_functionality(f::Function, model::MOI.ModelLike, config::TestConfig, set::MOI.AbstractSet, N::Int=1;
    delete::Bool                  = true,
    get_constraint_function::Bool = true,
    get_constraint_set::Bool      = true
    )
    MOI.empty!(model)
    x = MOI.addvariables!(model, N)
    constraint_function = f(x)
    F, S = typeof(constraint_function), typeof(set)

    @test MOI.supportsconstraint(model, F, S)
    @test MOI.canaddconstraint(model, F, S)

    @testset "NumberOfConstraints" begin
        @test MOI.canget(model, MOI.NumberOfConstraints{F,S}())
    end

    @testset "addconstraint!" begin
        n = MOI.get(model, MOI.NumberOfConstraints{F,S}())
        c = MOI.addconstraint!(model, constraint_function, set)
        @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == n + 1

        @testset "ConstraintName" begin
            @test MOI.canget(model, MOI.ConstraintName(), typeof(c))
            @test MOI.get(model, MOI.ConstraintName(), c) == ""
            @test MOI.canset(model, MOI.ConstraintName(), typeof(c))
            MOI.set!(model, MOI.ConstraintName(), c, "c")
            @test MOI.get(model, MOI.ConstraintName(), c) == "c"
        end

        if get_constraint_function
            @testset "ConstraintFunction" begin
                @test MOI.canget(model, MOI.ConstraintFunction(), typeof(c))
                @test MOI.get(model, MOI.ConstraintFunction(), c) ≈ constraint_function
            end
        end
        if get_constraint_set
            @testset "ConstraintSet" begin
                @test MOI.canget(model, MOI.ConstraintSet(), typeof(c))
                @test MOI.get(model, MOI.ConstraintSet(), c) == set
            end
        end
    end

    @testset "addconstraints!" begin
        n = MOI.get(model, MOI.NumberOfConstraints{F,S}())
        cc = MOI.addconstraints!(model,
            [constraint_function, constraint_function],
            [set, set]
        )
        @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == n + 2
    end

    @testset "ListOfConstraintIndices" begin
        @test MOI.canget(model, MOI.ListOfConstraintIndices{F,S}())
        c_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        @test length(c_indices) == MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 3
    end

    @testset "isvalid" begin
        c_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        @test length(c_indices) == 3  # check that we've added a constraint
        @test all(MOI.isvalid.(model, c_indices))
    end

    if delete
        @testset "delete!" begin
            c_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            @test length(c_indices) == 3  # check that we've added a constraint
            @test MOI.candelete(model, c_indices[1])

            @test length(c_indices) == 3  # check that we've added a constraint
            MOI.delete!(model, c_indices[1])
            @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == length(c_indices)-1 == 2
            @test !MOI.isvalid(model, c_indices[1])
        end
    end
end

# x
const dummy_single_variable   = (x::Vector{MOI.VariableIndex}) -> MOI.SingleVariable(x[1])
# x₁, x₂
const dummy_vectorofvariables = (x::Vector{MOI.VariableIndex}) -> MOI.VectorOfVariables(x)
# 1.0 * x
const dummy_scalar_affine     = (x::Vector{MOI.VariableIndex}) -> MOI.ScalarAffineFunction(x, [1.0], 0.0)
# 1.0 * x + 1.0 * x^2
const dummy_scalar_quadratic  = (x::Vector{MOI.VariableIndex}) -> MOI.ScalarQuadraticFunction(x, [1.0], x, x, [1.0], 0.0)
# x₁ +    - 1
#    + x₂ + 1
const dummy_vector_affine     = (x::Vector{MOI.VariableIndex}) -> MOI.VectorAffineFunction([1, 2], x, [1.0, 1.0], [-1.0, 1.0])
# x₁ +    + x₁^2
#    + x₂ +      + x₂^2
const dummy_vector_quadratic  = (x::Vector{MOI.VariableIndex}) -> MOI.VectorQuadraticFunction(
    [1,2], x, [1.0, 1.0],       # affine component
    [1,2], x, x, [1.0, 1.0],    # quadratic component
    [0.0, 0.0]                  # constant term
)

const BasicConstraintTests = Dict(
    (MOI.SingleVariable, MOI.LessThan{Float64})    => ( dummy_single_variable, 1, MOI.LessThan(1.0) ),
    (MOI.SingleVariable, MOI.GreaterThan{Float64}) => ( dummy_single_variable, 1, MOI.GreaterThan(1.0) ),
    (MOI.SingleVariable, MOI.EqualTo{Float64})     => ( dummy_single_variable, 1, MOI.EqualTo(1.0) ),
    (MOI.SingleVariable, MOI.Interval{Float64})    => ( dummy_single_variable, 1, MOI.Interval(1.0, 2.0) ),

    (MOI.SingleVariable, MOI.ZeroOne)                 => ( dummy_single_variable, 1, MOI.ZeroOne() ),
    (MOI.SingleVariable, MOI.Integer)                 => ( dummy_single_variable, 1, MOI.Integer() ),
    (MOI.SingleVariable, MOI.Semicontinuous{Float64}) => ( dummy_single_variable, 1, MOI.Semicontinuous(1.0, 2.0) ),
    (MOI.SingleVariable, MOI.Semiinteger{Float64})    => ( dummy_single_variable, 1, MOI.Semiinteger(1.0, 2.0) ),

    (MOI.VectorOfVariables, MOI.SOS1{Float64}) => ( dummy_vectorofvariables, 2, MOI.SOS1([1.0, 2.0]) ),
    (MOI.VectorOfVariables, MOI.SOS2{Float64}) => ( dummy_vectorofvariables, 2, MOI.SOS2([1.0, 2.0]) ),
    (MOI.VectorOfVariables, MOI.Reals)         => ( dummy_vectorofvariables, 2, MOI.Reals(2) ),
    (MOI.VectorOfVariables, MOI.Zeros)         => ( dummy_vectorofvariables, 2, MOI.Zeros(2) ),
    (MOI.VectorOfVariables, MOI.Nonpositives)  => ( dummy_vectorofvariables, 2, MOI.Nonpositives(2) ),
    (MOI.VectorOfVariables, MOI.Nonnegatives)  => ( dummy_vectorofvariables, 2, MOI.Nonnegatives(2) ),

    (MOI.VectorOfVariables, MOI.SecondOrderCone)        => ( dummy_vectorofvariables, 3, MOI.SecondOrderCone(3) ),
    (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) => ( dummy_vectorofvariables, 3, MOI.RotatedSecondOrderCone(3) ),
    (MOI.VectorOfVariables, MOI.GeometricMeanCone)      => ( dummy_vectorofvariables, 3, MOI.GeometricMeanCone(3) ),
    (MOI.VectorOfVariables, MOI.ExponentialCone)        => ( dummy_vectorofvariables, 3, MOI.ExponentialCone() ),
    (MOI.VectorOfVariables, MOI.DualExponentialCone)    => ( dummy_vectorofvariables, 3, MOI.DualExponentialCone() ),

    (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => ( dummy_scalar_affine, 1, MOI.LessThan(1.0) ),
    (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => ( dummy_scalar_affine, 1, MOI.GreaterThan(1.0) ),
    (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})     => ( dummy_scalar_affine, 1, MOI.EqualTo(1.0) ),
    (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})    => ( dummy_scalar_affine, 1, MOI.Interval(1.0, 2.0) ),

    (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64})    => ( dummy_scalar_quadratic, 1, MOI.LessThan(1.0) ),
    (MOI.ScalarQuadraticFunction{Float64}, MOI.GreaterThan{Float64}) => ( dummy_scalar_quadratic, 1, MOI.GreaterThan(1.0) ),
    (MOI.ScalarQuadraticFunction{Float64}, MOI.EqualTo{Float64})     => ( dummy_scalar_quadratic, 1, MOI.EqualTo(1.0) ),
    (MOI.ScalarQuadraticFunction{Float64}, MOI.Interval{Float64})    => ( dummy_scalar_quadratic, 1, MOI.Interval(1.0, 2.0) ),

    (MOI.VectorAffineFunction{Float64}, MOI.Reals)        => ( dummy_vector_affine, 2, MOI.Reals(2) ),
    (MOI.VectorAffineFunction{Float64}, MOI.Zeros)        => ( dummy_vector_affine, 2, MOI.Zeros(2) ),
    (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => ( dummy_vector_affine, 2, MOI.Nonpositives(2) ),
    (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => ( dummy_vector_affine, 2, MOI.Nonnegatives(2) ),

    (MOI.VectorQuadraticFunction{Float64}, MOI.Reals)        => ( dummy_vector_quadratic, 2, MOI.Reals(2) ),
    (MOI.VectorQuadraticFunction{Float64}, MOI.Zeros)        => ( dummy_vector_quadratic, 2, MOI.Zeros(2) ),
    (MOI.VectorQuadraticFunction{Float64}, MOI.Nonpositives) => ( dummy_vector_quadratic, 2, MOI.Nonpositives(2) ),
    (MOI.VectorQuadraticFunction{Float64}, MOI.Nonnegatives) => ( dummy_vector_quadratic, 2, MOI.Nonnegatives(2) )
)
"""
    basic_constraint_tests(model::MOI.ModelLike, config::TestConfig;
        delete                  = true,
        get_constraint_function = true,
        get_constraint_set      = true,
        include                 = Any[],
        exclude                 = Any[]
        )

Test basic constraint functionality for all function-in-set combinations that
are supported by `model`.

See also `test_basic_constraint_functionality`.

If `delete = true`, it will test the deletion of constraints.
If `get_constraint_function = true`, it will test the getting of `ConstraintFunction`.
If `get_constraint_set = true`, it will test the getting of `ConstraintSet`.

`include` and `exclude` can be used to run a subset of the tests, although only
one can be used in each call.

### Examples

    basic_constraint_tests(model, config; exclude = [
        (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
    ])

    basic_constraint_tests(model, config;
        delete = false,
        include = [
            (MOI.VectorOfVariables, MOI.SOS2{Float64})
        ]
    )
"""
function basic_constraint_tests(model::MOI.ModelLike, config::TestConfig;
    delete                  = true,
    get_constraint_function = true,
    get_constraint_set      = true,
    include                 = Any[],
    exclude                 = Any[]
    )
    if length(include) > 0 && length(exclude) > 0
        error("Don't use `include` and `exclude` together.")
    end
    test_keys = length(include) > 0 ? include : Iterators.filter(x->!(x in exclude), keys(BasicConstraintTests))
    for (F,S) in test_keys
        if MOI.supportsconstraint(model, F, S)
            @testset "$(F)-$(S)" begin
                (cf, N, set) = BasicConstraintTests[(F,S)]
                test_basic_constraint_functionality(cf, model, config, set, N;
                    delete                  = delete,
                    get_constraint_function = get_constraint_function,
                    get_constraint_set      = get_constraint_set
                )
            end
        end
    end
end
