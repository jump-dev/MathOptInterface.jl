function _function(
    ::Any,
    ::Type{MOI.VariableIndex},
    x::Vector{MOI.VariableIndex},
)
    return x[1]
end

function _function(
    ::Any,
    ::Type{MOI.VectorOfVariables},
    x::Vector{MOI.VariableIndex},
)
    return MOI.VectorOfVariables(x)
end

function _function(
    ::Type{T},
    ::Type{MOI.ScalarAffineFunction},
    x::Vector{MOI.VariableIndex},
) where {T}
    return MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), x), zero(T))
end

function _function(
    ::Type{T},
    ::Type{MOI.ScalarQuadraticFunction},
    x::Vector{MOI.VariableIndex},
) where {T}
    return MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm.(one(T), x, x),
        MOI.ScalarAffineTerm.(one(T), x),
        zero(T),
    )
end

function _function(
    ::Type{T},
    ::Type{MOI.VectorAffineFunction},
    x::Vector{MOI.VariableIndex},
) where {T}
    return MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:length(x), MOI.ScalarAffineTerm.(one(T), x)),
        zeros(T, length(x)),
    )
end

function _function(
    ::Type{T},
    ::Type{MOI.VectorQuadraticFunction},
    x::Vector{MOI.VariableIndex},
) where {T}
    return MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            1:length(x),
            MOI.ScalarQuadraticTerm.(one(T), x, x),
        ),
        MOI.VectorAffineTerm.(1:length(x), MOI.ScalarAffineTerm.(one(T), x)),
        zeros(T, length(x)),
    )
end

# Default fallback.
_set(::Any, ::Type{S}) where {S} = _set(S)

_set(::Type{T}, ::Type{MOI.LessThan}) where {T} = MOI.LessThan(one(T))
_set(::Type{T}, ::Type{MOI.GreaterThan}) where {T} = MOI.GreaterThan(one(T))
_set(::Type{T}, ::Type{MOI.EqualTo}) where {T} = MOI.EqualTo(one(T))
_set(::Type{T}, ::Type{MOI.Interval}) where {T} = MOI.Interval(zero(T), one(T))
_set(::Type{MOI.ZeroOne}) = MOI.ZeroOne()
_set(::Type{MOI.Integer}) = MOI.Integer()
function _set(::Type{T}, ::Type{MOI.Semicontinuous}) where {T}
    return MOI.Semicontinuous(zero(T), one(T))
end
function _set(::Type{T}, ::Type{MOI.Semiinteger}) where {T}
    return MOI.Semiinteger(zero(T), one(T))
end
_set(::Type{T}, ::Type{MOI.SOS1}) where {T} = MOI.SOS1(convert.(T, 1:2))
_set(::Type{T}, ::Type{MOI.SOS2}) where {T} = MOI.SOS2(convert.(T, 1:2))
_set(::Type{MOI.Zeros}) = MOI.Zeros(2)
_set(::Type{MOI.Nonpositives}) = MOI.Nonpositives(2)
_set(::Type{MOI.Nonnegatives}) = MOI.Nonnegatives(2)
_set(::Type{MOI.NormInfinityCone}) = MOI.NormInfinityCone(3)
_set(::Type{MOI.NormOneCone}) = MOI.NormOneCone(3)
_set(::Type{MOI.SecondOrderCone}) = MOI.SecondOrderCone(3)
_set(::Type{MOI.RotatedSecondOrderCone}) = MOI.RotatedSecondOrderCone(3)
_set(::Type{MOI.GeometricMeanCone}) = MOI.GeometricMeanCone(3)
_set(::Type{MOI.ExponentialCone}) = MOI.ExponentialCone()
_set(::Type{MOI.DualExponentialCone}) = MOI.DualExponentialCone()
_set(::Type{MOI.PowerCone}) = MOI.PowerCone(0.5)
_set(::Type{MOI.DualPowerCone}) = MOI.DualPowerCone(0.5)
_set(::Type{MOI.RelativeEntropyCone}) = MOI.RelativeEntropyCone(3)
_set(::Type{MOI.NormSpectralCone}) = MOI.NormSpectralCone(2, 3)
_set(::Type{MOI.NormNuclearCone}) = MOI.NormNuclearCone(2, 3)
function _set(::Type{MOI.PositiveSemidefiniteConeTriangle})
    return MOI.PositiveSemidefiniteConeTriangle(3)
end
function _set(::Type{MOI.PositiveSemidefiniteConeSquare})
    return MOI.PositiveSemidefiniteConeSquare(3)
end
_set(::Type{MOI.LogDetConeTriangle}) = MOI.LogDetConeTriangle(3)
_set(::Type{MOI.LogDetConeSquare}) = MOI.LogDetConeSquare(3)
_set(::Type{MOI.RootDetConeTriangle}) = MOI.RootDetConeTriangle(3)
_set(::Type{MOI.RootDetConeSquare}) = MOI.RootDetConeSquare(3)
_set(::Type{MOI.Complements}) = MOI.Complements(2)

function _set(
    ::Type{MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}}},
) where {T}
    return MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(convert(T, 3)))
end

function _set(
    ::Type{MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}}},
) where {T}
    return MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.GreaterThan(convert(T, 3)))
end

function _basic_constraint_test_helper(
    model::MOI.ModelLike,
    config::Config{T},
    ::Type{UntypedF},
    ::Type{UntypedS},
) where {T,UntypedF,UntypedS}
    set = _set(T, UntypedS)
    N = MOI.dimension(set)
    x = MOI.add_variables(model, N)
    constraint_function = _function(T, UntypedF, x)
    F, S = typeof(constraint_function), typeof(set)
    ###
    ### Test MOI.supports_constraint
    ###
    @requires MOI.supports_constraint(model, F, S)
    ###
    ### Test MOI.NumberOfConstraints
    ###
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 0
    c = MOI.add_constraint(model, constraint_function, set)
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
    _test_attribute_value_type(model, MOI.NumberOfConstraints{F,S}())
    ###
    ### Test MOI.is_valid
    ###
    @test MOI.is_valid(model, c)
    ###
    ### Test MOI.ConstraintName
    ###
    if _supports(config, MOI.ConstraintName)
        if F == MOI.VariableIndex
            @test_throws(
                MOI.VariableIndexConstraintNameError(),
                MOI.supports(model, MOI.ConstraintName(), typeof(c)),
            )
            @test_throws(
                MOI.VariableIndexConstraintNameError(),
                MOI.set(model, MOI.ConstraintName(), c, "c"),
            )
        else
            @test MOI.get(model, MOI.ConstraintName(), c) == ""
            @test MOI.supports(model, MOI.ConstraintName(), typeof(c))
            MOI.set(model, MOI.ConstraintName(), c, "c")
            @test MOI.get(model, MOI.ConstraintName(), c) == "c"
            _test_attribute_value_type(model, MOI.ConstraintName(), c)
        end
    end
    ###
    ### Test MOI.ConstraintFunction
    ###
    if _supports(config, MOI.ConstraintFunction)
        # Don't compare directly, because `f` might not be canonicalized.
        f = MOI.get(model, MOI.ConstraintFunction(), c)
        @test isapprox(MOI.Utilities.canonical(f), constraint_function, config)
        @test isapprox(
            MOI.get(model, MOI.CanonicalConstraintFunction(), c),
            constraint_function,
            config,
        )
        _test_attribute_value_type(model, MOI.ConstraintFunction(), c)
        _test_attribute_value_type(model, MOI.CanonicalConstraintFunction(), c)
    end
    ###
    ### Test MOI.ConstraintSet
    ###
    if _supports(config, MOI.ConstraintSet)
        @test MOI.get(model, MOI.ConstraintSet(), c) == set
        _test_attribute_value_type(model, MOI.ConstraintSet(), c)
    end
    ###
    ### Test MOI.ListOfConstraintIndices
    ###
    c_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
    @test length(c_indices) == 1
    ###
    ### Test MOI.add_constraints
    ###
    if F != MOI.VariableIndex && F != MOI.VectorOfVariables
        # We can't add multiple variable constraints as these are
        # interpreted as bounds etc.
        MOI.add_constraints(
            model,
            [constraint_function, constraint_function],
            [set, set],
        )
        @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 3
        @test length(MOI.get(model, MOI.ListOfConstraintIndices{F,S}())) == 3
        c_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        @test all(MOI.is_valid.(model, c_indices))
    end
    ###
    ### Test MOI.delete
    ###
    if _supports(config, MOI.delete)
        MOI.delete(model, c_indices[1])
        @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) ==
              length(c_indices) - 1
        @test !MOI.is_valid(model, c_indices[1])
        @test_throws(
            MOI.InvalidIndex(c_indices[1]),
            MOI.delete(model, c_indices[1]),
        )
        if _supports(config, MOI.ConstraintFunction)
            @test_throws(
                MOI.InvalidIndex(c_indices[1]),
                MOI.get(model, MOI.ConstraintFunction(), c_indices[1]),
            )
        end
        if _supports(config, MOI.ConstraintSet)
            @test_throws(
                MOI.InvalidIndex(c_indices[1]),
                MOI.get(model, MOI.ConstraintSet(), c_indices[1]),
            )
        end
    end
    return
end

for s in [
    :GreaterThan,
    :LessThan,
    :EqualTo,
    :Interval,
    :ZeroOne,
    :Semicontinuous,
    :Semiinteger,
    :SOS1,
    :SOS2,
    :Zeros,
    :Nonpositives,
    :Nonnegatives,
    :NormInfinityCone,
    :NormOneCone,
    :SecondOrderCone,
    :RotatedSecondOrderCone,
    :GeometricMeanCone,
    :ExponentialCone,
    :DualExponentialCone,
    :PowerCone,
    :DualPowerCone,
    :RelativeEntropyCone,
    :NormSpectralCone,
    :NormNuclearCone,
    :PositiveSemidefiniteConeSquare,
    :PositiveSemidefiniteConeTriangle,
    :LogDetConeTriangle,
    :LogDetConeSquare,
    :RootDetConeTriangle,
    :RootDetConeSquare,
    :Complements,
]
    S = getfield(MOI, s)
    functions = if S <: MOI.AbstractScalarSet
        (:VariableIndex, :ScalarAffineFunction, :ScalarQuadraticFunction)
    else
        (:VectorOfVariables, :VectorAffineFunction, :VectorQuadraticFunction)
    end
    for f in functions
        func = Symbol("test_basic_$(f)_$(s)")
        F = getfield(MOI, f)
        @eval begin
            function $(func)(model::MOI.ModelLike, config::Config)
                _basic_constraint_test_helper(model, config, $F, $S)
                return
            end
        end
    end
end

function test_basic_VectorAffineFunction_Indicator_LessThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _basic_constraint_test_helper(
        model,
        config,
        MOI.VectorAffineFunction,
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}},
    )
    return
end

function test_basic_VectorAffineFunction_Indicator_GreaterThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    _basic_constraint_test_helper(
        model,
        config,
        MOI.VectorAffineFunction,
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}},
    )
    return
end
