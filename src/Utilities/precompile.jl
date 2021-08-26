function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    T = Float64
    scalar_sets = (
        MOI.LessThan{T},
        MOI.GreaterThan{T},
        MOI.EqualTo{T},
        MOI.Interval{T},
        MOI.Integer,
        MOI.ZeroOne,
        MOI.Semiinteger{T},
        MOI.Semicontinuous{T},
    )
    scalar_functions = (
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    )
    vector_sets = (
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.Zeros,
        MOI.Reals,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.PositiveSemidefiniteConeSquare,
        MOI.PositiveSemidefiniteConeTriangle,
    )
    vector_functions = (
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    )
    constraints = vcat(
        [(F, S) for F in scalar_functions, S in scalar_sets],
        [(F, S) for F in vector_functions, S in vector_sets],
    )
    MOI.precompile_model(UniversalFallback{Model{T}}, constraints)
    MOI.precompile_model(
        CachingOptimizer{MOI.AbstractOptimizer,UniversalFallback{Model{T}}},
        constraints,
    )
    return
end
