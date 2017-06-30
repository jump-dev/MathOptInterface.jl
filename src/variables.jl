"""
    addvariables!(m::AbstractModel, N::Int)::Vector{VariableReference}

Add `N` scalar variables to the model, returning a vector of variable
references.
"""
function addvariables! end

"""
    addvariable!(m::AbstractModel)::VariableReference

Add a scalar variable to the model, returning a variable reference.

In addition, there is a special case for adding variables to existing linear problems.

    addvariable!(m::AbstractModel,
        cref::Vector{Union{
                AffineConstraintRef{NonPositive},
                AffineConstraintRef{NonNegative},
                AffineConstraintRef{Zero},
                AffineConstraintRef{Interval}
            }},
        coefs)::VariableReference

Add a variable with coefficients specified by `coefs` in the existing affine
constraints given by the constraint references `cref`. If you want to add a
variable with coefficients in a constraint that is not listed here (such as a
quadratic term, or in the SOC), use `addvariable!(m)` and then
`modifyconstraint!` instead.
"""
function addvariable! end
