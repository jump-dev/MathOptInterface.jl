"""
    add_scalar_constraint(model::MOI.ModelLike,
                          func::MOI.AbstractScalarFunction,
                          set::MOI.AbstractScalarSet;
                          allow_modify_function::Bool=false)

Adds the scalar constraint obtained by moving the constant term in `func` to
the set in `model`. If `allow_modify_function` is `true` then the function
`func`, can be modified.
"""
function add_scalar_constraint end

function add_scalar_constraint(model::MOI.ModelLike,
                               func::MOI.AbstractScalarFunction,
                               set::MOI.AbstractScalarSet;
                               allow_modify_function::Bool=false) where T
    return MOI.add_constraint(
        model, normalize_constant(
            func, set; allow_modify_function=allow_modify_function)...)
end

function normalize_constant(func::MOI.AbstractFunction, set::MOI.AbstractSet;
                            allow_modify_function::Bool=false)
    return func, set
end
function normalize_constant(
    func::Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}},
    set::MOI.AbstractScalarSet; allow_modify_function::Bool=false) where T
    set = shift_constant(set, -func.constant)
    if !allow_modify_function
        func = copy(func)
    end
    func.constant = zero(T)
    return func, set
end
