"""
    get_bounds(model::MOI.ModelLike, ::Type{T}, x::MOI.VariableIndex)

Return a tuple `(lb, ub)` of type `Tuple{T, T}`, where `lb` and `ub` are lower
 and upper bounds, respectively, imposed on `x` in `model`.
"""
function get_bounds(model::MOI.ModelLike, ::Type{T}, x::MOI.VariableIndex) where {T}
    c_lt = MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}(x.value)
    c_gt = MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}(x.value)
    c_int = MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}(x.value)
    if MOI.is_valid(model, c_int)
        @assert !MOI.is_valid(model, c_lt) && !MOI.is_valid(model, c_gt)
        int::MOI.Interval{T} = MOI.get(model, MOI.ConstraintSet(), c_int)
        return int.lower, int.upper
    elseif MOI.is_valid(model, c_lt)
        @assert !MOI.is_valid(model, c_gt)
        lt::MOI.LessThan{T} = MOI.get(model, MOI.ConstraintSet(), c_lt)
        return typemin(T), lt.upper
    elseif MOI.is_valid(model, c_gt)
        gt::MOI.GreaterThan{T} = MOI.get(model, MOI.ConstraintSet(), c_gt)
        return gt.lower, typemax(T)
    else
        return typemin(T), typemax(T)
    end
end
