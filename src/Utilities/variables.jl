"""
    get_bounds(model::MOI.ModelLike, ::Type{T}, x::MOI.VariableIndex)

Return a tuple `(lb, ub)` of type `Tuple{T, T}`, where `lb` and `ub` are lower
 and upper bounds, respectively, imposed on `x` in `model`.
"""
function get_bounds(
    model::MOI.ModelLike,
    ::Type{T},
    x::MOI.VariableIndex,
) where {T}
    xval = x.value
    c_lt = MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{T}}(xval)
    c_gt = MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{T}}(xval)
    c_int = MOI.ConstraintIndex{MOI.SingleVariable,MOI.Interval{T}}(xval)
    c_eq = MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{T}}(xval)
    c_sc = MOI.ConstraintIndex{MOI.SingleVariable,MOI.Semicontinuous{T}}(xval)
    c_si = MOI.ConstraintIndex{MOI.SingleVariable,MOI.Semiinteger{T}}(xval)
    if MOI.is_valid(model, c_int)
        # It is assumed that none of the other ConstraintIndexs are valid
        int::MOI.Interval{T} = MOI.get(model, MOI.ConstraintSet(), c_int)
        return int.lower, int.upper
    elseif MOI.is_valid(model, c_eq)
        # It is assumed that none of the other ConstraintIndexs are valid
        eq::MOI.EqualTo{T} = MOI.get(model, MOI.ConstraintSet(), c_eq)
        return eq.value, eq.value
    elseif MOI.is_valid(model, c_sc)
        # It is assumed that none of the other ConstraintIndexs are valid
        sc::MOI.Semicontinuous{T} = MOI.get(model, MOI.ConstraintSet(), c_sc)
        return min(zero(T), sc.lower), max(zero(T), sc.upper)
    elseif MOI.is_valid(model, c_si)
        # It is assumed that none of the other ConstraintIndexs are valid
        si::MOI.Semiinteger{T} = MOI.get(model, MOI.ConstraintSet(), c_si)
        return min(zero(T), si.lower), max(zero(T), si.upper)
    elseif MOI.is_valid(model, c_lt)
        lt::MOI.LessThan{T} = MOI.get(model, MOI.ConstraintSet(), c_lt)
        # It is valid to have both LessThan and GreaterThan constraints on the
        # same variable.
        if MOI.is_valid(model, c_gt)
            gt_1::MOI.GreaterThan{T} = MOI.get(model, MOI.ConstraintSet(), c_gt)
            return gt_1.lower, lt.upper
        else
            return typemin(T), lt.upper
        end
    elseif MOI.is_valid(model, c_gt)
        gt_2::MOI.GreaterThan{T} = MOI.get(model, MOI.ConstraintSet(), c_gt)
        return gt_2.lower, typemax(T)
    else
        return typemin(T), typemax(T)
    end
end
