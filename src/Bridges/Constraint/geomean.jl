function _ilog2(n, i)
    if n <= (one(n) << i)
        i
    else
        _ilog2(n, i+1)
    end
end
function ilog2(n::Integer)
    @assert n > zero(n)
    _ilog2(n, zero(n))
end

"""
    GeoMeanBridge{T, F, G, H}

The `GeometricMeanCone` is `SecondOrderCone` representable; see [1, p. 105].
The reformulation is best described in an example.
Consider the cone of dimension 4
```math
t \\le \\sqrt[3]{x_1 x_2 x_3}
```
This can be rewritten as ``\\exists x_{21} \\ge 0`` such that
```math
\\begin{align*}
  t & \\le x_{21},\\\\
  x_{21}^4 & \\le x_1 x_2 x_3 x_{21}.
\\end{align*}
```
Note that we need to create ``x_{21}`` and not use ``t^4`` directly as ``t`` is allowed to be negative.
Now, this is equivalent to
```math
\\begin{align*}
  t & \\le x_{21}/\\sqrt{4},\\\\
  x_{21}^2 & \\le 2x_{11} x_{12},\\\\
  x_{11}^2 & \\le 2x_1 x_2, & x_{12}^2 & \\le 2x_3(x_{21}/\\sqrt{4}).
\\end{align*}
```

[1] Ben-Tal, Aharon, and Arkadi Nemirovski. *Lectures on modern convex optimization: analysis, algorithms, and engineering applications*. Society for Industrial and Applied Mathematics, 2001.
"""
struct GeoMeanBridge{T, F, G, H} <: AbstractBridge
    # Initially, (t, x) is of dimension d so x is dimension (d-1)
    # TODO the sentence below is a little confusing, the n isn't used anywhere
    # We create n new variables so that there are 2^l = d-1+n variables x_i
    # We then need to create 2^l-1 new variables (1+2+...+2^{l-1})
    # So the total number of variables is d+2^l-1
    d::Int
    xij::Vector{MOI.VariableIndex}
    tubc::CI{F, MOI.LessThan{T}}
    socrc::Vector{CI{G, MOI.RotatedSecondOrderCone}}
    nonneg::Union{Nothing, CI{H, MOI.Nonnegatives}}
end
function bridge_constraint(::Type{GeoMeanBridge{T, F, G, H}}, model,
                           f::MOI.AbstractVectorFunction,
                           s::MOI.GeometricMeanCone) where {T, F, G, H}
    d = s.dimension
    if d <= 1
        # TODO change to a standard error: https://github.com/jump-dev/MathOptInterface.jl/issues/967
        error("Dimension of GeometricMeanCone must be greater than 1.")
    end
    n = d - 1
    l = ilog2(n)
    N = 1 << l
    socrc = Vector{CI{G, MOI.RotatedSecondOrderCone}}(undef, N - 1)
    f_scalars = MOIU.eachscalar(f)
    t = f_scalars[1]
    SG = MOIU.scalar_type(G)

    if d == 2
        xij = MOI.VariableIndex[]
        tubc = MOIU.normalize_and_add_constraint(
            model, MOIU.operate!(-, T, t, f_scalars[2]), MOI.LessThan(zero(T)),
            allow_modify_function=true)
        nonneg = MOI.add_constraint(model, f_scalars[2:end], MOI.Nonnegatives(1))
        return GeoMeanBridge{T, F, G, H}(d, xij, tubc, socrc, nonneg)
    end

    xij = MOI.add_variables(model, N - 1)
    xl1 = MOI.SingleVariable(xij[1])
    sN = one(T) / √N
    function _getx(i)::SG
        if i > n
            return sN * xl1
        else
            return f_scalars[1 + i]
        end
    end

    # With sqrt(2)^l*t - xl1, we should scale both the ConstraintPrimal and ConstraintDual
    tubc = MOIU.normalize_and_add_constraint(
        model, MOIU.operate!(+, T, t, -sN * xl1), MOI.LessThan(zero(T)),
        allow_modify_function=true)

    offset = offset_next = 0
    for i in 1:l
        num_lvars = 1 << (i - 1)
        offset_next = offset + num_lvars
        for j in 1:num_lvars
            if i == l
                a = _getx(2j - 1)
                b = _getx(2j)
            else
                a = convert(SG, MOI.SingleVariable(xij[offset_next + 2j - 1]))
                b = convert(SG, MOI.SingleVariable(xij[offset_next + 2j]))
            end
            c = MOI.SingleVariable(xij[offset+j])
            socrc[offset + j] = MOI.add_constraint(
                model, MOIU.operate(vcat, T, a, b, c),
                MOI.RotatedSecondOrderCone(3))
        end
        offset = offset_next
    end
    @assert offset == length(socrc)
    return GeoMeanBridge{T, F, G, H}(d, xij, tubc, socrc, nothing)
end

function MOI.supports_constraint(::Type{GeoMeanBridge{T}},
                                ::Type{<:MOI.AbstractVectorFunction},
                                ::Type{MOI.GeometricMeanCone}) where T
    return true
end
MOIB.added_constrained_variable_types(::Type{<:GeoMeanBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{<:GeoMeanBridge{T, F, G}}) where {T, F, G}
    return [(F, MOI.LessThan{T}), (G, MOI.RotatedSecondOrderCone), (G, MOI.Nonnegatives)]
end
function concrete_bridge_type(::Type{<:GeoMeanBridge{T}},
                              H::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.GeometricMeanCone}) where T
    S = MOIU.scalar_type(H)
    A = MOIU.promote_operation(*, T, T, MOI.SingleVariable)
    F = MOIU.promote_operation(+, T, S, A)
    G = MOIU.promote_operation(vcat, T, A, S, MOI.SingleVariable)
    return GeoMeanBridge{T, F, G, H}
end

# Attributes, Bridge acting as a model
MOI.get(b::GeoMeanBridge, ::MOI.NumberOfVariables) = length(b.xij)
MOI.get(b::GeoMeanBridge, ::MOI.ListOfVariableIndices) = b.xij
function MOI.get(b::GeoMeanBridge{T, F},
                 ::MOI.NumberOfConstraints{F, MOI.LessThan{T}}) where {T, F}
    return 1 # t ≤ x_{l1}/sqrt(N)
end
function MOI.get(b::GeoMeanBridge{T, F, G},
                 ::MOI.NumberOfConstraints{G, MOI.RotatedSecondOrderCone}) where {T, F, G}
    return length(b.socrc)
end
function MOI.get(b::GeoMeanBridge{T, F, G},
                 ::MOI.NumberOfConstraints{G, MOI.Nonnegatives}) where {T, F, G}
    return (b.d > 2 ? 0 : 1)
end
function MOI.get(b::GeoMeanBridge{T, F},
                 ::MOI.ListOfConstraintIndices{F, MOI.LessThan{T}}) where {T, F}
    return [b.tubc]
end
function MOI.get(b::GeoMeanBridge{T, F, G},
                 ::MOI.ListOfConstraintIndices{G, MOI.RotatedSecondOrderCone}) where {T, F, G}
    return b.socrc
end
function MOI.get(b::GeoMeanBridge{T, F, G, H},
                 ::MOI.ListOfConstraintIndices{H, MOI.Nonnegatives}) where {T, F, G, H}
    return (b.d > 2 ? MOI.ConstraintIndex{H, MOI.Nonnegatives}[] : [b.nonneg])
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::GeoMeanBridge)
    MOI.delete(model, bridge.xij)
    MOI.delete(model, bridge.tubc)
    MOI.delete(model, bridge.socrc)
    if bridge.d == 2
        MOI.delete(model, bridge.nonneg)
    end
end

# Attributes, Bridge acting as a constraint
function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::GeoMeanBridge)
    return MOI.GeometricMeanCone(bridge.d)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 bridge::GeoMeanBridge{T, F, G, H}) where {T, F, G, H}
    d = bridge.d
    f_scalars = Vector{MOIU.scalar_type(H)}(undef, bridge.d)
    tub = MOI.get(model, attr, bridge.tubc)
    rhs = MOI.constant(MOI.get(model, MOI.ConstraintSet(), bridge.tubc))
    tub = MOIU.operate(-, T, tub, rhs)
    if d == 2
        x = MOI.get(model, attr, bridge.nonneg)
        f_scalars[2] = MOIU.eachscalar(x)[1]
        f_scalars[1] = MOIU.operate(+, T, tub, f_scalars[2])
    else
        t = MOIU.remove_variable(tub, bridge.xij[1])
        f_scalars[1] = t
        n = d - 1
        l = ilog2(n)
        num_lvars = 1 << (l - 1)
        offset = num_lvars - 1
        for j in 1:num_lvars
            if 2j <= bridge.d
                func = MOI.get(model, attr, bridge.socrc[offset + j])
                func_scalars = MOIU.eachscalar(func)
                f_scalars[2j] = func_scalars[1]
                if 2j + 1 <= bridge.d
                    f_scalars[2j + 1] = func_scalars[2]
                end
            end
        end
    end
    return MOIU.vectorize(f_scalars)
end
function _getconstrattr(model, attr, bridge::GeoMeanBridge{T}) where T
    output = Vector{T}(undef, bridge.d)
    output[1] = MOI.get(model, attr, bridge.tubc)
    if bridge.d == 2
        output[2] = MOI.get(model, attr, bridge.nonneg)[1]
    else
        N = length(bridge.xij) + 1
        # div(N, 2) gets layer before original problem variables are involved
        offset = div(N, 2) - 1 # 1 + 2 + ... + n/4
        for i in 1:(bridge.d - 1)
            j = ((i - 1) >> 1) + 1
            k = i - 2(j - 1)
            output[1+i] = MOI.get(model, attr, bridge.socrc[offset + j])[k]
        end
    end
    output
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, bridge::GeoMeanBridge)
    output = _getconstrattr(model, attr, bridge)
    N = length(bridge.xij) + 1
    # the constraint is t - x_l1/sqrt(2^l) ≤ 0, we need to add the value of x_l1
    if bridge.d == 2
        output[1] += MOI.get(model, attr, bridge.nonneg)[1]
    else
        output[1] += MOI.get(model, MOI.VariablePrimal(), bridge.xij[1]) / sqrt(N)
    end
    return output
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual, bridge::GeoMeanBridge)
    output = _getconstrattr(model, attr, bridge)
    if bridge.d == 2
        # The transformation is:
        # [t]                [1 -1] [t]
        # [x] in GeoMean <=> [0  1] [x] in R- x R+
        # Hence the dual transformation is
        # [ 1 0] [u]                 [u]
        # [-1 1] [v] in GeoMean* <=> [v] in R- x R+
        output[2] -= MOI.get(model, attr, bridge.tubc)
    end
    # Otherwise, the transformation is:
    # [t]                                  [t]
    # [x] in GeoMean <=> exists xij s.t. A [x] + B xij in R- x RSOC^(N-1)
    # Hence the dual transformation is
    # A' y in GeoMean* <=>
    # B' y = 0         <=> y in R- x RSOC^(N-1)
    # Here A has at most one nonzero entry by line and column and this entry is 1.
    # Therefore we can A' = inv(A) hence we can use the same `getconstrattr` function
    # than for getting the `ConstraintPrimal`.
    # We don't need to use B' y = 0.
    return output
end
