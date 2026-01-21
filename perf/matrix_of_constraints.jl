# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

import SparseArrays
using BenchmarkTools

import MathOptInterface as MOI

# Inspired from MatrixOfConstraints
MOI.Utilities.@product_of_sets(_EqualTos, MOI.EqualTo{T},)

a = 1

function _equality_constraints(
    A::AbstractMatrix{T},
    b::AbstractVector{T},
) where {T}
    sets = _EqualTos{T}()
    for _ in eachindex(b)
        MOI.Utilities.add_set(
            sets,
            MOI.Utilities.set_index(sets, MOI.EqualTo{T}),
        )
    end
    MOI.Utilities.final_touch(sets)
    constants = MOI.Utilities.Hyperrectangle(b, b)
    model = MOI.Utilities.MatrixOfConstraints{T}(A, constants, sets)
    model.final_touch = true
    return model
end

function _free_variables(n, ::Type{T}) where {T}
    model = MOI.Utilities.VariablesContainer{T}()
    for _ in 1:n
        MOI.add_variable(model)
    end
    return model
end

function lp_standard_form(m, n; p = 0.1, tr = false)
    T = Float64
    A = SparseArrays.sprand(T, m, n, p)
    if tr
        A = copy(A')'
    end
    b = rand(m)
    return MOI.Utilities.GenericModel{T}(
        MOI.Utilities.ObjectiveContainer{T}(),
        _free_variables(n, T),
        _equality_constraints(A, b),
    )
end

function bench(m, args...; kws...)
    T = Float64
    model = lp_standard_form(m, args...; kws...)
    ci = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}(
        rand(1:m),
    )
    return @benchmark MOI.get($model, MOI.ConstraintFunction(), $ci)
end

bench(10, 100000)
bench(10, 100000, tr = true)

function prof(m, args...)
    T = Float64
    model = lp_standard_form(m, args...)
    return @profview for i in 1:m
        ci = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}(
            rand(1:m),
        )
        MOI.get(model, MOI.ConstraintFunction(), ci)
    end
end

prof(10, 100000)
