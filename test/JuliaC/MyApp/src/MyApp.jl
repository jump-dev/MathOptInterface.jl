# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# !!! info
#     There is a one-to-one correspondence between the `run_` functions in this
#     module and the `test_` functions in test_juliac.jl. If you add to one, you
#     must also add to the other.

module MyApp

import MathOptInterface as MOI

function @main(args::Vector{String})::Cint
    @assert length(args) >= 1
    head = popfirst!(args)
    if head == "--mathoptformat"
        return run_mathoptformat(args)
    end
    # The following commands are not traced by `test_juliac.jl`: the compiled
    # binary must run them with the methods resolved at compile time.
    if head == "--starts"
        return run_starts(args)
    end
    if head == "--linear"
        return run_linear(args)
    end
    if head == "--names"
        return run_names(args)
    end
    return 1  # Incorrect head
end

# The model of the traced command. Each command that is not traced writes this
# model with one difference, so that it tests one code path that is not traced.
function _model()
    model = MOI.FileFormats.MOF.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.add_constraint(model, x, MOI.GreaterThan(0.0))
    f = MOI.ScalarNonlinearFunction(
        :+,
        Any[MOI.ScalarNonlinearFunction(:sin, Any[x]), 2, 3.0],
    )
    c = MOI.add_constraint(model, f, MOI.LessThan(7.0))
    MOI.set(model, MOI.ConstraintName(), c, "c")
    objective = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(objective)}(), objective)
    return model, x
end

function _write(model, args::Vector{String})::Cint
    filename = only(args)
    open(io -> write(io, model), filename, "w")
    return 0
end

function run_mathoptformat(args::Vector{String})::Cint
    model, _ = _model()
    return _write(model, args)
end

# A starting value for `x`, so the object of the variable in the document has a
# `primal_start` field, which the traced model does not have. When the objects
# were `NamedTuple`s, JSON needed methods for each combination of fields, so
# this failed without a trace, see
# https://github.com/jump-dev/MathOptInterface.jl/pull/3151.
function run_starts(args::Vector{String})::Cint
    model, x = _model()
    MOI.set(model, MOI.VariablePrimalStart(), x, 1.0)
    return _write(model, args)
end

# The traced model without its nonlinear constraint, so the document has no
# `has_scalar_nonlinear` field: its top-level object has other fields than the
# one of the traced model, see
# https://github.com/jump-dev/MathOptInterface.jl/pull/3151.
function run_linear(args::Vector{String})::Cint
    model = MOI.FileFormats.MOF.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.add_constraint(model, x, MOI.GreaterThan(0.0))
    objective = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(objective)}(), objective)
    return _write(model, args)
end

# A second variable named `x`, which is renamed.
function run_names(args::Vector{String})::Cint
    model, _ = _model()
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), y, "x")
    return _write(model, args)
end

end # module MyApp
