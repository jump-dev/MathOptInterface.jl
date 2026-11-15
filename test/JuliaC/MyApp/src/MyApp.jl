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
    if head == "--constraints"
        return run_constraints(args)
    end
    if head == "--sets"
        return run_sets(args)
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

# A starting value, which adds a field to the object of the variable.
function run_starts(args::Vector{String})::Cint
    model, x = _model()
    MOI.set(model, MOI.VariablePrimalStart(), x, 1.0)
    return _write(model, args)
end

# Without the nonlinear constraint, the model has another layout.
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

# Constraint types that the traced model does not have, in its sets.
function run_constraints(args::Vector{String})::Cint
    model, x = _model()
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        MOI.ScalarAffineTerm{Float64}[],
        0.0,
    )
    q = MOI.add_constraint(model, f, MOI.LessThan(4.0))
    MOI.set(model, MOI.ConstraintName(), q, "q")
    g = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    a = MOI.add_constraint(model, g, MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ConstraintName(), a, "a")
    return _write(model, args)
end

# A set written by the generic writer of sets, with its fields.
function run_sets(args::Vector{String})::Cint
    model, x = _model()
    f = MOI.VectorOfVariables([x, x, x])
    s = MOI.add_constraint(model, f, MOI.SecondOrderCone(3))
    MOI.set(model, MOI.ConstraintName(), s, "s")
    g = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
        [0.0],
    )
    v = MOI.add_constraint(model, g, MOI.Nonnegatives(1))
    MOI.set(model, MOI.ConstraintName(), v, "v")
    return _write(model, args)
end

end # module MyApp
