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
    return 1  # Incorrect head
end

function run_mathoptformat(args::Vector{String})::Cint
    filename = only(args)
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
    open(io -> write(io, model), filename, "w")
    return 0
end

end # module MOFWriter
