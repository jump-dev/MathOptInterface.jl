# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestCachingOptimizerRuntests

using Test

import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_MOI_Test()
    # It seems like these loops might take a while. But the first one takes
    # _forever_, and then the rest are really quick (like 140s vs 0.4s).
    for state in (
        MOI.Utilities.NO_OPTIMIZER,
        MOI.Utilities.EMPTY_OPTIMIZER,
        MOI.Utilities.ATTACHED_OPTIMIZER,
    )
        for mode in (MOI.Utilities.MANUAL, MOI.Utilities.AUTOMATIC)
            _test_caching_optimizer(state, mode)
        end
    end
    return
end

function _test_caching_optimizer(state, mode)
    model = MOI.Utilities.CachingOptimizer(MOI.Utilities.Model{Float64}(), mode)
    if state != MOI.Utilities.NO_OPTIMIZER
        optimizer = MOI.Utilities.MockOptimizer(
            MOI.Utilities.Model{Float64}(),
            supports_names = false,
        )
        MOI.Utilities.reset_optimizer(model, optimizer)
        if state == MOI.Utilities.ATTACHED_OPTIMIZER
            MOI.Utilities.attach_optimizer(model)
        end
    end
    @test MOI.Utilities.state(model) == state
    @test MOI.Utilities.mode(model) == mode
    MOI.Test.runtests(
        model,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        exclude = ["test_attribute_SolverName", "test_attribute_SolverVersion"],
    )
    return
end

end  # module

TestCachingOptimizerRuntests.runtests()
