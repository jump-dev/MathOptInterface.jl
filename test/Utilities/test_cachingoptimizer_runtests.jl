# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestCachingOptimizerRuntests

using Test

import MathOptInterface as MOI
import MathOptInterface.Utilities as MOIU

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
    for state in
        (MOIU.NO_OPTIMIZER, MOIU.EMPTY_OPTIMIZER, MOIU.ATTACHED_OPTIMIZER)
        for mode in (MOIU.MANUAL, MOIU.AUTOMATIC)
            _test_caching_optimizer(state, mode)
        end
    end
    return
end

function _test_caching_optimizer(state, mode)
    model = MOIU.CachingOptimizer(MOIU.Model{Float64}(), mode)
    if state != MOIU.NO_OPTIMIZER
        optimizer = MOIU.MockOptimizer(
            MOIU.Model{Float64}(),
            supports_names = false,
        )
        MOIU.reset_optimizer(model, optimizer)
        if state == MOIU.ATTACHED_OPTIMIZER
            MOIU.attach_optimizer(model)
        end
    end
    @test MOIU.state(model) == state
    @test MOIU.mode(model) == mode
    MOI.Test.runtests(
        model,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        exclude = [
            "test_attribute_SolverName",
            "test_attribute_SolverVersion",
        ],
    )
    return
end

end  # module

TestCachingOptimizerRuntests.runtests()
