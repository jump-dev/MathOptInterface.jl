# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestModelRuntests

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
    MOI.Test.runtests(
        MOI.Utilities.Model{Float64}(),
        MOI.Test.Config(exclude = Any[MOI.optimize!,]),
    )
    return
end

end  # module

TestModelRuntests.runtests()
