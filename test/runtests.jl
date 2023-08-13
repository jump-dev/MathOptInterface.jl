# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# To try and speed up the tests, MOI uses a `MOI_TEST_MODULES` environment
# variable. This environment variable may be missing, or it may be a subset of
# the following, concatenated with `;` as a separator:
#
#  * General
#  * Benchmarks
#  * Bridges
#  * FileFormats
#  * Nonlinear
#  * Test
#  * Utilities
#
# If present, the tests run only those submodules defined above. `General` is
# not a submodule, but it runs all of the top-level tests in MOI.

using Test

# This file gets called first. If it doesn't crash, all is well.
include("issue980.jl")

import MathOptInterface as MOI

MODULES_TO_TEST = get(
    ENV,
    "MOI_TEST_MODULES",
    "General;Benchmarks;Bridges;FileFormats;Nonlinear;Test;Utilities",
)

if occursin("General", MODULES_TO_TEST)
    @test isempty(Test.detect_ambiguities(MOI; recursive = true))
    for file in readdir(@__DIR__)
        if file in ["issue980.jl", "dummy.jl", "hygiene.jl", "runtests.jl"]
            continue
        elseif !endswith(file, ".jl")
            continue
        end
        @testset "$(file)" begin
            include(file)
        end
    end
end

for submodule in split(MODULES_TO_TEST, ";")
    if submodule == "General"
        continue
    end
    include("$(submodule)/$(submodule).jl")
    GC.gc()  # Force GC run here to reduce memory pressure
end

if occursin("General", MODULES_TO_TEST)
    # Test hygiene of @model macro
    include("hygiene.jl")
end
