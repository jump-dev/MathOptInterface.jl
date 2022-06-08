# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Test

# Should be of form
#   "base", "Nonlinear", "Bridges", "FileFormats", "Test", "Utilities", "Benchmarks"
# or some combination, separated by `;`
#   "base;Benchmarks;FileFormats"
MODULES_TO_TEST = get(
    ENV,
    "JULIA_TEST_MATHOPTINTERFACE",
    "base;Nonlinear;Bridges;FileFormats;Test;Utilities;Benchmarks",
)

if occursin("base", MODULES_TO_TEST)
    # This file gets called first. It it doesn't crash, all is well.
    include("issue980.jl")
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

@testset "MOI.$(submodule)" for submodule in [
    "Nonlinear",
    "Bridges",
    "FileFormats",
    "Test",
    "Utilities",
    "Benchmarks",
]
    if occursin(submodule, MODULES_TO_TEST)
        include("$(submodule)/$(submodule).jl")
    end
end

if occursin("base", MODULES_TO_TEST)
    # Test hygiene of @model macro
    include("hygiene.jl")
end
