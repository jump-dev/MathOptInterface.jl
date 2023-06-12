# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Test

# This file gets called first. If it doesn't crash, all is well.
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

for submodule in
    ["Nonlinear", "Bridges", "FileFormats", "Test", "Utilities", "Benchmarks"]
    include("$(submodule)/$(submodule).jl")
end

# Test hygiene of @model macro
include("hygiene.jl")
