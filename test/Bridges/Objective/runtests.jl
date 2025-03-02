# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Test

@testset "$(file)" for file in readdir(@__DIR__; join = true)
    if !endswith(file, ".jl") || endswith(file, "runtests.jl")
        continue
    end
    include(file)
end
