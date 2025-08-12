# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Test

files_to_exclude =
    ["runtests.jl", "sdpa_models.jl", "utilities.jl", "identity_bridge.jl"]
@testset "$(file)" for file in readdir(@__DIR__; join = true)
    if !endswith(file, ".jl") || any(f -> endswith(file, f), files_to_exclude)
        continue
    end
    include(file)
end
