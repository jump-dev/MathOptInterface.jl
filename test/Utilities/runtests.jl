# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Test

for file in readdir(@__DIR__)
    if file == "runtests.jl"
        continue
    end
    @testset "$(file)" begin
        include(file)
    end
end
