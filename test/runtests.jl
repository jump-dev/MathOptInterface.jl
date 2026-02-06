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
#  * Bridges/General
#  * Bridges/Constraint
#  * Bridges/Objective
#  * Bridges/Variable
#  * FileFormats
#  * Nonlinear
#  * Test
#  * Utilities
#
# If present, the tests run only those submodules defined above. `General` is
# not a submodule, but it runs all of the top-level tests in MOI.

# This file gets called first. If it doesn't crash, all is well.
include("issue980.jl")

MODULES_TO_TEST = get(
    ENV,
    "MOI_TEST_MODULES",
    "General;Benchmarks;Bridges/General;Bridges/Constraint;Bridges/Objective;Bridges/Variable;FileFormats;Nonlinear;Test;Utilities",
)

is_test_file(f) = startswith(f, "test_") && endswith(f, ".jl")

testsuite = Dict{String,Expr}()
for submodule in split(MODULES_TO_TEST, ";")
    for (root, dirs, files) in walkdir(submodule)
        for file in joinpath.(root, filter(is_test_file, files))
            testsuite[file] = :(include($file))
        end
    end
end

import MathOptInterface
import ParallelTestRunner
import Test

if Sys.WORD_SIZE == 64
    ParallelTestRunner.runtests(MathOptInterface, ARGS; testsuite)
else
    Test.@testset "$filename" for filename in keys(testsuite)
        include(filename)
    end
end
