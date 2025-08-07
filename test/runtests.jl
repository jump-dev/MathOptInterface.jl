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
    "General;Benchmarks;Bridges;Bridges/Constraint;Bridges/Objective;Bridges/Variable;FileFormats;Nonlinear;Test;Utilities",
)

"""
    include_with_method_redefinition_check(jl_filename)

This function runs `include(jl_filename)` with an additional check that there
are no `WARNING: Method definition foo in module Foo overwritten` warnings.

It does this by piping `stderr` to a file, and then parsing the file.

One consequence is that `stderr` is not printed until the _end_ of this
function. Thus, some warnings may appear in the wrong place.

This function requires Julia to be started with `--warn-overwrite=true`.
"""
function include_with_method_redefinition_check(jl_filename)
    stderr_filename = tempname()
    open(stderr_filename, "w") do io
        return redirect_stderr(() -> include(jl_filename), io)
    end
    contents = read(stderr_filename, String)
    print(stderr, contents)
    regex =
        r"WARNING: Method definition (.+?) in module (.+?) at (.+?) overwritten at (.+?)\n"
    if match(regex, contents) !== nothing
        error("Found overwritten method")
    end
    return
end

for submodule in split(MODULES_TO_TEST, ";")
    include_with_method_redefinition_check("$(submodule)/runtests.jl")
    GC.gc()  # Force GC run here to reduce memory pressure
end

if occursin("General", MODULES_TO_TEST)
    # Test hygiene of @model macro
    include("hygiene.jl")
end
