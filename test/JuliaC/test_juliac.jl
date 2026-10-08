# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# !!! info
#     There is a one-to-one correspondence between the `test_` functions in this
#     module and the `run_` functions in MyApp.jl. If you add to one, you must
#     also add to the other.

# !!! info
#     To run this file, you must first instantiate the MyApp Project.toml so
#     that it includes MathOptInterface from the source code. From the root, do:
#     ```julia
#     using Pkg
#     Pkg.develop(PackageSpec(path=pwd()))
#     Pkg.instantiate()
#     ```
module TestJuliaC

using Test

import JSON
import JuliaC

function compile()
    output_dir = mktempdir()
    outname = joinpath(output_dir, "MyApp")
    image_recipe = JuliaC.ImageRecipe(
        output_type = "--output-exe",
        file = joinpath(@__DIR__, "MyApp"),
        trim_mode = "unsafe-warn",
        add_ccallables = false,
        verbose = true,
    )
    link_recipe = JuliaC.LinkRecipe(; image_recipe, outname)
    bundle_recipe = JuliaC.BundleRecipe(; link_recipe, output_dir)
    JuliaC.compile_products(image_recipe)
    JuliaC.link_products(link_recipe)
    JuliaC.bundle_products(bundle_recipe)
    return output_dir
end

function runtests()
    output_dir = compile()
    is_test(name) = startswith("$name", "test_")
    @testset "$name" for name in filter(is_test, names(@__MODULE__; all = true))
        getfield(@__MODULE__, name)(output_dir)
    end
    return
end

function test_mathoptformat(output_dir)
    # Run via the compiled binary
    output = joinpath(output_dir, "compiled.mof.json")
    run(`$(output_dir)/bin/MyApp --mathoptformat $output`)
    # Compare against running as `-m`
    reference = joinpath(output_dir, "reference.mof.json")
    project = joinpath(@__DIR__, "MyApp")
    run(
        `$(Base.julia_cmd()) --project=$project -m MyApp --mathoptformat $reference`,
    )
    @test read(output, String) == read(reference, String)
    # Manually test the output
    object = JSON.parsefile(output)
    @test object["variables"] == [Dict("name" => "x")]
    @test object["objective"] == Dict(
        "sense" => "min",
        "function" => Dict(
            "type" => "ScalarAffineFunction",
            "terms" => [Dict("coefficient" => 1.0, "variable" => "x")],
            "constant" => 0.0,
        ),
    )
    con_c = Dict(
        "name" => "c",
        "function" => Dict(
            "type" => "ScalarNonlinearFunction",
            "root" => Dict("type" => "node", "index" => 2),
            "node_list" => [
                Dict("type" => "sin", "args" => ["x"]),
                Dict(
                    "type" => "+",
                    "args" => [Dict("type" => "node", "index" => 1), 2, 3.0],
                ),
            ],
        ),
        "set" => Dict("type" => "LessThan", "upper" => 7.0),
    )
    con_bound = Dict(
        "function" => Dict("type" => "Variable", "name" => "x"),
        "set" => Dict("type" => "GreaterThan", "lower" => 0.0),
    )
    @test object["constraints"] == [con_c, con_bound]
    return
end

end # module TestJuliaC

TestJuliaC.runtests()
