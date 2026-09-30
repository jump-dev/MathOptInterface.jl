# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestJuliaC

using Test

import JSON
import JuliaC

function compile(output_dir)
    image_recipe = JuliaC.ImageRecipe(
        output_type = "--output-exe",
        file = joinpath(@__DIR__, "MOFWriter"),
        trim_mode = "no",
        add_ccallables = false,
        verbose = true,
    )
    link_recipe = JuliaC.LinkRecipe(;
        image_recipe,
        outname = joinpath(output_dir, "MOFWriter"),
    )
    bundle_recipe = JuliaC.BundleRecipe(; link_recipe, output_dir)
    JuliaC.compile_products(image_recipe)
    JuliaC.link_products(link_recipe)
    JuliaC.bundle_products(bundle_recipe)
    return
end

@testset "JuliaC MOF writer" begin
    mktempdir() do output_dir
        compile(output_dir)
        app = joinpath(output_dir, "bin", "MOFWriter")
        output = joinpath(output_dir, "compiled.mof.json")
        run(`$app $output`)
        object = JSON.parsefile(output)
        @test object["variables"] == [Dict("name" => "x")]
        @test object["objective"]["sense"] == "min"
        @test object["objective"]["function"] == Dict(
            "type" => "ScalarAffineFunction",
            "terms" => [Dict("coefficient" => 1.0, "variable" => "x")],
            "constant" => 0.0,
        )
        @test object["has_scalar_nonlinear"]
        @test length(object["constraints"]) == 2
        c = only(filter(c -> get(c, "name", "") == "c", object["constraints"]))
        @test c["set"] == Dict("type" => "LessThan", "upper" => 7.0)
        @test c["function"] == Dict(
            "type" => "ScalarNonlinearFunction",
            "root" => Dict("type" => "node", "index" => 2),
            "node_list" => [
                Dict("type" => "sin", "args" => ["x"]),
                Dict(
                    "type" => "+",
                    "args" =>
                        [Dict("type" => "node", "index" => 1), 2, 3.0],
                ),
            ],
        )
        bound = only(filter(c -> !haskey(c, "name"), object["constraints"]))
        @test bound["function"] == Dict("type" => "Variable", "name" => "x")
        @test bound["set"] == Dict("type" => "GreaterThan", "lower" => 0.0)
        reference = joinpath(output_dir, "reference.mof.json")
        project = joinpath(@__DIR__, "MOFWriter")
        run(`$(Base.julia_cmd()) --project=$project -m MOFWriter $reference`)
        @test read(output, String) == read(reference, String)
    end
end

end # module TestJuliaC
