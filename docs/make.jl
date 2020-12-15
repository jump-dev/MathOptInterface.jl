using Documenter, MathOptInterface

"""
Pass `julia docs/make.jl --fix` to rebuild the doctests.
"""
const _FIX = findfirst(isequal("--fix"), ARGS) !== nothing

makedocs(
    sitename = "MathOptInterface",
    format = Documenter.HTML(
        # See https://github.com/JuliaDocs/Documenter.jl/issues/868
        prettyurls = get(ENV, "CI", nothing) == "true",
        mathengine = Documenter.MathJax2(),
        collapselevel = 1,
    ),
    strict = true,
    modules = [MathOptInterface],
    checkdocs = :exports,
    doctest = _FIX ? :fix : true,
    pages = [
        "Introduction" => "index.md",
        "Manual" => [
            "manual/basic_usage.md",
            "manual/advanced_usage.md",
            "manual/implementing.md",
            "manual/Benchmarks.md",
            "manual/Bridges.md",
            "manual/FileFormats.md",
            "manual/Test.md",
            "manual/Utilities.md",
        ],
        "API Reference" => "apireference.md",
    ]
)

deploydocs(
    repo   = "github.com/jump-dev/MathOptInterface.jl.git",
)
