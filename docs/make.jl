using Documenter, MathOptInterface

makedocs(
    sitename = "MathOptInterface",
    format = Documenter.HTML(
        # See https://github.com/JuliaDocs/Documenter.jl/issues/868
        prettyurls = get(ENV, "CI", nothing) == "true",
        mathengine = Documenter.MathJax(),
        collapselevel = 1,
    ),
    # See https://github.com/jump-dev/JuMP.jl/issues/1576
    strict = true,
    pages = [
        "Introduction" => "index.md",
        "Manual" => [
            "manual/manual.md",
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
