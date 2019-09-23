using Documenter, MathOptInterface

makedocs(
    sitename = "MathOptInterface",
    # See https://github.com/JuliaDocs/Documenter.jl/issues/868
    format = Documenter.HTML(prettyurls = get(ENV, "CI", nothing) == "true"),
    # See https://github.com/JuliaOpt/JuMP.jl/issues/1576
    strict = true,
    pages = [
        "Introduction" => "index.md",
        "Manual" => "apimanual.md",
        "File formats" => "formats.md",
        "Reference" => "apireference.md",
    ]
)

deploydocs(
    repo   = "github.com/JuliaOpt/MathOptInterface.jl.git",
)
