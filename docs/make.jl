using Documenter, MathOptInterface

makedocs(
    sitename = "MathOptInterface",
    html_prettyurls = get(ENV, "CI", nothing) == "true",
    pages = [
        "Introduction" => "index.md",
        "Manual" => "apimanual.md",
        "Reference" => "apireference.md",
    ]
)

deploydocs(
    repo   = "github.com/JuliaOpt/MathOptInterface.jl.git",
)
