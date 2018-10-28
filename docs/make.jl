using Documenter, MathOptInterface

makedocs(
    format = :html,
    sitename = "MathOptInterface",
    pages = [
        "Introduction" => "index.md",
        "Manual" => "apimanual.md",
        "Reference" => "apireference.md",
    ]
)

deploydocs(
    repo   = "github.com/JuliaOpt/MathOptInterface.jl.git",
    target = "build",
    osname = "linux",
    julia  = "1.0",
    deps   = nothing,
    make   = nothing,
)
