using Documenter, MathOptFormat

makedocs(
    clean = false,
    format = :html,
    sitename = "MathOptFormat.jl",
    pages = [
        "Manual" => "index.md"
    ],
    assets = ["assets/custom.css"]
)
