using Documenter, MathOptFormat

makedocs(
    clean = false,
    format = :html,
    sitename = "MathOptFormat",
    pages = [
        "MathOptFormat" => "index.md"
    ]
)

deploydocs(
    repo   = "github.com/odow/MathOptFormat.jl.git",
    target = "build",
    osname = "linux",
    julia  = "1.0",
    deps   = nothing,
    make   = nothing
)
