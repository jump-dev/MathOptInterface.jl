using Documenter, MathOptFormat

makedocs(
    clean = false,
    format = :html,
    sitename = "MathOptFormat",
    pages = [
        "MathOptFormat" => "index.md"
    ]
)
