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
        "Background" => [
            "background/motivation.md",
            "background/duality.md",
            "background/naming_conventions.md",
        ],
        "Tutorials" => [
            "tutorials/example.md",
            "tutorials/implementing.md",
            "tutorials/mathprogbase.md",
            "tutorials/bridging_constraint.md",
            "tutorials/manipulating_expressions.md",
            "tutorials/latency.md",
        ],
        "Manual" => [
            "manual/standard_form.md",
            "manual/models.md",
            "manual/variables.md",
            "manual/constraints.md",
            "manual/solutions.md",
            "manual/modification.md",
        ],
        "API Reference" => [
            "reference/standard_form.md",
            "reference/models.md",
            "reference/variables.md",
            "reference/constraints.md",
            "reference/modification.md",
            "reference/nonlinear.md",
            "reference/callbacks.md",
            "reference/errors.md",
        ],
        "Submodules" => [
            "Benchmarks" => [
                "Overview" => "submodules/Benchmarks/overview.md",
                "API Reference" => "submodules/Benchmarks/reference.md",
            ],
            "Bridges" => [
                "Overview" => "submodules/Bridges/overview.md",
                "Implementation" => "submodules/Bridges/implementation.md",
                "API Reference" => "submodules/Bridges/reference.md",
            ],
            "FileFormats" => [
                "Overview" => "submodules/FileFormats/overview.md",
                "API Reference" => "submodules/FileFormats/reference.md",
            ],
            "Utilities" => [
                "Overview" => "submodules/Utilities/overview.md",
                "API Reference" => "submodules/Utilities/reference.md",
            ],
            "Test" => [
                "Overview" => "submodules/Test/overview.md",
                "API Reference" => "submodules/Test/reference.md",
            ],
        ],
    ],
)

deploydocs(
    push_preview = true,
    repo = "github.com/jump-dev/MathOptInterface.jl.git",
)
