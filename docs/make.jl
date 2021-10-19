import Documenter
import MathOptInterface

# Pass --fix` to rebuild the doctests.
const _FIX = findfirst(isequal("--fix"), ARGS) !== nothing

# A flag to check if we are running in a GitHub action.
const _IS_GITHUB_ACTIONS = get(ENV, "GITHUB_ACTIONS", "false") == "true"

# Pass --pdf to build the PDF. On GitHub actions, we always build the PDF.
const _PDF = findfirst(isequal("--pdf"), ARGS) !== nothing || _IS_GITHUB_ACTIONS

# ==============================================================================
#  Documentation structure
# ==============================================================================

const _PAGES = [
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
    "release_notes.md",
]

# ==============================================================================
#  Build the HTML docs
# ==============================================================================

@time Documenter.makedocs(
    sitename = "MathOptInterface",
    authors = "The JuMP core developers and contributors",
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
    pages = _PAGES,
)

# ==============================================================================
#  Build the LaTeX docs (if needed)
# ==============================================================================

if _PDF
    latex_platform = _IS_GITHUB_ACTIONS ? "docker" : "native"
    @time Documenter.makedocs(
        sitename = "MathOptInterface",
        authors = "The JuMP core developers and contributors",
        format = Documenter.LaTeX(platform = latex_platform),
        build = "latex_build",
        pages = _PAGES,
    )
    # Hack for deploying: copy the pdf (and only the PDF) into the HTML build
    # directory! We don't want to copy everything in `latex_build` because it
    # includes lots of extraneous LaTeX files.
    cp(
        joinpath(@__DIR__, "latex_build", "MathOptInterface.pdf"),
        joinpath(@__DIR__, "build", "MathOptInterface.pdf"),
    )
end

# ==============================================================================
#  Deploy everything in `build`
# ==============================================================================

Documenter.deploydocs(
    repo = "github.com/jump-dev/MathOptInterface.jl.git",
    push_preview = true,
)
