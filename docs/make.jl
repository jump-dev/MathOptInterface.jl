# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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
    "Introduction" => ["index.md", "background/motivation.md"],
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
    "Background" => [
        "background/duality.md",
        "background/infeasibility_certificates.md",
        "background/naming_conventions.md",
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
            "List of bridges" => "submodules/Bridges/list_of_bridges.md",
            "API Reference" => "submodules/Bridges/reference.md",
        ],
        "FileFormats" => [
            "Overview" => "submodules/FileFormats/overview.md",
            "API Reference" => "submodules/FileFormats/reference.md",
            "The LP file format" => "submodules/FileFormats/LP.md",
        ],
        "Nonlinear" => [
            "Overview" => "submodules/Nonlinear/overview.md",
            "SymbolicAD" => "submodules/Nonlinear/SymbolicAD.md",
            "API Reference" => "submodules/Nonlinear/reference.md",
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
    "Developer Docs" =>
        ["developer/checklists.md", "developer/contributing.md"],
    "Release notes" => "release_notes.md",
]

# ==============================================================================
#  Modify the release notes
# ==============================================================================

function fix_release_line(
    line::String,
    url::String = "https://github.com/jump-dev/MathOptInterface.jl",
)
    # (#XXXX) -> ([#XXXX](url/issue/XXXX))
    while (m = match(r"\(\#([0-9]+)\)", line)) !== nothing
        id = m.captures[1]
        line = replace(line, m.match => "([#$id]($url/issues/$id))")
    end
    # ## vX.Y.Z -> [vX.Y.Z](url/releases/tag/vX.Y.Z)
    while (m = match(r"\#\# (v[0-9]+.[0-9]+.[0-9]+)", line)) !== nothing
        tag = m.captures[1]
        line = replace(line, m.match => "## [$tag]($url/releases/tag/$tag)")
    end
    return line
end

open(joinpath(@__DIR__, "src", "changelog.md"), "r") do in_io
    open(joinpath(@__DIR__, "src", "release_notes.md"), "w") do out_io
        for line in readlines(in_io; keep = true)
            write(out_io, fix_release_line(line))
        end
    end
end

# ==============================================================================
#  Build the HTML docs
# ==============================================================================

# Needed to make Documenter think that there is a PDF in the right place when
# link checking. Inn production we replace this by running the LaTeX build.
write(joinpath(@__DIR__, "src", "MathOptInterface.pdf"), "")

Documenter.DocMeta.setdocmeta!(
    MathOptInterface,
    :DocTestSetup,
    :(import MathOptInterface as MOI);
    recursive = true,
)

@time Documenter.makedocs(
    sitename = "MathOptInterface",
    authors = "The JuMP core developers and contributors",
    format = Documenter.HTML(
        # See https://github.com/JuliaDocs/Documenter.jl/issues/868
        prettyurls = get(ENV, "CI", nothing) == "true",
        mathengine = Documenter.MathJax2(),
        collapselevel = 1,
        # Do no check for large pages.
        size_threshold_ignore = [
            "changelog.md",
            "release_notes.md",
            "reference/models.md",
            "reference/standard_form.md",
            "submodules/Bridges/list_of_bridges.md",
            "submodules/Bridges/reference.md",
            "submodules/Utilities/reference.md",
        ],
    ),
    clean = true,
    linkcheck = true,
    linkcheck_ignore = [
        # Ignore the PDF link, because it hasn't been built yet.
        "MathOptInterface.pdf",
        # Ignore the very many GitHub links
        r"https://github.com/jump-dev/.+",
        "https://arxiv.org/abs/2002.03447",
        # https://github.com/JuliaDocs/Documenter.jl/issues/2834
        "https://lpsolve.sourceforge.net/5.5/CPLEX-format.htm",
    ],
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
    # Remove the Release Notes; we don't need them in the PDF.
    pop!(_PAGES)
    @time Documenter.makedocs(
        sitename = "MathOptInterface",
        authors = "The JuMP core developers and contributors",
        format = Documenter.LaTeX(platform = latex_platform),
        build = "latex_build",
        pages = _PAGES,
    )
    # Hack for deploying: copy the PDF (and only the PDF) into the HTML build
    # directory. We don't want to copy everything in `latex_build` because it
    # includes lots of extraneous LaTeX files.
    cp(
        joinpath(@__DIR__, "latex_build", "MathOptInterface.pdf"),
        joinpath(@__DIR__, "build", "MathOptInterface.pdf");
        force = true,
    )
end

# ==============================================================================
#  Deploy everything in `build`
# ==============================================================================

Documenter.deploydocs(
    repo = "github.com/jump-dev/MathOptInterface.jl.git",
    push_preview = true,
)
