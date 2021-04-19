using Printf

_drop_moi(s) = replace(string(s), "MathOptInterface." => "")

#------------------------------------------------------------------------
# Math Symbols
#------------------------------------------------------------------------

# REPL-specific symbols
# Anything here: https://en.wikipedia.org/wiki/Windows-1252
# should probably work fine on Windows
function _to_string(::MIME, name::Symbol)
    if name == :leq
        return "<="
    elseif name == :geq
        return ">="
    elseif name == :eq
        return "=="
    elseif name == :times
        return "*"
    elseif name == :sq
        return "²"
    elseif name == :in
        return Sys.iswindows() ? "in" : "∈"
    end
end

function _to_string(::MIME"text/latex", name::Symbol)
    if name == :leq
        return "\\leq"
    elseif name == :geq
        return "\\geq"
    elseif name == :eq
        return "="
    elseif name == :times
        return "\\times "
    elseif name == :sq
        return "^2"
    elseif name == :in
        return "\\in"
    end
end

#------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------

function _to_string(::MIME, model::ModelLike, v::VariableIndex)
    var_name = get(model, VariableName(), v)
    return isempty(var_name) ? "noname" : var_name
end

function _to_string(::MIME"text/latex", model::ModelLike, v::VariableIndex)
    var_name = get(model, VariableName(), v)
    if isempty(var_name)
        return "noname"
    end
    # We need to escape latex math characters that appear in the name.
    # However, it's probably impractical to catch everything, so let's just
    # escape the common ones:
    # Escape underscores to prevent them being treated as subscript markers.
    var_name = replace(var_name, "_" => "\\_")
    # Escape carets to prevent them being treated as superscript markers.
    var_name = replace(var_name, "^" => "\\^")
    # Convert any x[args] to x_{args} so that indices on x print as subscripts.
    m = match(r"^(.*)\[(.+)\]$", var_name)
    if m !== nothing
        var_name = m[1] * "_{" * m[2] * "}"
    end
    return var_name
end

function _to_string(mime, model::ModelLike, f::SingleVariable)
    return _to_string(mime, model, f.variable)
end

function _to_string(mime, model::ModelLike, term::ScalarAffineTerm)
    name = _to_string(mime, model, term.variable_index)
    if term.coefficient < 0
        return string(" - ", string(-term.coefficient), " ", name)
    else
        return string(" + ", string(term.coefficient), " ", name)
    end
end

function _to_string(mime, model::ModelLike, f::ScalarAffineFunction)
    s = string(f.constant)
    for term in f.terms
        s *= _to_string(mime, model, term)
    end
    return s
end

function _to_string(mime, model::ModelLike, term::ScalarQuadraticTerm)
    name_1 = _to_string(mime, model, term.variable_index_1)
    name_2 = _to_string(mime, model, term.variable_index_2)
    name = if term.variable_index_1 == term.variable_index_2
        string(" ", name_1, _to_string(mime, :sq))
    else
        string(" ", name_1, _to_string(mime, :times), name_2)
    end
    if term.coefficient < 0
        return string(" - ", string(-term.coefficient), name)
    else
        return string(" + ", string(term.coefficient), name)
    end
end

function _to_string(mime, model::ModelLike, f::ScalarQuadraticFunction)
    s = string(f.constant)
    for term in f.affine_terms
        s *= _to_string(mime, model, term)
    end
    for term in f.quadratic_terms
        s *= _to_string(mime, model, term)
    end
    return s
end

function _to_string(mime, model::ModelLike, f::AbstractVectorFunction)
    rows = map(fi -> _to_string(mime, model, fi), Utilities.eachscalar(f))
    max_length = maximum(length.(rows))
    s = join(map(r -> string("│", rpad(r, max_length), "│"), rows), '\n')
    return string(
        "┌",
        rpad("", max_length),
        "┐\n",
        s,
        "\n└",
        rpad("", max_length),
        "┘",
    )
end

function _to_string(
    mime::MIME"text/latex",
    model::ModelLike,
    f::AbstractVectorFunction,
)
    return string(
        "\\begin{bmatrix}\n",
        join(
            map(fi -> _to_string(mime, model, fi), Utilities.eachscalar(f)),
            "\\\\\n",
        ),
        "\\end{bmatrix}",
    )
end

#------------------------------------------------------------------------
# Sets
#------------------------------------------------------------------------

function _to_string(mime, set::LessThan)
    return string(_to_string(mime, :leq), " ", set.upper)
end

_to_string(::MIME"text/latex", set::LessThan) = "\\le $(set.upper)"

function _to_string(mime, set::GreaterThan)
    return string(_to_string(mime, :geq), " ", set.lower)
end

_to_string(::MIME"text/latex", set::GreaterThan) = "\\ge $(set.lower)"

function _to_string(mime, set::EqualTo)
    return string(_to_string(mime, :eq), " ", set.value)
end

_to_string(::MIME"text/latex", set::EqualTo) = "= $(set.value)"

function _to_string(mime, set::Interval)
    return string(_to_string(mime, :in), " [", set.lower, ", ", set.upper, "]")
end

function _to_string(::MIME"text/latex", set::Interval)
    return "\\in \\[$(set.lower), $(set.upper)\\]"
end

_to_string(::MIME, ::ZeroOne) = "∈ {0, 1}"

_to_string(::MIME"text/latex", ::ZeroOne) = "\\in \\{0, 1\\}"

_to_string(::MIME, ::Integer) = "∈ ℤ"

_to_string(::MIME"text/latex", ::Integer) = "\\in \\mathbb{Z}"

function _to_string(mime, set::AbstractSet)
    return string(_to_string(mime, :in), " ", _drop_moi(set))
end

function _to_string(::MIME"text/latex", set::AbstractSet)
    set_str = replace(replace(_drop_moi(set), "{" => "\\{"), "}" => "\\}")
    return string("\\in \\text{", set_str, "}")
end

#------------------------------------------------------------------------
# Constraints
#------------------------------------------------------------------------

function _to_string(mime::MIME, model::ModelLike, cref::ConstraintIndex)
    f = get(model, ConstraintFunction(), cref)
    s = get(model, ConstraintSet(), cref)
    return string(_to_string(mime, model, f), " ", _to_string(mime, s))
end

#------------------------------------------------------------------------
# ModelLike
#------------------------------------------------------------------------

"""
    _print_model(io::IO, model::ModelLike, mime::MIME"text/plain")

Print a plain-text formulation of `model` to `io`.
"""
function _print_model(io::IO, model::ModelLike, mime::MIME"text/plain")
    sense = get(model, ObjectiveSense())
    if sense == MAX_SENSE
        F = get(model, ObjectiveFunctionType())
        println(io, "Maximize $(_drop_moi(F)):")
        f = get(model, ObjectiveFunction{F}())
        println(io, " ", _to_string(mime, model, f))
    elseif sense == MIN_SENSE
        F = get(model, ObjectiveFunctionType())
        println(io, "Minimize $(_drop_moi(F)):")
        f = get(model, ObjectiveFunction{F}())
        println(io, " ", _to_string(mime, model, f))
    else
        println(io, "Feasibility")
    end
    println(io, "\nSubject to:")
    for (F, S) in get(model, ListOfConstraints())
        println(io, "\n$(_drop_moi(F))-in-$(_drop_moi(S))")
        for cref in get(model, ListOfConstraintIndices{F,S}())
            s = _to_string(mime, model, cref)
            println(io, " ", replace(s, '\n' => "\n "))
        end
    end
    return
end

"""
    _print_model(io::IO, model::ModelLike, mime::MIME"text/latex")

Print a LaTeX formulation of `model` to `io`.
"""
function _print_model(io::IO, model::ModelLike, mime::MIME"text/latex")
    println(io, "\$\$ \\begin{aligned}")
    sense = get(model, ObjectiveSense())
    if sense == MAX_SENSE
        print(io, "\\max\\quad & ")
        F = get(model, ObjectiveFunctionType())
        f = get(model, ObjectiveFunction{F}())
        println(io, _to_string(mime, model, f), "\\\\")
    elseif sense == MIN_SENSE
        print(io, "\\min\\quad & ")
        F = get(model, ObjectiveFunctionType())
        f = get(model, ObjectiveFunction{F}())
        println(io, _to_string(mime, model, f), "\\\\")
    else
        println(io, "\\text{feasibility}\\\\")
    end
    print(io, "\\text{Subject to} \\quad")
    for (F, S) in get(model, ListOfConstraints())
        println(io, "$F-in-$S")
        for cref in get(model, ListOfConstraintIndices{F,S}())
            println(io, " & ", _to_string(mime, model, cref), "\\\\")
        end
    end
    return print(io, "\\end{aligned} \$\$")
end

#------------------------------------------------------------------------
# Latex
#------------------------------------------------------------------------

struct _LatexModel{T<:ModelLike}
    model::T
end

"""
    latex_formulation(model::ModelLike)

Wrap `model` in a type so that it can be pretty-printed as `text/latex` in a
notebook like IJulia, or in Documenter.

To render the model, end the cell with `latex_formulation(model)`, or call
`display(latex_formulation(model))` in to force the display of the model from
inside a function.
"""
latex_formulation(model::ModelLike) = _LatexModel(model)

function Base.show(io::IO, model::_LatexModel)
    return _print_model(io, model.model, MIME("text/latex"))
end

Base.show(io::IO, ::MIME"text/latex", model::_LatexModel) = show(io, model)

function Base.print(model::ModelLike)
    for d in Base.Multimedia.displays
        if Base.Multimedia.displayable(d, "text/latex") &&
           startswith("$(typeof(d))", "IJulia.")
            return display(d, "text/latex", latex_formulation(model))
        end
    end
    return _print_model(stdout, model, MIME("text/plain"))
end

function Base.print(io::IO, model::ModelLike)
    return _print_model(io, model, MIME("text/plain"))
end
