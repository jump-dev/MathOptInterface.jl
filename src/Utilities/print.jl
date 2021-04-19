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

function _to_string(::MIME, model::MOI.ModelLike, v::MOI.VariableIndex)
    var_name = MOI.get(model, MOI.VariableName(), v)
    return isempty(var_name) ? "noname" : var_name
end

function _to_string(::MIME"text/latex", model::MOI.ModelLike, v::MOI.VariableIndex)
    var_name = MOI.get(model, MOI.VariableName(), v)
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

function _to_string(mime::MIME, model::MOI.ModelLike, f::MOI.SingleVariable)
    return _to_string(mime, model, f.variable)
end

function _to_string(mime::MIME, model::MOI.ModelLike, term::MOI.ScalarAffineTerm)
    name = _to_string(mime, model, term.variable_index)
    if term.coefficient < 0
        return string(" - ", string(-term.coefficient), " ", name)
    else
        return string(" + ", string(term.coefficient), " ", name)
    end
end

function _to_string(mime::MIME, model::MOI.ModelLike, f::MOI.ScalarAffineFunction)
    s = string(f.constant)
    for term in f.terms
        s *= _to_string(mime, model, term)
    end
    return s
end

function _to_string(mime::MIME, model::MOI.ModelLike, term::MOI.ScalarQuadraticTerm)
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

function _to_string(mime::MIME, model::MOI.ModelLike, f::MOI.ScalarQuadraticFunction)
    s = string(f.constant)
    for term in f.affine_terms
        s *= _to_string(mime, model, term)
    end
    for term in f.quadratic_terms
        s *= _to_string(mime, model, term)
    end
    return s
end

function _to_string(mime::MIME, model::MOI.ModelLike, f::MOI.AbstractVectorFunction)
    rows = map(fi -> _to_string(mime, model, fi), eachscalar(f))
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
    model::MOI.ModelLike,
    f::MOI.AbstractVectorFunction,
)
    return string(
        "\\begin{bmatrix}\n",
        join(map(fi -> _to_string(mime, model, fi), eachscalar(f)), "\\\\\n"),
        "\\end{bmatrix}",
    )
end

#------------------------------------------------------------------------
# Sets
#------------------------------------------------------------------------

function _to_string(mime::MIME, set::MOI.LessThan)
    return string(_to_string(mime, :leq), " ", set.upper)
end

_to_string(::MIME"text/latex", set::MOI.LessThan) = "\\le $(set.upper)"

function _to_string(mime::MIME, set::MOI.GreaterThan)
    return string(_to_string(mime, :geq), " ", set.lower)
end

_to_string(::MIME"text/latex", set::MOI.GreaterThan) = "\\ge $(set.lower)"

function _to_string(mime::MIME, set::MOI.EqualTo)
    return string(_to_string(mime, :eq), " ", set.value)
end

_to_string(::MIME"text/latex", set::MOI.EqualTo) = "= $(set.value)"

function _to_string(mime::MIME, set::MOI.Interval)
    return string(_to_string(mime, :in), " [", set.lower, ", ", set.upper, "]")
end

function _to_string(::MIME"text/latex", set::MOI.Interval)
    return "\\in \\[$(set.lower), $(set.upper)\\]"
end

_to_string(mime::MIME, ::MOI.ZeroOne) = string(_to_string(mime, :in), " {0, 1}")

_to_string(::MIME"text/latex", ::MOI.ZeroOne) = "\\in \\{0, 1\\}"

_to_string(mime::MIME, ::MOI.Integer) = string(_to_string(mime, :in), " ℤ")

_to_string(::MIME"text/latex", ::MOI.Integer) = "\\in \\mathbb{Z}"

function _to_string(mime::MIME, set::MOI.AbstractSet)
    return string(_to_string(mime, :in), " ", _drop_moi(set))
end

function _to_string(::MIME"text/latex", set::MOI.AbstractSet)
    set_str = replace(replace(_drop_moi(set), "{" => "\\{"), "}" => "\\}")
    return string("\\in \\text{", set_str, "}")
end

#------------------------------------------------------------------------
# Constraints
#------------------------------------------------------------------------

function _to_string(mime::MIME, model::MOI.ModelLike, cref::MOI.ConstraintIndex)
    f = MOI.get(model, MOI.ConstraintFunction(), cref)
    s = MOI.get(model, MOI.ConstraintSet(), cref)
    return string(_to_string(mime, model, f), " ", _to_string(mime, s))
end

#------------------------------------------------------------------------
# MOI.ModelLike
#------------------------------------------------------------------------

"""
    _print_model(io::IO, mime::MIME"text/plain", model::MOI.ModelLike)

Print a plain-text formulation of `model` to `io`.
"""
function _print_model(io::IO, mime::MIME"text/plain", model::MOI.ModelLike)
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.MAX_SENSE
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        println(io, "Maximize $(_drop_moi(F)):")
        f = MOI.get(model, MOI.ObjectiveFunction{F}())
        println(io, " ", _to_string(mime, model, f))
    elseif sense == MOI.MIN_SENSE
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        println(io, "Minimize $(_drop_moi(F)):")
        f = MOI.get(model, MOI.ObjectiveFunction{F}())
        println(io, " ", _to_string(mime, model, f))
    else
        println(io, "Feasibility")
    end
    println(io, "\nSubject to:")
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        println(io, "\n$(_drop_moi(F))-in-$(_drop_moi(S))")
        for cref in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            s = _to_string(mime, model, cref)
            println(io, " ", replace(s, '\n' => "\n "))
        end
    end
    return
end

"""
    _print_model(io::IO, mime::MIME"text/latex", model::MOI.ModelLike)

Print a LaTeX formulation of `model` to `io`.
"""
function _print_model(io::IO, mime::MIME"text/latex", model::MOI.ModelLike)
    println(io, "\$\$ \\begin{aligned}")
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.MAX_SENSE
        print(io, "\\max\\quad & ")
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        f = MOI.get(model, MOI.ObjectiveFunction{F}())
        println(io, _to_string(mime, model, f), " \\\\")
    elseif sense == MOI.MIN_SENSE
        print(io, "\\min\\quad & ")
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        f = MOI.get(model, MOI.ObjectiveFunction{F}())
        println(io, _to_string(mime, model, f), " \\\\")
    else
        println(io, "\\text{feasibility}\\\\")
    end
    println(io, "\\text{Subject to}\\\\")
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        println(io, " & \\text{$(_drop_moi(F))-in-$(_drop_moi(S))} \\\\")
        for cref in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            println(io, " & ", _to_string(mime, model, cref), " \\\\")
        end
    end
    return print(io, "\\end{aligned} \$\$")
end

#------------------------------------------------------------------------
# Latex
#------------------------------------------------------------------------

struct _LatexModel{T<:MOI.ModelLike}
    model::T
end

"""
    latex_formulation(model::MOI.ModelLike)

Wrap `model` in a type so that it can be pretty-printed as `text/latex` in a
notebook like IJulia, or in Documenter.

To render the model, end the cell with `latex_formulation(model)`, or call
`display(latex_formulation(model))` in to force the display of the model from
inside a function.
"""
latex_formulation(model::MOI.ModelLike) = _LatexModel(model)

function Base.show(io::IO, model::_LatexModel)
    return _print_model(io, MIME("text/latex"), model.model)
end

Base.show(io::IO, ::MIME"text/latex", model::_LatexModel) = show(io, model)

function Base.print(model::MOI.ModelLike)
    for d in Base.Multimedia.displays
        if Base.Multimedia.displayable(d, "text/latex") &&
           startswith("$(typeof(d))", "IJulia.")
            return display(d, "text/latex", latex_formulation(model))
        end
    end
    return _print_model(stdout, MIME("text/plain"), model)
end

function Base.print(io::IO, model::MOI.ModelLike)
    return _print_model(io, MIME("text/plain"), model)
end
