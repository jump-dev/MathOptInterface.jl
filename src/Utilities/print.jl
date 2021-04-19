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

function _to_string(
    ::MIME"text/latex",
    model::MOI.ModelLike,
    v::MOI.VariableIndex,
)
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

function _to_string(
    mime::MIME,
    model::MOI.ModelLike,
    term::MOI.ScalarAffineTerm,
)
    name = _to_string(mime, model, term.variable_index)
    if term.coefficient < 0
        return string(" - ", string(-term.coefficient), " ", name)
    else
        return string(" + ", string(term.coefficient), " ", name)
    end
end

function _to_string(
    mime::MIME,
    model::MOI.ModelLike,
    f::MOI.ScalarAffineFunction,
)
    s = string(f.constant)
    for term in f.terms
        s *= _to_string(mime, model, term)
    end
    return s
end

function _to_string(
    mime::MIME,
    model::MOI.ModelLike,
    term::MOI.ScalarQuadraticTerm,
)
    name_1 = _to_string(mime, model, term.variable_index_1)
    name_2 = _to_string(mime, model, term.variable_index_2)
    # Be careful here when printing the coefficient. ScalarQuadraticFunction
    # assumes an additional 0.5 factor!
    coef = term.coefficient
    name = if term.variable_index_1 == term.variable_index_2
        coef /= 2
        string(" ", name_1, _to_string(mime, :sq))
    else
        string(" ", name_1, _to_string(mime, :times), name_2)
    end
    if coef < 0
        return string(" - ", string(-coef), name)
    else
        return string(" + ", string(coef), name)
    end
end

function _to_string(
    mime::MIME,
    model::MOI.ModelLike,
    f::MOI.ScalarQuadraticFunction,
)
    s = string(f.constant)
    for term in f.affine_terms
        s *= _to_string(mime, model, term)
    end
    for term in f.quadratic_terms
        s *= _to_string(mime, model, term)
    end
    return s
end

function _to_string(
    mime::MIME,
    model::MOI.ModelLike,
    f::MOI.AbstractVectorFunction,
)
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
# Nonlinear constraints
#------------------------------------------------------------------------

"""
    _VariableNode

A type used to work-around the default printing of Julia expressions. If we
subsititued the variable names into the expression and the converted to a
string, each variable would be printed with enclosing `"`.

To work-around this, create a new type and overload `show`.
"""
struct _VariableNode
    x::String
end
Base.show(io::IO, x::_VariableNode) = print(io, x.x)

_replace_names(::MIME, ::MOI.ModelLike, x, ::Any) = x
function _replace_names(mime::MIME, model::MOI.ModelLike, x::Expr, lookup)
    if x.head == :ref
        return get!(lookup, x.args[2]) do
            return _VariableNode(_to_string(mime, model, x.args[2]))
        end
    else
        for (i, arg) in enumerate(x.args)
            x.args[i] = _replace_names(mime, model, arg, lookup)
        end
    end
    return x
end

function _replace_nonlinear_latex(::MIME"text/latex", s::String)
    s = replace(s, " * " => " \\times ")
    s = replace(s, " >= " => " \\ge ")
    s = replace(s, " <= " => " \\le ")
    s = replace(s, " == " => " = ")
    return s
end
_replace_nonlinear_latex(::MIME, s::String) = s

function _print_nonlinear_constraints(
    io::IO,
    mime::MIME"text/plain",
    model::MOI.ModelLike,
    block::MOI.NLPBlockData,
)
    lookup = Dict{MOI.VariableIndex,_VariableNode}()
    println(io, "\nNonlinear")
    has_expr = :ExprGraph in MOI.features_available(block.evaluator)
    for (i, bound) in enumerate(block.constraint_bounds)
        if has_expr
            ex = MOI.constraint_expr(block.evaluator, i)
            println(io, " ", _replace_names(mime, model, ex, lookup))
        else
            println(io, " ", bound.lower, " <= g_$(i)(x) <= ", bound.upper)
        end
    end
end

function _print_nonlinear_constraints(
    io::IO,
    mime::MIME"text/latex",
    model::MOI.ModelLike,
    block::MOI.NLPBlockData,
)
    lookup = Dict{MOI.VariableIndex,_VariableNode}()
    println(io, " & \\text{Nonlinear} \\\\")
    has_expr = :ExprGraph in MOI.features_available(block.evaluator)
    for (i, bound) in enumerate(block.constraint_bounds)
        if has_expr
            ex = MOI.constraint_expr(block.evaluator, i)
            nl_c = string(_replace_names(mime, model, ex, lookup))
            println(io, " & ", _replace_nonlinear_latex(mime, nl_c), " \\\\")
        else
            println(
                io,
                "& ",
                bound.lower,
                " \\le g_$(i)(x) \\le ",
                bound.upper,
                " \\\\",
            )
        end
    end
end

_print_nonlinear_constraints(::IO, ::MIME, ::MOI.ModelLike, ::Nothing) = nothing

#------------------------------------------------------------------------
# ObjectiveFunction
#------------------------------------------------------------------------

function _objective_function_string(mime::MIME, model::MOI.ModelLike, ::Nothing)
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    return _drop_moi(F), _to_string(mime, model, f)
end

function _objective_function_string(
    mime::MIME,
    model::MOI.ModelLike,
    block::MOI.NLPBlockData,
)
    lookup = Dict{MOI.VariableIndex,_VariableNode}()
    if block.has_objective
        f = "f(x)"
        if :ExprGraph in MOI.features_available(block.evaluator)
            ex = MOI.objective_expr(block.evaluator)
            f = string(_replace_names(mime, model, ex, lookup))
            f = _replace_nonlinear_latex(mime, f)
        end
        return "Nonlinear", f
    else
        return _objective_function_string(mime, model, nothing)
    end
end

#------------------------------------------------------------------------
# MOI.ModelLike
#------------------------------------------------------------------------

function _nlp_block(model::MOI.ModelLike)
    try
        block = MOI.get(model, MOI.NLPBlock())
        if :ExprGraph in MOI.features_available(block.evaluator)
            MOI.initialize(block.evaluator, [:ExprGraph])
        end
        return block
    catch
        return nothing
    end
end

"""
    _print_model(io::IO, mime::MIME"text/plain", model::MOI.ModelLike)

Print a plain-text formulation of `model` to `io`.
"""
function _print_model(io::IO, mime::MIME"text/plain", model::MOI.ModelLike)
    nlp_block = _nlp_block(model)
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        println(io, "Feasibility")
    else
        F, f = _objective_function_string(mime, model, nlp_block)
        sense_s = sense == MOI.MIN_SENSE ? "Minimize" : "Maximize"
        println(io, sense_s, " ", F, ":\n ", f)
    end
    println(io, "\nSubject to:")
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        println(io, "\n$(_drop_moi(F))-in-$(_drop_moi(S))")
        for cref in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            s = _to_string(mime, model, cref)
            println(io, " ", replace(s, '\n' => "\n "))
        end
    end
    _print_nonlinear_constraints(io, mime, model, nlp_block)
    return
end

"""
    _print_model(io::IO, mime::MIME"text/latex", model::MOI.ModelLike)

Print a LaTeX formulation of `model` to `io`.
"""
function _print_model(io::IO, mime::MIME"text/latex", model::MOI.ModelLike)
    nlp_block = _nlp_block(model)
    println(io, "\$\$ \\begin{aligned}")
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        println(io, "\\text{feasibility}\\\\")
    else
        F, f = _objective_function_string(mime, model, nlp_block)
        sense_s = sense == MOI.MIN_SENSE ? "min" : "max"
        println(io, "\\", sense_s, "\\quad & ", f, " \\\\")
    end
    println(io, "\\text{Subject to}\\\\")
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        println(io, " & \\text{$(_drop_moi(F))-in-$(_drop_moi(S))} \\\\")
        for cref in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            println(io, " & ", _to_string(mime, model, cref), " \\\\")
        end
    end
    _print_nonlinear_constraints(io, mime, model, nlp_block)
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
