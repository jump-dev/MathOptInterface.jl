module TestPrint

using MathOptInterface
const MOI = MathOptInterface

using Test

const LATEX = MIME("text/latex")
const PLAIN = MIME("text/plain")

function test_nonname_variable()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    @test MOI._to_string(PLAIN, model, x) == "noname"
    @test MOI._to_string(LATEX, model, x) == "noname"
end

function test_variable()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    for (name, latex_name) in
        [("x", "x"), ("x_y", "x\\_y"), ("x^2", "x\\^2"), ("x[a,b]", "x_{a,b}")]
        MOI.set(model, MOI.VariableName(), x, name)
        @test MOI._to_string(PLAIN, model, x) == name
        @test MOI._to_string(LATEX, model, x) == latex_name
    end
end

function test_single_variable()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    f = MOI.SingleVariable(x)
    @test MOI._to_string(PLAIN, model, f) == "x"
    @test MOI._to_string(LATEX, model, f) == "x"
end

function test_ScalarAffineTerm()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    @test MOI._to_string(PLAIN, model, MOI.ScalarAffineTerm(-1.2, x)) ==
          " - 1.2 x"
    @test MOI._to_string(LATEX, model, MOI.ScalarAffineTerm(1.2, x)) ==
          " + 1.2 x"
end

function test_ScalarAffineFunction()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    f = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([-1.2, 1.3], [x, x]),
        1.4,
    )
    @test MOI._to_string(PLAIN, model, f) == "1.4 - 1.2 x + 1.3 x"
    @test MOI._to_string(LATEX, model, f) == "1.4 - 1.2 x + 1.3 x"
end

function test_ScalarQuadraticTerm()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    term = MOI.ScalarQuadraticTerm(-1.2, x, x)
    @test MOI._to_string(PLAIN, model, term) == " - 1.2 x²"
    @test MOI._to_string(LATEX, model, term) == " - 1.2 x^2"
    term = MOI.ScalarQuadraticTerm(1.2, x, y)
    @test MOI._to_string(PLAIN, model, term) == " + 1.2 x*y"
    @test MOI._to_string(LATEX, model, term) == " + 1.2 x\\times y"
end

function test_ScalarQuadraticFunction()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    f = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm.([-1.2, 1.3], [x, x]),
        MOI.ScalarQuadraticTerm.([0.5, 0.6], [x, x], [x, y]),
        1.4,
    )
    @test MOI._to_string(PLAIN, model, f) ==
          "1.4 - 1.2 x + 1.3 x + 0.5 x² + 0.6 x*y"
    @test MOI._to_string(LATEX, model, f) ==
          "1.4 - 1.2 x + 1.3 x + 0.5 x^2 + 0.6 x\\times y"
end

function test_VectorOfVariables()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    f = MOI.VectorOfVariables([x, y])
    @test MOI._to_string(PLAIN, model, f) == "┌ ┐\n│x│\n│y│\n└ ┘"
    @test MOI._to_string(LATEX, model, f) ==
          "\\begin{bmatrix}\nx\\\\\ny\\end{bmatrix}"
end

function test_LessThan()
    s = MOI.LessThan(1.2)
    @test MOI._to_string(PLAIN, s) == "<= 1.2"
    @test MOI._to_string(LATEX, s) == "\\le 1.2"
end

function test_GreaterThan()
    s = MOI.GreaterThan(1.2)
    @test MOI._to_string(PLAIN, s) == ">= 1.2"
    @test MOI._to_string(LATEX, s) == "\\ge 1.2"
end

function test_EqualTo()
    s = MOI.EqualTo(1.2)
    @test MOI._to_string(PLAIN, s) == "== 1.2"
    @test MOI._to_string(LATEX, s) == "= 1.2"
end

function test_Interval()
    s = MOI.Interval(1.2, 1.3)
    @test MOI._to_string(PLAIN, s) == "∈ [1.2, 1.3]"
    @test MOI._to_string(LATEX, s) == "\\in \\[1.2, 1.3\\]"
end

function test_ZeroOne()
    s = MOI.ZeroOne()
    @test MOI._to_string(PLAIN, s) == "∈ {0, 1}"
    @test MOI._to_string(LATEX, s) == "\\in \\{0, 1\\}"
end

function test_Integer()
    s = MOI.Integer()
    @test MOI._to_string(PLAIN, s) == "∈ ℤ"
    @test MOI._to_string(LATEX, s) == "\\in \\mathbb{Z}"
end

function test_ExponentialCone()
    s = MOI.ExponentialCone()
    @test MOI._to_string(PLAIN, s) == "∈ ExponentialCone()"
    @test MOI._to_string(LATEX, s) == "\\in \\text{ExponentialCone()}"
end

function test_feasibility()
    model = MOI.Utilities.Model{Float64}()
    @test sprint(print, model) == "Feasibility\n\nSubject to:\n"
    @test sprint(print, MOI.latex_formulation(model)) == raw"""
    $$ \begin{aligned}
    \text{feasibility}\\
    \text{Subject to} \quad\end{aligned} $$"""
end

function test_min()
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, "variables: x\nminobjective: x")
    @test sprint(print, model) == """
    Minimize SingleVariable:
     x

    Subject to:
    """
    @test sprint(print, MOI.latex_formulation(model)) == raw"""
    $$ \begin{aligned}
    \min\quad & x\\
    \text{Subject to} \quad\end{aligned} $$"""
end

function test_max()
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, "variables: x\nmaxobjective: x")
    @test sprint(print, model) == """
    Maximize SingleVariable:
     x

    Subject to:
    """
    @test sprint(print, MOI.latex_formulation(model)) == raw"""
    $$ \begin{aligned}
    \max\quad & x\\
    \text{Subject to} \quad\end{aligned} $$"""
end

function test_model()
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, y, z
        minobjective: x + 2 + 3.1*y + -1.2*z
        c1: x >= 0.1
        c2: y in ZeroOne()
        c2: z in Integer()
        c3: [x, y] in SecondOrderCone(2)
        c4: [1, x, y] in SecondOrderCone(2)
        c4: [1.0 * x * x, y, 1] in ExponentialCone()
        c4: [1, 1.0 * x * x, y] in ExponentialCone()
        c2: x in ZeroOne()
        c5: 2.0 * x * x + y + -1 * z <= 1.0
        c5: x + x >= 1.0
        c5: x + x in Interval(1.0, 2.0)
        c5: x + -1 * y == 0.0
        """,
    )
    @test sprint(print, model) == """
    Minimize ScalarAffineFunction{Float64}:
     2.0 + 1.0 x + 3.1 y - 1.2 z

    Subject to:

    ScalarAffineFunction{Float64}-in-EqualTo{Float64}
     0.0 + 1.0 x - 1.0 y == 0.0

    ScalarAffineFunction{Float64}-in-GreaterThan{Float64}
     0.0 + 2.0 x >= 1.0

    ScalarAffineFunction{Float64}-in-Interval{Float64}
     0.0 + 2.0 x ∈ [1.0, 2.0]

    ScalarQuadraticFunction{Float64}-in-LessThan{Float64}
     0.0 + 1.0 y - 1.0 z + 4.0 x² <= 1.0

    VectorOfVariables-in-SecondOrderCone
     ┌ ┐
     │x│
     │y│
     └ ┘ ∈ SecondOrderCone(2)

    VectorAffineFunction{Float64}-in-SecondOrderCone
     ┌           ┐
     │1.0        │
     │0.0 + 1.0 x│
     │0.0 + 1.0 y│
     └           ┘ ∈ SecondOrderCone(2)

    VectorQuadraticFunction{Float64}-in-ExponentialCone
     ┌            ┐
     │0.0 + 2.0 x²│
     │0.0 + 1.0 y │
     │1.0         │
     └            ┘ ∈ ExponentialCone()
     ┌            ┐
     │1.0         │
     │0.0 + 2.0 x²│
     │0.0 + 1.0 y │
     └            ┘ ∈ ExponentialCone()

    SingleVariable-in-GreaterThan{Float64}
     x >= 0.1

    SingleVariable-in-Integer
     z ∈ ℤ

    SingleVariable-in-ZeroOne
     x ∈ {0, 1}
     y ∈ {0, 1}
    """
end

function test_latex()
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, y, z
        minobjective: x + 2 + 3.1*y + -1.2*z
        c1: x >= 0.1
        c2: y in ZeroOne()
        c2: z in Integer()
        c3: [x, y] in SecondOrderCone(2)
        c4: [1, x, y] in SecondOrderCone(2)
        c4: [1.0 * x * x, y, 1] in ExponentialCone()
        c4: [1, 1.0 * x * x, y] in ExponentialCone()
        c2: x in ZeroOne()
        c5: 2.0 * x * x + y + -1 * z <= 1.0
        c5: x + x >= 1.0
        c5: x + x in Interval(1.0, 2.0)
        c5: x + -1 * y == 0.0
        """,
    )
    evaluated =
        sprint(io -> show(io, MIME("text/latex"), MOI.latex_formulation(model)))
    @test evaluated == raw"""
    $$ \begin{aligned}
    \min\quad & 2.0 + 1.0 x + 3.1 y - 1.2 z\\
    \text{Subject to} \quadMathOptInterface.ScalarAffineFunction{Float64}-in-MathOptInterface.EqualTo{Float64}
     & 0.0 + 1.0 x - 1.0 y = 0.0\\
    MathOptInterface.ScalarAffineFunction{Float64}-in-MathOptInterface.GreaterThan{Float64}
     & 0.0 + 2.0 x \ge 1.0\\
    MathOptInterface.ScalarAffineFunction{Float64}-in-MathOptInterface.Interval{Float64}
     & 0.0 + 2.0 x \in \[1.0, 2.0\]\\
    MathOptInterface.ScalarQuadraticFunction{Float64}-in-MathOptInterface.LessThan{Float64}
     & 0.0 + 1.0 y - 1.0 z + 4.0 x^2 \le 1.0\\
    MathOptInterface.VectorOfVariables-in-MathOptInterface.SecondOrderCone
     & \begin{bmatrix}
    x\\
    y\end{bmatrix} \in \text{SecondOrderCone(2)}\\
    MathOptInterface.VectorAffineFunction{Float64}-in-MathOptInterface.SecondOrderCone
     & \begin{bmatrix}
    1.0\\
    0.0 + 1.0 x\\
    0.0 + 1.0 y\end{bmatrix} \in \text{SecondOrderCone(2)}\\
    MathOptInterface.VectorQuadraticFunction{Float64}-in-MathOptInterface.ExponentialCone
     & \begin{bmatrix}
    0.0 + 2.0 x^2\\
    0.0 + 1.0 y\\
    1.0\end{bmatrix} \in \text{ExponentialCone()}\\
     & \begin{bmatrix}
    1.0\\
    0.0 + 2.0 x^2\\
    0.0 + 1.0 y\end{bmatrix} \in \text{ExponentialCone()}\\
    MathOptInterface.SingleVariable-in-MathOptInterface.GreaterThan{Float64}
     & x \ge 0.1\\
    MathOptInterface.SingleVariable-in-MathOptInterface.Integer
     & z \in \mathbb{Z}\\
    MathOptInterface.SingleVariable-in-MathOptInterface.ZeroOne
     & x \in \{0, 1\}\\
     & y \in \{0, 1\}\\
    \end{aligned} $$"""
end

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

end

TestPrint.runtests()
