module TestPrint

using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

const LATEX = MIME("text/latex")
const PLAIN = MIME("text/plain")
const IN = Sys.iswindows() ? "in" : "∈"

# Windows fun...
function _string_compare(a, b)
    @test replace(a, "\r\n" => "\n") == replace(b, "\r\n" => "\n")
    return
end

function test_nonname_variable()
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    @test MOIU._to_string(PLAIN, model, x) == "v[1]"
    @test MOIU._to_string(LATEX, model, x) == "v_{1}"
end

function test_numbers()
    options =
        MOIU._PrintOptions(MIME("text/plain"); simplify_coefficients = true)
    @test MOIU._to_string(options, 1.0, "x"; is_first = true) == "x"
    @test MOIU._to_string(options, 1.0, "x"; is_first = false) == " + x"
    @test MOIU._to_string(options, -1.0, "x"; is_first = true) == "-x"
    @test MOIU._to_string(options, -1.0, "x"; is_first = false) == " - x"
    @test MOIU._to_string(options, 1.2, "x"; is_first = true) == "1.2 x"
    @test MOIU._to_string(options, 1.2, "x"; is_first = false) == " + 1.2 x"
    @test MOIU._to_string(options, -1.2, "x"; is_first = true) == "-1.2 x"
    @test MOIU._to_string(options, -1.2, "x"; is_first = false) == " - 1.2 x"

    options =
        MOIU._PrintOptions(MIME("text/plain"); simplify_coefficients = false)
    @test MOIU._to_string(options, 1.0, "x"; is_first = true) == "1.0 x"
    @test MOIU._to_string(options, 1.0, "x"; is_first = false) == " + 1.0 x"
    @test MOIU._to_string(options, -1.0, "x"; is_first = true) == "-1.0 x"
    @test MOIU._to_string(options, -1.0, "x"; is_first = false) == " - 1.0 x"
    @test MOIU._to_string(options, 1.2, "x"; is_first = true) == "1.2 x"
    @test MOIU._to_string(options, 1.2, "x"; is_first = false) == " + 1.2 x"
    @test MOIU._to_string(options, -1.2, "x"; is_first = true) == "-1.2 x"
    @test MOIU._to_string(options, -1.2, "x"; is_first = false) == " - 1.2 x"
end

function test_variable()
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    for (name, latex_name) in
        [("x", "x"), ("x_y", "x\\_y"), ("x^2", "x\\^2"), ("x[a,b]", "x_{a,b}")]
        MOI.set(model, MOI.VariableName(), x, name)
        @test MOIU._to_string(PLAIN, model, x) == name
        @test MOIU._to_string(LATEX, model, x) == latex_name
    end
end

function test_single_variable()
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    f = MOI.SingleVariable(x)
    @test MOIU._to_string(PLAIN, model, f) == "x"
    @test MOIU._to_string(LATEX, model, f) == "x"
end

function test_ScalarAffineTerm()
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    @test MOIU._to_string(
        PLAIN,
        model,
        MOI.ScalarAffineTerm(-1.2, x);
        is_first = false,
    ) == " - 1.2 x"
    @test MOIU._to_string(
        LATEX,
        model,
        MOI.ScalarAffineTerm(1.2, x);
        is_first = false,
    ) == " + 1.2 x"
end

function test_ScalarAffineFunction()
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    f = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([-1.2, 1.3], [x, x]),
        1.4,
    )
    @test MOIU._to_string(PLAIN, model, f) == "1.4 - 1.2 x + 1.3 x"
    @test MOIU._to_string(LATEX, model, f) == "1.4 - 1.2 x + 1.3 x"
end

function test_ScalarQuadraticTerm()
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    term = MOI.ScalarQuadraticTerm(-1.2, x, x)
    @test MOIU._to_string(PLAIN, model, term; is_first = false) == " - 0.6 x²"
    @test MOIU._to_string(LATEX, model, term; is_first = false) == " - 0.6 x^2"
    term = MOI.ScalarQuadraticTerm(1.2, x, y)
    @test MOIU._to_string(PLAIN, model, term; is_first = false) == " + 1.2 x*y"
    @test MOIU._to_string(LATEX, model, term; is_first = false) ==
          " + 1.2 x\\times y"
end

function test_ScalarQuadraticFunction()
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    f = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm.([-1.2, 1.3], [x, x]),
        MOI.ScalarQuadraticTerm.([0.5, 0.6], [x, x], [x, y]),
        1.4,
    )
    @test MOIU._to_string(PLAIN, model, f) ==
          "1.4 - 1.2 x + 1.3 x + 0.25 x² + 0.6 x*y"
    @test MOIU._to_string(LATEX, model, f) ==
          "1.4 - 1.2 x + 1.3 x + 0.25 x^2 + 0.6 x\\times y"
end

function test_VectorOfVariables()
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    f = MOI.VectorOfVariables([x, y])
    @test MOIU._to_string(PLAIN, model, f) == "┌ ┐\n│x│\n│y│\n└ ┘"
    @test MOIU._to_string(LATEX, model, f) ==
          "\\begin{bmatrix}\nx\\\\\ny\\end{bmatrix}"
end

function test_LessThan()
    s = MOI.LessThan(1.2)
    @test MOIU._to_string(PLAIN, s) == "<= 1.2"
    @test MOIU._to_string(LATEX, s) == "\\le 1.2"
end

function test_GreaterThan()
    s = MOI.GreaterThan(1.2)
    @test MOIU._to_string(PLAIN, s) == ">= 1.2"
    @test MOIU._to_string(LATEX, s) == "\\ge 1.2"
end

function test_EqualTo()
    s = MOI.EqualTo(1.2)
    @test MOIU._to_string(PLAIN, s) == "== 1.2"
    @test MOIU._to_string(LATEX, s) == "= 1.2"
end

function test_Interval()
    s = MOI.Interval(1.2, 1.3)
    @test MOIU._to_string(PLAIN, s) == "$(IN) [1.2, 1.3]"
    @test MOIU._to_string(LATEX, s) == "\\in \\[1.2, 1.3\\]"
end

function test_ZeroOne()
    s = MOI.ZeroOne()
    @test MOIU._to_string(PLAIN, s) == "$(IN) {0, 1}"
    @test MOIU._to_string(LATEX, s) == "\\in \\{0, 1\\}"
end

function test_Integer()
    s = MOI.Integer()
    @test MOIU._to_string(PLAIN, s) == "$(IN) ℤ"
    @test MOIU._to_string(LATEX, s) == "\\in \\mathbb{Z}"
end

function test_ExponentialCone()
    s = MOI.ExponentialCone()
    @test MOIU._to_string(PLAIN, s) == "$(IN) ExponentialCone()"
    @test MOIU._to_string(LATEX, s) == "\\in \\text{ExponentialCone()}"
end

function test_feasibility()
    model = MOIU.Model{Float64}()
    @test sprint(print, model) == "Feasibility\n\nSubject to:\n"
    _string_compare(
        sprint(print, MOIU.latex_formulation(model)),
        raw"""
        $$ \begin{aligned}
        \text{feasibility}\\
        \text{Subject to}\\
        \end{aligned} $$""",
    )
    return
end

function test_min()
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model, "variables: x\nminobjective: x")
    @test sprint(print, model) == """
    Minimize SingleVariable:
     x

    Subject to:
    """
    _string_compare(
        sprint(print, MOIU.latex_formulation(model)),
        raw"""
        $$ \begin{aligned}
        \min\quad & x \\
        \text{Subject to}\\
        \end{aligned} $$""",
    )
    return
end

function test_max()
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model, "variables: x\nmaxobjective: x")
    @test sprint(print, model) == """
    Maximize SingleVariable:
     x

    Subject to:
    """
    _string_compare(
        sprint(print, MOIU.latex_formulation(model)),
        raw"""
        $$ \begin{aligned}
        \max\quad & x \\
        \text{Subject to}\\
        \end{aligned} $$""",
    )
    return
end

function test_model()
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(
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
     0.0 + 2.0 x $(IN) [1.0, 2.0]

    ScalarQuadraticFunction{Float64}-in-LessThan{Float64}
     0.0 + 1.0 y - 1.0 z + 2.0 x² <= 1.0

    VectorOfVariables-in-SecondOrderCone
     ┌ ┐
     │x│
     │y│
     └ ┘ $(IN) SecondOrderCone(2)

    VectorAffineFunction{Float64}-in-SecondOrderCone
     ┌           ┐
     │1.0        │
     │0.0 + 1.0 x│
     │0.0 + 1.0 y│
     └           ┘ $(IN) SecondOrderCone(2)

    VectorQuadraticFunction{Float64}-in-ExponentialCone
     ┌            ┐
     │0.0 + 1.0 x²│
     │0.0 + 1.0 y │
     │1.0         │
     └            ┘ $(IN) ExponentialCone()
     ┌            ┐
     │1.0         │
     │0.0 + 1.0 x²│
     │0.0 + 1.0 y │
     └            ┘ $(IN) ExponentialCone()

    SingleVariable-in-GreaterThan{Float64}
     x >= 0.1

    SingleVariable-in-Integer
     z $(IN) ℤ

    SingleVariable-in-ZeroOne
     x $(IN) {0, 1}
     y $(IN) {0, 1}
    """
end

function test_latex()
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(
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
    _string_compare(
        sprint(
            io -> show(io, MIME("text/latex"), MOIU.latex_formulation(model)),
        ),
        raw"""
        $$ \begin{aligned}
        \min\quad & 2.0 + 1.0 x + 3.1 y - 1.2 z \\
        \text{Subject to}\\
         & \text{ScalarAffineFunction{Float64}-in-EqualTo{Float64}} \\
         & 0.0 + 1.0 x - 1.0 y = 0.0 \\
         & \text{ScalarAffineFunction{Float64}-in-GreaterThan{Float64}} \\
         & 0.0 + 2.0 x \ge 1.0 \\
         & \text{ScalarAffineFunction{Float64}-in-Interval{Float64}} \\
         & 0.0 + 2.0 x \in \[1.0, 2.0\] \\
         & \text{ScalarQuadraticFunction{Float64}-in-LessThan{Float64}} \\
         & 0.0 + 1.0 y - 1.0 z + 2.0 x^2 \le 1.0 \\
         & \text{VectorOfVariables-in-SecondOrderCone} \\
         & \begin{bmatrix}
        x\\
        y\end{bmatrix} \in \text{SecondOrderCone(2)} \\
         & \text{VectorAffineFunction{Float64}-in-SecondOrderCone} \\
         & \begin{bmatrix}
        1.0\\
        0.0 + 1.0 x\\
        0.0 + 1.0 y\end{bmatrix} \in \text{SecondOrderCone(2)} \\
         & \text{VectorQuadraticFunction{Float64}-in-ExponentialCone} \\
         & \begin{bmatrix}
        0.0 + 1.0 x^2\\
        0.0 + 1.0 y\\
        1.0\end{bmatrix} \in \text{ExponentialCone()} \\
         & \begin{bmatrix}
        1.0\\
        0.0 + 1.0 x^2\\
        0.0 + 1.0 y\end{bmatrix} \in \text{ExponentialCone()} \\
         & \text{SingleVariable-in-GreaterThan{Float64}} \\
         & x \ge 0.1 \\
         & \text{SingleVariable-in-Integer} \\
         & z \in \mathbb{Z} \\
         & \text{SingleVariable-in-ZeroOne} \\
         & x \in \{0, 1\} \\
         & y \in \{0, 1\} \\
        \end{aligned} $$""",
    )
    return
end

function test_latex_simplified()
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(
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
    model_string = sprint() do io
        return MOIU._print_model(
            io,
            MOIU._PrintOptions(
                MIME("text/latex");
                simplify_coefficients = true,
                print_types = false,
            ),
            model,
        )
    end
    _string_compare(
        model_string,
        raw"""
        $$ \begin{aligned}
        \min\quad & 2 + x + 3.1 y - 1.2 z \\
        \text{Subject to}\\
         & x - y = 0 \\
         & 2 x \ge 1 \\
         & 2 x \in \[1, 2\] \\
         & y - z + 2 x^2 \le 1 \\
         & \begin{bmatrix}
        x\\
        y\end{bmatrix} \in \text{SecondOrderCone(2)} \\
         & \begin{bmatrix}
        1\\
        x\\
        y\end{bmatrix} \in \text{SecondOrderCone(2)} \\
         & \begin{bmatrix}
        x^2\\
        y\\
        1\end{bmatrix} \in \text{ExponentialCone()} \\
         & \begin{bmatrix}
        1\\
        x^2\\
        y\end{bmatrix} \in \text{ExponentialCone()} \\
         & x \ge 0.1 \\
         & z \in \mathbb{Z} \\
         & x \in \{0, 1\} \\
         & y \in \{0, 1\} \\
        \end{aligned} $$""",
    )
    return
end

function test_plain_simplified()
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(
        model,
        """
        variables: x, y, z
        minobjective: x + -2 + 3.1*y + -1.2*z
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
    model_string = sprint() do io
        return MOIU._print_model(
            io,
            MOIU._PrintOptions(
                MIME("text/plain");
                simplify_coefficients = true,
                print_types = false,
            ),
            model,
        )
    end
    @test model_string == """
    Minimize: -2 + x + 3.1 y - 1.2 z

    Subject to:
     x - y == 0
     2 x >= 1
     2 x $(IN) [1, 2]
     y - z + 2 x² <= 1
     ┌ ┐
     │x│
     │y│
     └ ┘ $(IN) SecondOrderCone(2)
     ┌ ┐
     │1│
     │x│
     │y│
     └ ┘ $(IN) SecondOrderCone(2)
     ┌  ┐
     │x²│
     │y │
     │1 │
     └  ┘ $(IN) ExponentialCone()
     ┌  ┐
     │1 │
     │x²│
     │y │
     └  ┘ $(IN) ExponentialCone()
     x >= 0.1
     z $(IN) ℤ
     x $(IN) {0, 1}
     y $(IN) {0, 1}
    """
end

function test_nlp()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    v = MOI.add_variables(model, 4)
    l = [1.1, 1.2, 1.3, 1.4]
    u = [5.1, 5.2, 5.3, 5.4]
    MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.GreaterThan.(l))
    MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.LessThan.(u))
    for i in 1:4
        MOI.set(model, MOI.VariableName(), v[i], "x[$i]")
    end
    lb, ub = [25.0, 40.0], [Inf, 40.0]
    evaluator = MOI.Test.HS071(true)
    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, true)
    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test sprint(print, model) == """
    Minimize Nonlinear:
     x[1] * x[4] * (x[1] + x[2] + x[3]) + x[3]

    Subject to:

    SingleVariable-in-GreaterThan{Float64}
     x[1] >= 1.1
     x[2] >= 1.2
     x[3] >= 1.3
     x[4] >= 1.4

    SingleVariable-in-LessThan{Float64}
     x[1] <= 5.1
     x[2] <= 5.2
     x[3] <= 5.3
     x[4] <= 5.4

    Nonlinear
     x[1] * x[2] * x[3] * x[4] >= 25.0
     x[1] ^ 2 + x[2] ^ 2 + x[3] ^ 2 + x[4] ^ 2 == 40.0
    """
    _string_compare(
        sprint(print, MOIU.latex_formulation(model)),
        raw"""
        $$ \begin{aligned}
        \min\quad & x_{1} \times x_{4} \times (x_{1} + x_{2} + x_{3}) + x_{3} \\
        \text{Subject to}\\
         & \text{SingleVariable-in-GreaterThan{Float64}} \\
         & x_{1} \ge 1.1 \\
         & x_{2} \ge 1.2 \\
         & x_{3} \ge 1.3 \\
         & x_{4} \ge 1.4 \\
         & \text{SingleVariable-in-LessThan{Float64}} \\
         & x_{1} \le 5.1 \\
         & x_{2} \le 5.2 \\
         & x_{3} \le 5.3 \\
         & x_{4} \le 5.4 \\
         & \text{Nonlinear} \\
         & x_{1} \times x_{2} \times x_{3} \times x_{4} \ge 25.0 \\
         & x_{1} ^ 2 + x_{2} ^ 2 + x_{3} ^ 2 + x_{4} ^ 2 = 40.0 \\
        \end{aligned} $$""",
    )
    return
end

function test_print_with_acronym()
    @test sprint(MOIU.print_with_acronym, "MathOptInterface") == "MOI"
    @test sprint(
        MOIU.print_with_acronym,
        "MathOptInterface.MathOptInterface",
    ) == "MOI.MOI"
    @test sprint(
        MOIU.print_with_acronym,
        "MathOptInterface.Utilities.MathOptInterface",
    ) == "MOIU.MOI"
    @test sprint(MOIU.print_with_acronym, "MathOptInterfaceXXBridges") ==
          "MOIXXBridges"
    @test sprint(MOIU.print_with_acronym, "MathOptInterface.BridgesXX") ==
          "MOIBXX"
    @test sprint(MOIU.print_with_acronym, "MathOptInterface.Test.x") == "MOIT.x"
    @test sprint(MOIU.print_with_acronym, "MathOptInterface.x.Test") ==
          "MOI.x.Test"
    @test sprint(MOIU.print_with_acronym, "MathOptInterface.Utilities.Test") ==
          "MOIU.Test"
    @test sprint(MOIU.print_with_acronym, "MathOptInterface.Utilities.Test") ==
          "MOIU.Test"
    return
end

end

TestPrint.runtests()
