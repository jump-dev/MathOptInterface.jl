module TestNLModel

using MathOptInterface
const MOI = MathOptInterface
const NL = MOI.FileFormats.NL

using Test

function _test_nlexpr(expr::NL._NLExpr, nonlinear_terms, linear_terms, constant)
    @test expr.is_linear == (length(nonlinear_terms) == 0)
    @test expr.nonlinear_terms == nonlinear_terms
    @test expr.linear_terms == linear_terms
    @test expr.constant == constant
    return
end
_test_nlexpr(x, args...) = _test_nlexpr(NL._NLExpr(x), args...)

function test_nlexpr_singlevariable()
    x = MOI.VariableIndex(1)
    _test_nlexpr(MOI.SingleVariable(x), NL._NLTerm[], Dict(x => 1.0), 0.0)
    return
end

function test_nlexpr_scalaraffine()
    x = MOI.VariableIndex.(1:3)
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 4.0)
    return _test_nlexpr(f, NL._NLTerm[], Dict(x .=> 1), 4.0)
end

function test_nlexpr_scalarquadratic()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(1.1, x)],
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        3.0,
    )
    terms = [NL.OPMULT, x, x]
    return _test_nlexpr(f, terms, Dict(x => 1.1), 3.0)
end

function test_nlexpr_unary_addition()
    x = MOI.VariableIndex(1)
    return _test_nlexpr(:(+$x), [x], Dict(x => 0), 0.0)
end

function test_nlexpr_binary_addition()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    return _test_nlexpr(
        :($x + $y),
        [NL.OPPLUS, x, y],
        Dict(x => 0.0, y => 0.0),
        0.0,
    )
end

function test_nlexpr_nary_addition()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    return _test_nlexpr(
        :($x + $y + 1.0),
        [NL.OPSUMLIST, 3, x, y, 1.0],
        Dict(x => 0.0, y => 0.0),
        0.0,
    )
end

function test_nlexpr_unary_subtraction()
    x = MOI.VariableIndex(1)
    return _test_nlexpr(:(-$x), [NL.OPUMINUS, x], Dict(x => 0.0), 0.0)
end

function test_nlexpr_nary_multiplication()
    x = MOI.VariableIndex(1)
    return _test_nlexpr(
        :($x * $x * 2.0),
        [NL.OPMULT, x, NL.OPMULT, x, 2.0],
        Dict(x => 0.0),
        0.0,
    )
end

function test_nlexpr_unary_specialcase()
    x = MOI.VariableIndex(1)
    return _test_nlexpr(
        :(cbrt($x)),
        [NL.OPPOW, x, NL.OPDIV, 1, 3],
        Dict(x => 0.0),
        0.0,
    )
end

function test_nlexpr_binary_specialcase()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    return _test_nlexpr(
        :(\($x, $y)),
        [NL.OPDIV, y, x],
        Dict(x => 0.0, y => 0.0),
        0.0,
    )
end

function test_nlexpr_unsupportedoperation()
    x = MOI.VariableIndex(1)
    err = ErrorException("Unsupported operation foo")
    @test_throws err NL._NLExpr(:(foo($x)))
    return
end

function test_nlexpr_unsupportedexpression()
    x = MOI.VariableIndex(1)
    expr = :(1 <= $x <= 2)
    err = ErrorException("Unsupported expression: $(expr)")
    @test_throws err NL._NLExpr(expr)
    return
end

function test_nlexpr_ref()
    x = MOI.VariableIndex(1)
    return _test_nlexpr(:(x[$x]), [x], Dict(x => 0.0), 0.0)
end

function test_nlconstraint_interval()
    x = MOI.VariableIndex(1)
    expr = :(1.0 <= $x <= 2.0)
    con = NL._NLConstraint(expr, MOI.NLPBoundsPair(1.0, 2.0))
    @test con.lower == 1
    @test con.upper == 2
    @test con.opcode == 0
    @test con.expr == NL._NLExpr(expr.args[3])
end

function test_nlconstraint_lessthan()
    x = MOI.VariableIndex(1)
    expr = :($x <= 2.0)
    con = NL._NLConstraint(expr, MOI.NLPBoundsPair(-Inf, 2.0))
    @test con.lower == -Inf
    @test con.upper == 2
    @test con.opcode == 1
    @test con.expr == NL._NLExpr(expr.args[2])
end

function test_nlconstraint_greaterthan()
    x = MOI.VariableIndex(1)
    expr = :($x >= 2.0)
    con = NL._NLConstraint(expr, MOI.NLPBoundsPair(2.0, Inf))
    @test con.lower == 2
    @test con.upper == Inf
    @test con.opcode == 2
    @test con.expr == NL._NLExpr(expr.args[2])
end

function test_nlconstraint_equalto()
    x = MOI.VariableIndex(1)
    expr = :($x == 2.0)
    con = NL._NLConstraint(expr, MOI.NLPBoundsPair(2.0, 2.0))
    @test con.lower == 2
    @test con.upper == 2
    @test con.opcode == 4
    @test con.expr == NL._NLExpr(expr.args[2])
end

function test_nlconstraint_interval_warn()
    x = MOI.VariableIndex(1)
    expr = :(2.0 <= $x <= 1.0)
    @test_logs (:warn,) NL._NLConstraint(expr, MOI.NLPBoundsPair(1.0, 2.0))
end

function test_nlconstraint_lessthan_warn()
    x = MOI.VariableIndex(1)
    expr = :($x <= 1.0)
    @test_logs (:warn,) NL._NLConstraint(expr, MOI.NLPBoundsPair(-Inf, 2.0))
end

function test_nlconstraint_greaterthan_warn()
    x = MOI.VariableIndex(1)
    expr = :($x >= 0.0)
    @test_logs (:warn,) NL._NLConstraint(expr, MOI.NLPBoundsPair(1.0, Inf))
end

function test_nlconstraint_equalto_warn()
    x = MOI.VariableIndex(1)
    expr = :($x == 2.0)
    @test_logs (:warn,) NL._NLConstraint(expr, MOI.NLPBoundsPair(1.0, 1.0))
end

function test_nlmodel_hs071()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    v = MOI.add_variables(model, 4)
    l = [1.1, 1.2, 1.3, 1.4]
    u = [5.1, 5.2, 5.3, 5.4]
    start = [2.1, 2.2, 2.3, 2.4]
    MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.GreaterThan.(l))
    MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.LessThan.(u))
    MOI.set.(model, MOI.VariablePrimalStart(), v, start)
    lb, ub = [25.0, 40.0], [Inf, 40.0]
    evaluator = MOI.Test.HS071(true)
    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, true)
    @test MOI.supports(model, MOI.NLPBlock())
    MOI.set(model, MOI.NLPBlock(), block_data)
    @test MOI.supports(model, MOI.ObjectiveSense())
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    n = NL.Model()
    @test MOI.is_empty(n)
    MOI.copy_to(n, model)
    @test !MOI.is_empty(n)
    @test n.sense == MOI.MIN_SENSE
    @test n.f == NL._NLExpr(MOI.objective_expr(evaluator))
    _test_nlexpr(
        n.g[1].expr,
        [NL.OPMULT, v[1], NL.OPMULT, v[2], NL.OPMULT, v[3], v[4]],
        Dict(v .=> 0.0),
        0.0,
    )
    @test n.g[1].lower == 25.0
    @test n.g[1].upper == Inf
    @test n.g[1].opcode == 2
    _test_nlexpr(
        n.g[2].expr,
        [
            NL.OPSUMLIST,
            4,
            NL.OPPOW,
            v[1],
            2,
            NL.OPPOW,
            v[2],
            2,
            NL.OPPOW,
            v[3],
            2,
            NL.OPPOW,
            v[4],
            2,
        ],
        Dict(v .=> 0.0),
        0.0,
    )
    @test n.g[2].lower == 40.0
    @test n.g[2].upper == 40.0
    @test n.g[2].opcode == 4
    @test length(n.h) == 0
    for i in 1:4
        @test n.x[v[i]].lower == l[i]
        @test n.x[v[i]].upper == u[i]
        @test n.x[v[i]].type == NL._CONTINUOUS
        @test n.x[v[i]].jacobian_count == 2
        @test n.x[v[i]].in_nonlinear_constraint
        @test n.x[v[i]].in_nonlinear_objective
        @test 0 <= n.x[v[i]].order <= 3
    end
    @test length(n.types[1]) == 4
    @test sprint(write, n) == """
    g3 1 1 0
     4 2 1 0 1 0
     2 1
     0 0
     4 4 4
     0 0 0 1
     0 0 0 0 0
     8 4
     0 0
     0 0 0 0 0
    C0
    o2
    v0
    o2
    v1
    o2
    v2
    v3
    C1
    o54
    4
    o5
    v0
    n2
    o5
    v1
    n2
    o5
    v2
    n2
    o5
    v3
    n2
    O0 0
    o0
    o2
    v0
    o2
    v3
    o54
    3
    v0
    v1
    v2
    v2
    x4
    0 2.1
    1 2.2
    2 2.3
    3 2.4
    r
    2 25
    4 40
    b
    0 1.1 5.1
    0 1.2 5.2
    0 1.3 5.3
    0 1.4 5.4
    k3
    2
    4
    6
    J0 4
    0 0
    1 0
    2 0
    3 0
    J1 4
    0 0
    1 0
    2 0
    3 0
    G0 4
    0 0
    1 0
    2 0
    3 0
    """
    return
end

function test_nlmodel_hs071_linear_obj()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    v = MOI.add_variables(model, 4)
    l = [1.1, 1.2, 1.3, 1.4]
    u = [5.1, 5.2, 5.3, 5.4]
    start = [2.1, 2.2, 2.3, 2.4]
    MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.GreaterThan.(l))
    MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.LessThan.(u))
    MOI.add_constraint(model, MOI.SingleVariable(v[2]), MOI.ZeroOne())
    MOI.add_constraint(model, MOI.SingleVariable(v[3]), MOI.Integer())
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    MOI.set.(model, MOI.VariablePrimalStart(), v, start)
    lb, ub = [25.0, 40.0], [Inf, 40.0]
    evaluator = MOI.Test.HS071(true)
    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, false)
    MOI.set(model, MOI.NLPBlock(), block_data)
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(l, v), 2.0)
    @test MOI.supports(model, MOI.ObjectiveFunction{typeof(f)}())
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    n = NL.Model()
    MOI.copy_to(n, model)
    @test n.sense == MOI.MAX_SENSE
    @test n.f == NL._NLExpr(f)
    _test_nlexpr(
        n.g[1].expr,
        [NL.OPMULT, v[1], NL.OPMULT, v[2], NL.OPMULT, v[3], v[4]],
        Dict(v .=> 0.0),
        0.0,
    )
    @test n.g[1].lower == 25.0
    @test n.g[1].upper == Inf
    @test n.g[1].opcode == 2
    _test_nlexpr(
        n.g[2].expr,
        [
            NL.OPSUMLIST,
            4,
            NL.OPPOW,
            v[1],
            2,
            NL.OPPOW,
            v[2],
            2,
            NL.OPPOW,
            v[3],
            2,
            NL.OPPOW,
            v[4],
            2,
        ],
        Dict(v .=> 0.0),
        0.0,
    )
    @test n.g[2].lower == 40.0
    @test n.g[2].upper == 40.0
    @test n.g[2].opcode == 4
    @test length(n.h) == 0
    types = [NL._CONTINUOUS, NL._BINARY, NL._INTEGER, NL._CONTINUOUS]
    u[2] = 1.0
    for i in 1:4
        @test n.x[v[i]].lower == l[i]
        @test n.x[v[i]].upper == u[i]
        @test n.x[v[i]].type == types[i]
        @test n.x[v[i]].jacobian_count == 2
        @test n.x[v[i]].in_nonlinear_constraint
        @test !n.x[v[i]].in_nonlinear_objective
        @test 0 <= n.x[v[i]].order <= 3
    end
    @test length(n.types[3]) == 2
    @test length(n.types[4]) == 2
    @test v[1] in n.types[3]
    @test v[2] in n.types[4]
    @test v[3] in n.types[4]
    @test v[4] in n.types[3]
    @test sprint(write, n) == """
    g3 1 1 0
     4 2 1 0 1 0
     2 1
     0 0
     4 0 0
     0 0 0 1
     0 0 0 2 0
     8 4
     0 0
     0 0 0 0 0
    C0
    o2
    v0
    o2
    v2
    o2
    v3
    v1
    C1
    o54
    4
    o5
    v0
    n2
    o5
    v2
    n2
    o5
    v3
    n2
    o5
    v1
    n2
    O0 1
    n2
    x4
    0 2.1
    1 2.4
    2 2.2
    3 2.3
    r
    2 25
    4 40
    b
    0 1.1 5.1
    0 1.4 5.4
    0 1.2 1
    0 1.3 5.3
    k3
    2
    4
    6
    J0 4
    0 0
    1 0
    2 0
    3 0
    J1 4
    0 0
    1 0
    2 0
    3 0
    G0 4
    0 1.1
    1 1.4
    2 1.2
    3 1.3
    """
    return
end

function test_nlmodel_linear_quadratic()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variables(model, 4)
    MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.LessThan(2.0))
    MOI.add_constraint(model, MOI.SingleVariable(x[2]), MOI.ZeroOne())
    MOI.add_constraint(model, MOI.SingleVariable(x[3]), MOI.Integer())
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x[2:4]), 2.0)
    g = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(1.0, x[1])],
        [MOI.ScalarQuadraticTerm(2.0, x[1], x[2])],
        3.0,
    )
    h = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(1.0, x[3])],
        [MOI.ScalarQuadraticTerm(1.0, x[1], x[2])],
        0.0,
    )
    MOI.add_constraint(model, f, MOI.Interval(1.0, 10.0))
    MOI.add_constraint(model, g, MOI.LessThan(5.0))
    MOI.set(model, MOI.ObjectiveFunction{typeof(h)}(), h)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    n = NL.Model()
    MOI.copy_to(n, model)
    @test n.sense == MOI.MAX_SENSE
    @test n.f == NL._NLExpr(h)
    terms = [NL.OPMULT, 2.0, NL.OPMULT, x[1], x[2]]
    _test_nlexpr(n.g[1].expr, terms, Dict(x[1] => 1.0, x[2] => 0.0), 3.0)
    @test n.g[1].opcode == 1
    @test n.g[1].lower == -Inf
    @test n.g[1].upper == 5.0
    @test n.h[1].expr == NL._NLExpr(f)
    @test n.h[1].opcode == 0
    @test n.h[1].lower == 1.0
    @test n.h[1].upper == 10.0
    @test n.types[1] == [x[1]]  # Continuous in both
    @test n.types[2] == [x[2]]  # Discrete in both
    @test n.types[6] == [x[3]]  # Discrete in objective only
    @test n.types[7] == [x[4]]  # Continuous in linear
    @test sprint(write, n) == """
    g3 1 1 0
     4 2 1 1 0 0
     1 1
     0 0
     2 3 2
     0 0 0 1
     0 0 1 0 1
     5 3
     0 0
     0 0 0 0 0
    C0
    o0
    n3
    o2
    n2
    o2
    v0
    v1
    C1
    n2
    O0 1
    o2
    v0
    v1
    x4
    0 0
    1 0
    2 0
    3 0
    r
    1 5
    0 1 10
    b
    0 0 2
    0 0 1
    0 0 2
    0 0 2
    k3
    1
    3
    4
    J0 2
    0 1
    1 0
    J1 3
    1 1
    2 1
    3 1
    G0 3
    0 0
    1 0
    2 1
    """
    return
end

function test_nlmodel_quadratic_interval()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    g = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(1.0, x)],
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        3.0,
    )
    MOI.add_constraint(model, g, MOI.Interval(1.0, 10.0))
    n = NL.Model()
    MOI.copy_to(n, model)
    @test sprint(write, n) == """
    g3 1 1 0
     1 1 1 1 0 0
     1 1
     0 0
     1 0 0
     0 0 0 1
     0 0 0 0 0
     1 0
     0 0
     0 0 0 0 0
    C0
    o0
    n3
    o2
    v0
    v0
    O0 0
    n0
    x1
    0 0
    r
    0 1 10
    b
    3
    k0
    J0 1
    0 1
    """
    return
end

function test_eval_singlevariable()
    x = MOI.VariableIndex(1)
    f = NL._NLExpr(MOI.SingleVariable(x))
    @test NL._evaluate(f, Dict(x => 1.2)) == 1.2
end

function test_eval_scalaraffine()
    x = MOI.VariableIndex.(1:3)
    f = NL._NLExpr(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 4.0))
    @test NL._evaluate(f, Dict(x[i] => Float64(i) for i in 1:3)) == 10.0
end

function test_eval_scalarquadratic()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(1.1, x)],
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        3.0,
    )
    @test NL._evaluate(NL._NLExpr(f), Dict(x => 1.1)) == 5.42
end

function test_eval_unary_addition()
    x = MOI.VariableIndex(1)
    @test NL._evaluate(NL._NLExpr(:(+$x)), Dict(x => 1.1)) == 1.1
end

function test_eval_binary_addition()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    @test NL._evaluate(NL._NLExpr(:($x + $y)), Dict(x => 1.1, y => 2.2)) ≈ 3.3
end

function test_eval_nary_addition()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    @test NL._evaluate(NL._NLExpr(:($x + $y + 1.0)), Dict(x => 1.1, y => 2.2)) ≈
          4.3
end

function test_eval_unary_subtraction()
    x = MOI.VariableIndex(1)
    @test NL._evaluate(NL._NLExpr(:(-$x)), Dict(x => 1.1)) == -1.1
end

function test_eval_nary_multiplication()
    x = MOI.VariableIndex(1)
    @test NL._evaluate(NL._NLExpr(:($x * $x * 2.0)), Dict(x => 1.1)) ≈ 2.42
end

function test_eval_unary_specialcases()
    x = MOI.VariableIndex(1)
    S = [:acoth, :asec, :acsc, :acot, :asecd, :acscd, :acotd]
    for (k, v) in NL._UNARY_SPECIAL_CASES
        expr = NL._NLExpr(:($k($x)))
        xv = k in S ? 1.49 : 0.49
        @test NL._evaluate(expr, Dict(x => xv)) ≈ getfield(Main, k)(xv)
    end
end

function test_eval_binary_specialcases()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    for (k, v) in NL._BINARY_SPECIAL_CASES
        expr = NL._NLExpr(:($k($x, $y)))
        target = getfield(Main, k)(1.0, 2.0)
        @test NL._evaluate(expr, Dict(x => 1.0, y => 2.0)) ≈ target
    end
end

"""
    test_issue_79()

Test the problem

    min (z - 0.5)^2 = z^2 - z + 1/4
    s.t. x * z <= 0
         z ∈ {0, 1}
"""
function test_issue_79()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    z = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(z), MOI.ZeroOne())
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(-1.0, z)],
        [MOI.ScalarQuadraticTerm(1.0, z, z)],
        0.25,
    )
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.add_constraint(
        model,
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Float64}[],
            [MOI.ScalarQuadraticTerm(1.0, x, z)],
            0.0,
        ),
        MOI.LessThan(0.0),
    )
    n = NL.Model()
    MOI.copy_to(n, model)
    # x is continuous in a nonlinear constraint             [Priority 3]
    # z is discrete in a nonlinear constraint and objective [Priority 2]
    @test n.x[x].order == 1
    @test n.x[z].order == 0
end

function test_malformed_constraint_error()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 1.0),
        MOI.LessThan(0.0),
    )
    n = NL.Model()
    @test_throws ErrorException MOI.copy_to(n, model)
end

struct NoExprGraph <: MOI.AbstractNLPEvaluator end
MOI.features_available(::NoExprGraph) = Symbol[]

function test_noexpr_graph()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair[], NoExprGraph(), false)
    MOI.set(model, MOI.NLPBlock(), block_data)
    n = NL.Model()
    @test_throws ErrorException MOI.copy_to(n, model)
end

function test_Name()
    model = NL.Model()
    @test MOI.supports(model, MOI.Name())
    MOI.set(model, MOI.Name(), "MyModel")
    @test MOI.get(model, MOI.Name()) == "MyModel"
end

function test_SolverName()
    model = NL.Model()
    @test MOI.get(model, MOI.SolverName()) == "AmplNLWriter"
end

function test_show()
    model = NL.Model()
    @test sprint(show, model) == "An AMPL (.nl) model"
end

function test_linear_constraint_types()
    model = MOI.Utilities.Model{Float64}()
    y = MOI.add_variables(model, 3)
    MOI.add_constraint(model, MOI.SingleVariable(y[1]), MOI.ZeroOne())
    MOI.add_constraint(model, MOI.SingleVariable(y[2]), MOI.Integer())
    for set in [
        MOI.GreaterThan(0.0),
        MOI.LessThan(1.0),
        MOI.EqualTo(2.0),
        MOI.Interval(3.0, 4.0),
    ]
        x = MOI.add_variable(model)
        MOI.add_constraint(model, MOI.SingleVariable(x), set)
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            set,
        )
    end
    n = NL.Model()
    map = MOI.copy_to(n, model)
    @test length(n.g) == 0
    @test length(n.h) == 4
    @test length(n.x) == 7
    @test n.order == [
        MOI.VariableIndex(3),
        MOI.VariableIndex(4),
        MOI.VariableIndex(5),
        MOI.VariableIndex(6),
        MOI.VariableIndex(7),
        MOI.VariableIndex(1),
        MOI.VariableIndex(2),
    ]
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

TestNLModel.runtests()
