# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMPS

using Test

import MathOptInterface as MOI
import MathOptInterface.FileFormats: MPS
import OrderedCollections: OrderedDict

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$name" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function _test_write_to_file(input::String, output::String; kwargs...)
    model = MPS.Model(; kwargs...)
    MOI.Utilities.loadfromstring!(model, input)
    data = sprint(write, model)
    if data != output
        print(data)
    end
    @test data == output
    return model
end

function _test_model_equality(
    model_string,
    variables,
    constraints,
    args...;
    kwargs...,
)
    model = MPS.Model(; kwargs...)
    MOI.Utilities.loadfromstring!(model, model_string)
    io = IOBuffer()
    write(io, model)
    model_2 = MPS.Model(; kwargs...)
    seekstart(io)
    read!(io, model_2)
    MOI.Test.util_test_models_equal(
        model,
        model_2,
        variables,
        constraints,
        args...,
    )
    return
end

function test_show()
    @test sprint(summary, MPS.Model()) == "MOI.FileFormats.MPS.Model"
    return
end

function test_nonempty()
    model = MPS.Model()
    @test MOI.is_empty(model)
    MOI.add_variable(model)
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    MOI.add_variable(model)
    @test_throws(
        ErrorException("Cannot read in file because model is not empty."),
        MOI.read_from_file(model, joinpath(@__DIR__, "free_integer.mps")),
    )
    return
end

function test_failing_models()
    dir = joinpath(@__DIR__, "failing_models")
    @testset "$file" for file in filter(endswith(".mps"), readdir(dir))
        @test_throws(
            MPS.ParseError,
            MOI.read_from_file(MPS.Model(), joinpath(dir, file))
        )
        @info file
        try
            MOI.read_from_file(MPS.Model(), joinpath(dir, file))
        catch err
            showerror(stdout, err)
        end
        println()
    end
    return
end

function test_empty_row_name()
    model = MPS.Model()
    x = MOI.add_variable(model)
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.LessThan(1.0),
    )
    @test_throws Exception sprint(MPS.write_rows, model)
    return
end

function test_sos()
    model = MPS.Model()
    x = MOI.add_variables(model, 3)
    names = Dict{MOI.VariableIndex,Int}()
    for i in 1:3
        MOI.set(model, MOI.VariableName(), x[i], "x$(i)")
        names[x[i]] = i
    end
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.SOS1([1.5, 2.5, 3.5]),
    )
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.SOS2([1.25, 2.25, 3.25]),
    )
    @test sprint(MPS.write_sos, model, names) ==
          "SOS\n" *
          " S1 SOS1\n" *
          "    x1        1.5\n" *
          "    x2        2.5\n" *
          "    x3        3.5\n" *
          " S2 SOS2\n" *
          "    x1        1.25\n" *
          "    x2        2.25\n" *
          "    x3        3.25\n"
    return
end

function test_maximization()
    model = MPS.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    @test sprint(MPS.write_columns, model, true, OrderedDict(x => 1)) ==
          "COLUMNS\n    x         OBJ       -1\n"
    return
end

function test_maximization_objsense_false()
    model = MPS.Model(; print_objsense = true)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    @test sprint(MPS.write_columns, model, false, OrderedDict(x => 1)) ==
          "COLUMNS\n    x         OBJ       1\n"
    return
end

function test_stacked_data()
    model = MPS.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "stacked_data.mps"))
    model_2 = MPS.Model()
    MOI.Utilities.loadfromstring!(
        model_2,
        """
        variables: x, y, z
        maxobjective: x + y + z + 2.5
        blank_obj: 1.0 * x + 2.0 * y in Interval(-Inf, Inf)
        con1: 1.0 * x in Interval(1.0, 5.0)
        con2: 1.0 * x in Interval(2.0, 6.0)
        con3: 1.0 * x in Interval(3.0, 7.0)
        con4: 2.0 * x in Interval(4.0, 8.0)
        y in Integer()
        y >= 1.0
        y <= 4.0
        z in ZeroOne()
        """,
    )
    MOI.set(model_2, MOI.Name(), "stacked_data")
    MOI.Test.util_test_models_equal(
        model,
        model_2,
        ["x", "y", "z"],
        ["blank_obj", "con1", "con2", "con3", "con4"],
        [
            ("y", MOI.Integer()),
            ("y", MOI.GreaterThan{Float64}(1.0)),
            ("y", MOI.LessThan{Float64}(4.0)),
            ("z", MOI.ZeroOne()),
        ],
    )
    return
end

function test_integer_default_bounds()
    model = MPS.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "integer_default_bounds.mps"))
    x = only(MOI.get(model, MOI.ListOfVariableIndices()))
    ci =
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}}(x.value)
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.GreaterThan(0.0)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}}(x.value)
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.LessThan(1.0)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Float64}}(x.value)
    @test !MOI.is_valid(model, ci)
    return
end

function test_integer_default_bounds_LI()
    model = MPS.Model()
    filename = joinpath(@__DIR__, "integer_default_bounds_LI.mps")
    MOI.read_from_file(model, filename)
    x = only(MOI.get(model, MOI.ListOfVariableIndices()))
    c_types = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(c_types) == 2
    @test (MOI.VariableIndex, MOI.Integer) in c_types
    F, S = MOI.VariableIndex, MOI.GreaterThan{Float64}
    @test (F, S) in c_types
    ci = MOI.ConstraintIndex{F,S}(x.value)
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.GreaterThan(1.0)
    return
end

function test_integer_default_bounds_MI()
    model = MPS.Model()
    filename = joinpath(@__DIR__, "integer_default_bounds_MI.mps")
    MOI.read_from_file(model, filename)
    @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) ==
          [(MOI.VariableIndex, MOI.Integer)]
    return
end

function test_integer_default_bounds_PL()
    model = MPS.Model()
    filename = joinpath(@__DIR__, "integer_default_bounds_PL.mps")
    MOI.read_from_file(model, filename)
    x = only(MOI.get(model, MOI.ListOfVariableIndices()))
    c_types = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test length(c_types) == 2
    @test (MOI.VariableIndex, MOI.Integer) in c_types
    F, S = MOI.VariableIndex, MOI.GreaterThan{Float64}
    @test (F, S) in c_types
    ci = MOI.ConstraintIndex{F,S}(x.value)
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.GreaterThan(0.0)
    return
end

function test_free_integer()
    model = MPS.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "free_integer.mps"))
    model_2 = MPS.Model()
    MOI.Utilities.loadfromstring!(
        model_2,
        """
        variables: x
        minobjective: x
        con1: 1.0 * x >= 1.0
        x in Integer()
        """,
    )
    MOI.Test.util_test_models_equal(
        model,
        model_2,
        ["x"],
        ["con1"],
        [("x", MOI.Integer())],
    )
    return
end

function test_min_objective()
    _test_model_equality(
        """
        variables: x
        minobjective: x
        """,
        ["x"],
        String[],
    )
    return
end

function test_objconst()
    _test_model_equality(
        """
        variables: x
        minobjective: 1.1 * x + 1.2
        """,
        ["x"],
        String[],
    )
    return
end

function test_default_rhs_greater()
    _test_model_equality(
        """
        variables: x
        minobjective: x
        c1: 2.0 * x >= 0.0
        """,
        ["x"],
        ["c1"],
    )
    return
end

function test_default_rhs_less()
    _test_model_equality(
        """
        variables: x
        minobjective: x
        c1: 2.0 * x <= 0.0
        """,
        ["x"],
        ["c1"],
    )
    return
end

function test_default_rhs_equal()
    _test_model_equality(
        """
        variables: x
        minobjective: x
        c1: 2.0 * x == 0.0
        """,
        ["x"],
        ["c1"],
    )
    return
end

function test_min_scalaraffine()
    _test_model_equality(
        """
        variables: x
        minobjective: 1.2x
        """,
        ["x"],
        String[],
    )
    return
end

function test_scalaraffine_greaterthan()
    _test_model_equality(
        """
        variables: x
        minobjective: 1.2x
        c1: 1.1 * x >= 2.0
        """,
        ["x"],
        ["c1"],
    )
    return
end

function test_scalaraffine_lessthan()
    _test_model_equality(
        """
        variables: x
        minobjective: 1.2x
        c1: 1.1 * x <= 2.0
        """,
        ["x"],
        ["c1"],
    )
    return
end

function test_scalaraffine_equalto()
    _test_model_equality(
        """
        variables: x
        minobjective: 1.2x
        c1: 1.1 * x == 2.0
        """,
        ["x"],
        ["c1"],
    )
    return
end

function test_scalaraffine_interval()
    _test_model_equality(
        """
        variables: x
        minobjective: 1.2x
        c1: 1.1 * x in Interval(1.0, 2.0)
        """,
        ["x"],
        ["c1"],
    )
    return
end

function test_objsense_max()
    _test_model_equality(
        """
        variables: x
        maxobjective: 1.2x
        c1: 1.0 * x >= 0.0
        """,
        ["x"],
        ["c1"];
        print_objsense = true,
    )
    _test_model_equality(
        """
        variables: x
        minobjective: 1.2x
        c1: 1.0 * x >= 0.0
        """,
        ["x"],
        ["c1"];
        print_objsense = true,
    )
    return
end

function test_MARKER_INT()
    model = MPS.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, y, z
        minobjective: x + y + z
        x in Integer()
        c2: 2 * x + -1.0 * z <= 1.0
        z in ZeroOne()
        x >= 1.0
        """,
    )
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    model_2 = MPS.Model()
    read!(io, model_2)
    MOI.Test.util_test_models_equal(
        model,
        model_2,
        ["x", "y", "z"],
        ["c2"],
        [
            ("x", MOI.Integer()),
            ("z", MOI.ZeroOne()),
            ("x", MOI.GreaterThan(1.0)),
        ],
    )
    return
end

function test_zero_variable_bounds()
    model = MPS.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, y, z
        minobjective: x + y + z
        x >= 0.0
        y <= 0.0
        """,
    )
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    model_2 = MPS.Model()
    read!(io, model_2)
    MOI.Test.util_test_models_equal(
        model,
        model_2,
        ["x", "y", "z"],
        String[],
        [
            ("x", MOI.GreaterThan{Float64}(0.0)),
            ("y", MOI.LessThan{Float64}(0.0)),
        ],
    )
    return
end

function test_nonzero_variable_bounds()
    model = MPS.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: w, x, y, z
        minobjective: w + x + y + z
        x == 1.0
        y >= 2.0
        z <= 3.0
        w >= 4.0
        w <= 5.0
        """,
    )
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    model_2 = MPS.Model()
    read!(io, model_2)
    MOI.Test.util_test_models_equal(
        model,
        model_2,
        ["w", "x", "y", "z"],
        String[],
        [
            ("x", MOI.EqualTo{Float64}(1.0)),
            ("y", MOI.GreaterThan{Float64}(2.0)),
            ("z", MOI.LessThan{Float64}(3.0)),
            ("w", MOI.GreaterThan{Float64}(4.0)),
            ("w", MOI.LessThan{Float64}(5.0)),
        ],
    )
    return
end

function test_multiple_variable_bounds()
    _test_write_to_file(
        """
        variables: a_really_long_name
        minobjective: a_really_long_name
        a_really_long_name >= 1.0
        a_really_long_name <= 2.0
        """,
        """
        NAME
        ROWS
         N  OBJ
        COLUMNS
            a_really_long_name OBJ 1
        RHS
        RANGES
        BOUNDS
         LO bounds    a_really_long_name 1
         UP bounds    a_really_long_name 2
        ENDATA
        """,
    )
    return
end

function test_unused_variable()
    # In this test, `x` will be written to the file with a 0 objective
    # coefficient since it does not appear in the objective or in the
    # constraints.
    model = MPS.Model()
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, y
        minobjective: y
        c1: 2.0 * y >= 1.0
        x >= 0.0
        """,
    )
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    model_2 = MPS.Model()
    read!(io, model_2)
    @test MOI.get(model_2, MOI.NumberOfVariables()) == 2
    return
end

function test_names_with_spaces()
    model = MPS.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x[1, 2]")
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.EqualTo(1.0),
    )
    MOI.set(model, MOI.ConstraintName(), c, "c c")
    @test sprint(write, model) ==
          "NAME\n" *
          "ROWS\n" *
          " N  OBJ\n" *
          " E  c_c\n" *
          "COLUMNS\n" *
          "    x[1,_2]   c_c       1\n" *
          "RHS\n" *
          "    rhs       c_c       1\n" *
          "RANGES\n" *
          "BOUNDS\n" *
          " FR bounds    x[1,_2]\n" *
          "ENDATA\n"
    return
end

function test_objsense_default()
    _test_write_to_file(
        """
        variables: x
        maxobjective: x
        """,
        """
        NAME
        ROWS
         N  OBJ
        COLUMNS
            x         OBJ       -1
        RHS
        RANGES
        BOUNDS
         FR bounds    x
        ENDATA
        """,
    )
    return
end

function test_objsense_true()
    _test_write_to_file(
        """
        variables: x
        maxobjective: x
        """,
        """
        NAME
        OBJSENSE
            MAX
        ROWS
         N  OBJ
        COLUMNS
            x         OBJ       1
        RHS
        RANGES
        BOUNDS
         FR bounds    x
        ENDATA
        """;
        print_objsense = true,
    )
    return
end

function test_sos_constraints()
    model = _test_write_to_file(
        """
        variables: x1, x2, x3
        [x1, x2, x3] in SOS1([1.0, 2.0, 3.0])
        [x3, x2, x1] in SOS2([1.2, 2.3, 3.4])
        """,
        """
        NAME
        ROWS
         N  OBJ
        COLUMNS
            x1        OBJ       0
            x2        OBJ       0
            x3        OBJ       0
        RHS
        RANGES
        BOUNDS
         FR bounds    x1
         FR bounds    x2
         FR bounds    x3
        SOS
         S1 SOS1
            x1        1
            x2        2
            x3        3
         S2 SOS2
            x3        1.2
            x2        2.3
            x1        3.4
        ENDATA
        """,
    )
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    new_model = MPS.Model()
    Base.read!(io, new_model)
    constraints = MOI.get(new_model, MOI.ListOfConstraintTypesPresent())
    @test (MOI.VectorOfVariables, MOI.SOS1{Float64}) in constraints
    @test (MOI.VectorOfVariables, MOI.SOS2{Float64}) in constraints
    return
end

function test_generic_names()
    _test_write_to_file(
        """
        variables: x, y
        c: x + y == 1.0
        y >= 2.0
        """,
        """
        NAME
        ROWS
         N  OBJ
         E  R1
        COLUMNS
            C1        R1        1
            C2        R1        1
        RHS
            rhs       R1        1
        RANGES
        BOUNDS
         FR bounds    C1
         LO bounds    C2        2
         PL bounds    C2
        ENDATA
        """;
        generic_names = true,
    )
    return
end

function test_rew_filename()
    model = MOI.FileFormats.Model(; filename = "test.rew")
    @test model isa MPS.Model
    @test model.ext[:MPS_OPTIONS].generic_names == true
    return
end

function test_rew_format()
    model = MOI.FileFormats.Model(; format = MOI.FileFormats.FORMAT_REW)
    @test model isa MPS.Model
    @test model.ext[:MPS_OPTIONS].generic_names == true
    return
end

function test_infinite_interval()
    _test_write_to_file(
        """
        variables: x1
        1.0 * x1 in Interval(-Inf, Inf)
        1.0 * x1 in Interval(-Inf, 1.0)
        1.0 * x1 in Interval(2.0, Inf)
        1.0 * x1 in Interval(3.0, 4.0)
        """,
        """
        NAME
        ROWS
         N  OBJ
         N  c1
         L  c2
         G  c3
         L  c4
        COLUMNS
            x1        c1        1
            x1        c2        1
            x1        c3        1
            x1        c4        1
        RHS
            rhs       c2        1
            rhs       c3        2
            rhs       c4        4
        RANGES
            rhs       c4        1
        BOUNDS
         FR bounds    x1
        ENDATA
        """,
    )
    return
end

function test_quadobj_gurobi()
    _test_write_to_file(
        """
        variables: x, y
        minobjective: x + y + 5.0 * x * x + 1.0 * x * y + 1.0 * y * x + 1.2 * y * y
        """,
        """
        NAME
        ROWS
         N  OBJ
        COLUMNS
            x         OBJ       1
            y         OBJ       1
        RHS
        RANGES
        BOUNDS
         FR bounds    x
         FR bounds    y
        QUADOBJ
            x         x         10
            x         y         2
            y         y         2.4
        ENDATA
        """,
    )
    return
end

function test_quadobj_cplex()
    _test_write_to_file(
        """
        variables: x, y
        minobjective: x + y + 5.0 * x * x + 1.0 * x * y + 1.0 * y * x + 1.2 * y * y
        """,
        """
        NAME
        ROWS
         N  OBJ
        COLUMNS
            x         OBJ       1
            y         OBJ       1
        RHS
        RANGES
        BOUNDS
         FR bounds    x
         FR bounds    y
        QUADOBJ
            x         x         10
            x         y         2
            y         y         2.4
        ENDATA
        """;
        quadratic_format = MPS.kQuadraticFormatCPLEX,
    )
    return
end

function test_quadcon_gurobi()
    _test_write_to_file(
        """
        variables: x, y
        c1: x + y + 5.0 * x * x + 1.0 * x * y + 1.0 * y * x + 1.2 * y * y <= 1.0
        """,
        """
        NAME
        ROWS
         N  OBJ
         L  c1
        COLUMNS
            x         c1        1
            y         c1        1
        RHS
            rhs       c1        1
        RANGES
        BOUNDS
         FR bounds    x
         FR bounds    y
        QCMATRIX   c1
            x         x         5
            x         y         1
            y         x         1
            y         y         1.2
        ENDATA
        """,
    )
    return
end

function test_quadcon_cplex()
    _test_write_to_file(
        """
        variables: x, y
        c1: x + y + 5.0 * x * x + 1.0 * x * y + 1.0 * y * x + 1.2 * y * y <= 1.0
        """,
        """
        NAME
        ROWS
         N  OBJ
         L  c1
        COLUMNS
            x         c1        1
            y         c1        1
        RHS
            rhs       c1        1
        RANGES
        BOUNDS
         FR bounds    x
         FR bounds    y
        QCMATRIX   c1
            x         x         10
            x         y         2
            y         x         2
            y         y         2.4
        ENDATA
        """;
        quadratic_format = MPS.kQuadraticFormatCPLEX,
    )
    return
end

function test_round_trip_quadobj_gurobi()
    _test_model_equality(
        """
        variables: x, y
        minobjective: 1.2x + 2.1 * x * x + 1.2 * x * y + 0.2 * y * x + 0.5 * y * y
        """,
        ["x", "y"],
        String[],
    )
    return
end

function test_round_trip_qmatrix_cplex()
    _test_model_equality(
        """
        variables: x, y
        minobjective: 1.2x + 2.1 * x * x + 1.2 * x * y + 0.2 * y * x + 0.5 * y * y
        """,
        ["x", "y"],
        String[];
        quadratic_format = MPS.kQuadraticFormatCPLEX,
    )
    return
end

function test_round_trip_qcmatrix_gurobi()
    _test_model_equality(
        """
        variables: x, y
        minobjective: 1.3 * x * x + 0.5 * x * y
        c1: 1.2x + 2.1 * x * x + 1.2 * x * y + 0.2 * y * x + 0.5 * y * y <= 1.0
        """,
        ["x", "y"],
        ["c1"],
    )
    return
end

function test_round_trip_qcmatrix_cplex()
    _test_model_equality(
        """
        variables: x, y
        minobjective: 1.3 * x * x + 0.5 * x * y
        c1: 1.2x + 2.1 * x * x + 1.2 * x * y + 0.2 * y * x + 0.5 * y * y <= 1.0
        """,
        ["x", "y"],
        ["c1"];
        quadratic_format = MPS.kQuadraticFormatCPLEX,
    )
    return
end

function test_round_trip_qcmatrix_mosek()
    _test_model_equality(
        """
        variables: x, y
        minobjective: 1.3 * x * x + 0.5 * x * y
        c1: 1.2x + 2.1 * x * x + 1.2 * x * y + 0.2 * y * x + 0.5 * y * y <= 1.0
        """,
        ["x", "y"],
        ["c1"];
        quadratic_format = MPS.kQuadraticFormatMosek,
    )
    return
end

function test_round_trip_indicator_lessthan()
    _test_model_equality(
        """
        variables: x, y, z
        minobjective: 1.0 * x + y + z
        z in ZeroOne()
        c1: [z, 1.0 * x + 1.0 * y] in Indicator{ACTIVATE_ON_ONE}(LessThan(1.0))
        """,
        ["x", "y", "z"],
        ["c1"],
        [("z", MOI.ZeroOne())],
    )
    return
end

function test_round_trip_indicator_greaterthan()
    _test_model_equality(
        """
        variables: x, y, z
        minobjective: 1.0 * x + y + z
        z in ZeroOne()
        c1: [z, 1.0 * x + 1.0 * y] in Indicator{ACTIVATE_ON_ONE}(GreaterThan(1.0))
        """,
        ["x", "y", "z"],
        ["c1"],
        [("z", MOI.ZeroOne())],
    )
    return
end

function test_round_trip_indicator_equalto()
    _test_model_equality(
        """
        variables: x, y, z
        minobjective: 1.0 * x + y + z
        z in ZeroOne()
        c1: [z, 1.0 * x + 1.0 * y] in Indicator{ACTIVATE_ON_ONE}(EqualTo(1.0))
        """,
        ["x", "y", "z"],
        ["c1"],
        [("z", MOI.ZeroOne())],
    )
    return
end

function test_round_trip_indicator_lessthan_false()
    _test_model_equality(
        """
        variables: x, y, z
        minobjective: 1.0 * x + y + z
        z in ZeroOne()
        c1: [z, 1.0 * x + 1.0 * y] in Indicator{ACTIVATE_ON_ZERO}(LessThan(1.0))
        """,
        ["x", "y", "z"],
        ["c1"],
        [("z", MOI.ZeroOne())],
    )
    return
end

function test_round_trip_indicator_greaterthan_false()
    _test_model_equality(
        """
        variables: x, y, z
        minobjective: 1.0 * x + y + z
        z in ZeroOne()
        c1: [z, 1.0 * x + 1.0 * y] in Indicator{ACTIVATE_ON_ZERO}(GreaterThan(1.0))
        """,
        ["x", "y", "z"],
        ["c1"],
        [("z", MOI.ZeroOne())],
    )
    return
end

function test_round_trip_indicator_equalto_false()
    _test_model_equality(
        """
        variables: x, y, z
        minobjective: 1.0 * x + y + z
        z in ZeroOne()
        c1: [z, 1.0 * x + 1.0 * y] in Indicator{ACTIVATE_ON_ZERO}(EqualTo(1.0))
        """,
        ["x", "y", "z"],
        ["c1"],
        [("z", MOI.ZeroOne())],
    )
    return
end

function test_vector_supports_constraint()
    model = MPS.Model()
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{Float64}},
    )
    @test !MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.SOS1{Float64},
    )
    return
end

function test_objsense_next_line()
    for (sense, enum) in [("MIN", MOI.MIN_SENSE), ("MAX", MOI.MAX_SENSE)]
        io = IOBuffer()
        write(
            io,
            """
            NAME          model
            OBJSENSE
                $sense
            ROWS
            N  Obj
            """,
        )
        seekstart(io)
        model = MPS.Model()
        Base.read!(io, model)
        @test MOI.get(model, MOI.ObjectiveSense()) == enum
    end
    for (sense, enum) in [("MIN", MOI.MIN_SENSE), ("MAX", MOI.MAX_SENSE)]
        io = IOBuffer()
        write(
            io,
            """
            NAME          model
            OBJSENSE $sense
            ROWS
            N  Obj
            """,
        )
        seekstart(io)
        model = MPS.Model()
        Base.read!(io, model)
        @test MOI.get(model, MOI.ObjectiveSense()) == enum
    end
    for (sense, enum) in [("MIN", MOI.MIN_SENSE), ("MAX", MOI.MAX_SENSE)]
        io = IOBuffer()
        write(
            io,
            """
            NAME          model
            OBJSENSE      $sense
            ROWS
            N  Obj
            """,
        )
        seekstart(io)
        model = MPS.Model()
        Base.read!(io, model)
        @test MOI.get(model, MOI.ObjectiveSense()) == enum
    end
    return
end

function test_parse_name_line()
    data = MPS.TempMPSModel{Float64}()
    for (line, name) in (
        "NAME" => "",
        "NAME   " => "",
        "NAME   \n" => "",
        "NAmE" => "",
        "NaME   " => "",
        "name   \n" => "",
        "name abc" => "abc",
        "NAME PILOTNOV (PILOTS) INTEGRATED MODEL -- NOVEMBER 1979" => "PILOTNOV (PILOTS) INTEGRATED MODEL -- NOVEMBER 1979",
        "Na d" => nothing,
        "the name" => nothing,
        " NAME" => "",
        " NAME foo" => "foo",
        "" => nothing,
    )
        data.name = "_"
        if name === nothing
            @test_throws MPS.ParseError MPS.parse_name_line(data, line)
        else
            MPS.parse_name_line(data, line)
            @test data.name == name
        end
    end
    return
end

function test_binary_with_unneeded_bounds()
    target = """
    NAME
    ROWS
     N  OBJ
    COLUMNS
        MARKER    'MARKER'                 'INTORG'
        x         OBJ       1
        MARKER    'MARKER'                 'INTEND'
    RHS
    RANGES
    BOUNDS
     BV bounds    x
    ENDATA
    """
    for test in [
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        """,
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        x >= 0.0
        x <= 1.0
        """,
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        x in Interval(-0.1, 1.2)
        """,
    ]
        _test_write_to_file(test, target)
    end
    return
end

function test_binary_with_restrictive_bounds()
    target = """
    NAME
    ROWS
     N  OBJ
    COLUMNS
        MARKER    'MARKER'                 'INTORG'
        x         OBJ       1
        MARKER    'MARKER'                 'INTEND'
    RHS
    RANGES
    BOUNDS
     FX bounds    x         1
    ENDATA
    """
    for test in [
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        x >= 0.5
        """,
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        x >= 0.1
        x <= 1.0
        """,
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        x in Interval(0.2, 1.2)
        """,
    ]
        _test_write_to_file(test, target)
    end
    return
end

function test_binary_with_infeasible_bounds()
    target = """
    NAME
    ROWS
     N  OBJ
    COLUMNS
        MARKER    'MARKER'                 'INTORG'
        x         OBJ       1
        MARKER    'MARKER'                 'INTEND'
    RHS
    RANGES
    BOUNDS
     LI bounds    x         1
     UI bounds    x         0
    ENDATA
    """
    for test in [
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        x >= 0.5
        x <= 0.1
        """,
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        x >= 1.0
        x <= 0.0
        """,
        """
        variables: x
        minobjective: 1.0 * x
        x in ZeroOne()
        x in Interval(0.9, 0.1)
        """,
    ]
        _test_write_to_file(test, target)
    end
    return
end

function test_issue_2538()
    model = MPS.Model()
    x = MOI.add_variables(model, 2)
    f = 1.0 * x[1] + 2.0 * x[2] + 3.0 * x[1] * x[2] + 4.0 * x[2] * x[2]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    io = IOBuffer()
    write(io, model)
    model_2 = MPS.Model()
    seekstart(io)
    read!(io, model_2)
    MOI.get(model_2, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    y = MOI.get(model_2, MOI.ListOfVariableIndices())
    g = MOI.get(model_2, MOI.ObjectiveFunction{typeof(f)}())
    @test isapprox(
        g,
        -1.0 * y[1] - 2.0 * y[2] - 3.0 * y[1] * y[2] - 4.0 * y[2] * y[2],
    )
    return
end

function test_qmatrix_objective()
    file = """
    NAME
    ROWS
        N  OBJ
    COLUMNS
        x         OBJ        1
        y         OBJ        1
    RHS
    RANGES
    BOUNDS
        FR bounds    x
        FR bounds    y
    QMATRIX
        x         x         10
        x         y         2.0
        y         x         2.0
        y         y         2.0
    ENDATA
    """
    io = IOBuffer()
    print(io, file)
    seekstart(io)
    model = MPS.Model()
    read!(io, model)
    x, y = MOI.get.(model, MOI.VariableIndex, ["x", "y"])
    F = MOI.ScalarQuadraticFunction{Float64}
    @test isapprox(
        MOI.get(model, MOI.ObjectiveFunction{F}()),
        1.0 * x + 1.0 * y + 5.0 * x * x + 2.0 * x * y + 1.0 * y * y,
    )
    return
end

function test_qsection_objective()
    file = """
    NAME
    ROWS
        N  OBJ
    COLUMNS
        x         OBJ        1
        y         OBJ        1
    RHS
    RANGES
    BOUNDS
        FR bounds    x
        FR bounds    y
    QSECTION      OBJ
        x         x         10
        x         y         2.0
        y         y         2.0
    ENDATA
    """
    io = IOBuffer()
    print(io, file)
    seekstart(io)
    model = MPS.Model()
    read!(io, model)
    x, y = MOI.get.(model, MOI.VariableIndex, ["x", "y"])
    F = MOI.ScalarQuadraticFunction{Float64}
    @test isapprox(
        MOI.get(model, MOI.ObjectiveFunction{F}()),
        1.0 * x + 1.0 * y + 5.0 * x * x + 2.0 * x * y + 1.0 * y * y,
    )
    return
end

function test_qsection_row()
    file = """
    NAME
    ROWS
        N  OBJ
        L  c1
    COLUMNS
        x         c1        1
        y         c1        1
    RHS
        rhs       c1        1
    RANGES
    BOUNDS
        FR bounds    x
        FR bounds    y
    QSECTION      c1
        x         x         10
        x         y         2.0
        y         y         2.0
    ENDATA
    """
    io = IOBuffer()
    print(io, file)
    seekstart(io)
    model = MPS.Model()
    read!(io, model)
    x, y = MOI.get.(model, MOI.VariableIndex, ["x", "y"])
    c1 = MOI.get(model, MOI.ConstraintIndex, "c1")
    @test isapprox(
        MOI.get(model, MOI.ConstraintFunction(), c1),
        1.0 * x + 1.0 * y + 10.0 * x * x + 4.0 * x * y + 2.0 * y * y,
    )
    return
end

function test_qcmatrix_read_gurobi()
    file = """
    NAME
    ROWS
        N  OBJ
        L  c1
    COLUMNS
        x         c1        1
        y         c1        1
    RHS
        rhs       c1        1
    RANGES
    BOUNDS
        FR bounds    x
        FR bounds    y
    QCMATRIX   c1
        x         x         10
        x         y         2.0
        y         x         2.0
        y         y         2.0
    ENDATA
    """
    io = IOBuffer()
    print(io, file)
    seekstart(io)
    model = MPS.Model()
    read!(io, model)
    x, y = MOI.get.(model, MOI.VariableIndex, ["x", "y"])
    c1 = MOI.get(model, MOI.ConstraintIndex, "c1")
    @test isapprox(
        MOI.get(model, MOI.ConstraintFunction(), c1),
        1.0 * x + 1.0 * y + 10.0 * x * x + 4.0 * x * y + 2.0 * y * y,
    )
    return
end

function test_qcmatrix_read_cplex()
    file = """
    NAME
    ROWS
        N  OBJ
        L  c1
    COLUMNS
        x         c1        1
        y         c1        1
    RHS
        rhs       c1        1
    RANGES
    BOUNDS
        FR bounds    x
        FR bounds    y
    QCMATRIX   c1
        x         x         1.0
        x         y         2.0
        y         x         2.0
        y         y         7.0
    ENDATA
    """
    io = IOBuffer()
    print(io, file)
    seekstart(io)
    model = MPS.Model(; quadratic_format = MPS.kQuadraticFormatCPLEX)
    read!(io, model)
    x, y = MOI.get.(model, MOI.VariableIndex, ["x", "y"])
    c1 = MOI.get(model, MOI.ConstraintIndex, "c1")
    @test isapprox(
        MOI.get(model, MOI.ConstraintFunction(), c1),
        1.0 * x + 1.0 * y + 0.5 * x * x + 2.0 * x * y + 3.5 * y * y,
    )
    return
end

function test_model_Name()
    model = MPS.Model()
    MOI.set(model, MOI.Name(), "TestFoo")
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    contents = read(io, String)
    @test occursin("NAME          TestFoo", contents)
    return
end

function test_model_large_integer()
    model = MPS.Model()
    x = MOI.add_constrained_variable(model, MOI.GreaterThan(123.0))
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    contents = read(io, String)
    @test occursin("LO bounds    x1        123", contents)
    return
end

function test_model_not_empty()
    model = MPS.Model()
    x = MOI.add_variable(model)
    io = IOBuffer()
    @test_throws(
        ErrorException("Cannot read in file because model is not empty."),
        read!(io, model),
    )
    return
end

function test_rhs_free_row()
    io = IOBuffer("""
    NAME
    ROWS
        N  OBJ
        N  c
    COLUMNS
        x         OBJ        1
        x         c          1
    RHS
        rhs       c          1
    RANGES
    BOUNDS
        FR bounds x
    ENDATA
    """)
    model = MPS.Model()
    @test_throws MPS.ParseError read!(io, model)
    return
end

function test_malformed_indicator()
    io = IOBuffer("""
    NAME
    ROWS
        N  OBJ
    COLUMNS
        x         OBJ        1
        y         OBJ        1
    RHS
    RANGES
    BOUNDS
        FR bounds x
        BV bounds y
    INDICATORS
     IF c1        y
    ENDATA
    """)
    model = MPS.Model()
    @test_throws MPS.ParseError read!(io, model)
    return
end

function test_unsupported_variable_types()
    model = MPS.Model()
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Parameter(2.0)),
    )
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Semicontinuous(2.0, 3.0)),
    )
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Semiinteger(2.0, 3.0)),
    )
    return
end

function _test_int_round_trip(src)
    model = MOI.FileFormats.MPS.Model(; coefficient_type = Int)
    io = IOBuffer()
    write(io, src)
    seekstart(io)
    read!(io, model)
    seekstart(io)
    write(io, model)
    seekstart(io)
    file = read(io, String)
    @test file == src
    return
end

function test_int_round_trip()
    for src in [
        """
        NAME
        ROWS
         N  OBJ
         G  c
        COLUMNS
            x         c         3
            x         OBJ       2
        RHS
            rhs       c         2
            rhs       OBJ       -3
        RANGES
        BOUNDS
         LO bounds    x         1
         PL bounds    x
        ENDATA
        """,
        """
        NAME
        ROWS
         N  OBJ
         L  c1
        COLUMNS
            x         c1        1
            y         c1        1
        RHS
            rhs       c1        1
        RANGES
        BOUNDS
         FR bounds    x
         FR bounds    y
        QCMATRIX   c1
            x         x         1
            x         y         2
            y         x         2
            y         y         7
        ENDATA
        """,
    ]
        _test_int_round_trip(src)
    end
    return
end

function test_obj_constant_min()
    model = MOI.FileFormats.MPS.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = 1.0 * x + 2.0
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    io = IOBuffer()
    write(io, model)
    dest = MOI.FileFormats.MPS.Model()
    seekstart(io)
    read!(io, dest)
    g = MOI.get(dest, MOI.ObjectiveFunction{typeof(f)}())
    @test g.constant == 2.0
    @test MOI.get(dest, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    return
end

function test_obj_constant_max_to_min()
    model = MOI.FileFormats.MPS.Model()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = 1.0 * x + 2.0
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    io = IOBuffer()
    write(io, model)
    dest = MOI.FileFormats.MPS.Model()
    seekstart(io)
    read!(io, dest)
    g = MOI.get(dest, MOI.ObjectiveFunction{typeof(f)}())
    @test g.constant == -2.0
    @test MOI.get(dest, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    return
end

function test_obj_constant_max_to_max()
    model = MOI.FileFormats.MPS.Model(; print_objsense = true)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = 1.0 * x + 2.0
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    io = IOBuffer()
    write(io, model)
    dest = MOI.FileFormats.MPS.Model()
    seekstart(io)
    read!(io, dest)
    g = MOI.get(dest, MOI.ObjectiveFunction{typeof(f)}())
    @test g.constant == 2.0
    @test MOI.get(dest, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    return
end

function test_duplicate_coefficient()
    model = MPS.Model()
    MOI.read_from_file(model, joinpath(@__DIR__, "duplicate_coefficient.mps"))
    dest = MOI.Utilities.Model{Float64}()
    MOI.copy_to(dest, model)
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}
    x = only(MOI.get(dest, MOI.ListOfVariableIndices()))
    c = only(MOI.get(dest, MOI.ListOfConstraintIndices{F,S}()))
    f = MOI.get(dest, MOI.ConstraintFunction(), c)
    @test isapprox(f, 2.0 * x)
    @test MOI.get(dest, MOI.ConstraintSet(), c) == MOI.EqualTo(1.0)
    return
end

function test_issue_2792()
    src = """
    NAME
    ROWS
     N  OBJ       \$t3       0
    COLUMNS
        x         OBJ       2
    RHS
        rhs       OBJ       -3
    BOUNDS
     LO bounds    x         1
     PL bounds    x
    ENDATA
    """
    model = MPS.Model()
    read!(IOBuffer(src), model)
    dest = MOI.Utilities.Model{Float64}()
    MOI.copy_to(dest, model)
    @test MOI.get(dest, MOI.ListOfConstraintTypesPresent()) ==
          [(MOI.VariableIndex, MOI.GreaterThan{Float64})]
    x = only(MOI.get(dest, MOI.ListOfVariableIndices()))
    F = MOI.get(dest, MOI.ObjectiveFunctionType())
    f = MOI.get(dest, MOI.ObjectiveFunction{F}())
    @test isapprox(f, 2.0 * x + 3.0)
    return
end

function test_issue_2797_tab()
    @test MPS.line_to_items("a b") == ["a", "b"]
    @test MPS.line_to_items(" a b") == ["a", "b"]
    @test MPS.line_to_items("a\tb") == ["a", "b"]
    @test MPS.line_to_items("a\tb") == ["a", "b"]
    @test MPS.line_to_items("a\t b") == ["a", "b"]
    @test MPS.line_to_items(" a \t b      c ") == ["a", "b", "c"]
    return
end

function test_unsupported_objectives()
    model = MPS.Model()
    for (F, ret) in [
        MOI.VariableIndex => true,
        MOI.ScalarAffineFunction{Float64} => true,
        MOI.ScalarQuadraticFunction{Float64} => true,
        MOI.ScalarNonlinearFunction => false,
        MOI.VectorOfVariables => false,
        MOI.VectorAffineFunction{Float64} => false,
        MOI.VectorQuadraticFunction{Float64} => false,
        MOI.VectorNonlinearFunction => false,
    ]
        @test MOI.supports(model, MOI.ObjectiveFunction{F}()) == ret
    end
    return
end

end  # TestMPS

TestMPS.runtests()
