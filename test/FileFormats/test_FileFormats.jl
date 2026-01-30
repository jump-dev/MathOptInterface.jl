# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestFileFormats

using Test

import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__, all = true)
        if startswith("$(name)", "test_")
            @testset "$name" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_default_fallbacks()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    set = MOI.GreaterThan(0.0)
    c = MOI.add_constraint(model, x, set)
    MOI.write_to_file(model, "tmp.lp")
    dest = MOI.Utilities.Model{Float64}()
    MOI.read_from_file(dest, "tmp.lp")
    x_dest = MOI.get(dest, MOI.VariableIndex, "x")
    @test MOI.get(dest, MOI.ConstraintSet(), typeof(c)(x_dest.value)) == set
    rm("tmp.lp")
    return
end

function test_copying()
    models = [
        MOI.FileFormats.CBF.Model,
        MOI.FileFormats.LP.Model,
        MOI.FileFormats.MOF.Model,
        MOI.FileFormats.MPS.Model,
        MOI.FileFormats.SDPA.Model,
    ]
    for src in models
        model_src = src()
        for dest in models
            model_dest = dest()
            MOI.copy_to(model_dest, model_src)
            @test !isempty(sprint(write, model_dest))
        end
        model_dest = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        MOI.copy_to(model_dest, model_src)
    end
end

function test_instantiate()
    models = [
        MOI.FileFormats.CBF.Model,
        MOI.FileFormats.LP.Model,
        MOI.FileFormats.MOF.Model,
        MOI.FileFormats.MPS.Model,
        MOI.FileFormats.SDPA.Model,
    ]
    for src in models
        model = MOI.instantiate(src)
        @test model isa src
        bridged = MOI.instantiate(src; with_bridge_type = Float64)
        @test bridged isa MOI.Bridges.LazyBridgeOptimizer
    end
    return
end

function test_compressed_open()
    for cs in [MOI.FileFormats.Bzip2(), MOI.FileFormats.Gzip()]
        for open_type in ["a", "r+", "w+", "a+"]
            @test_throws ArgumentError MOI.FileFormats.compressed_open(
                (x) -> nothing,
                "dummy.gz",
                open_type,
                cs,
            )
        end
    end
end

function test_compression()
    filename = joinpath(@__DIR__, "MPS", "free_integer.mps")
    model = MOI.FileFormats.Model(filename = filename)
    MOI.read_from_file(model, filename)
    filename = joinpath(@__DIR__, "free_integer.mps")
    MOI.write_to_file(model, filename * ".garbage")
    for ext in ["", ".bz2", ".gz"]
        filename_ext = filename * ext
        MOI.write_to_file(model, filename_ext)
        model2 = MOI.FileFormats.Model(filename = filename_ext)
        MOI.read_from_file(model2, filename_ext)
    end

    sleep(1.0)  # Allow time for unlink to happen.
    for ext in ["", ".garbage", ".bz2", ".gz"]
        rm(filename * ext, force = true)
    end
    return
end

function test_Model()
    for (format, model) in [
        (MOI.FileFormats.FORMAT_CBF, MOI.FileFormats.CBF.Model()),
        (MOI.FileFormats.FORMAT_LP, MOI.FileFormats.LP.Model()),
        (MOI.FileFormats.FORMAT_MOF, MOI.FileFormats.MOF.Model()),
        (MOI.FileFormats.FORMAT_MPS, MOI.FileFormats.MPS.Model()),
        (MOI.FileFormats.FORMAT_NL, MOI.FileFormats.NL.Model()),
        (MOI.FileFormats.FORMAT_SDPA, MOI.FileFormats.SDPA.Model()),
    ]
        @test typeof(
            MOI.FileFormats.Model(format = format, filename = "foo.bar"),
        ) == typeof(model)
    end
    @test_throws(
        ErrorException(
            "When `format==FORMAT_AUTOMATIC` you must pass a `filename`.",
        ),
        MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_AUTOMATIC)
    )
    for (ext, model) in [
        (".cbf", MOI.FileFormats.CBF.Model()),
        (".lp", MOI.FileFormats.LP.Model()),
        (".mof.json", MOI.FileFormats.MOF.Model()),
        (".mps", MOI.FileFormats.MPS.Model()),
        (".nl", MOI.FileFormats.NL.Model()),
        (".sdpa", MOI.FileFormats.SDPA.Model()),
    ]
        @test typeof(MOI.FileFormats.Model(filename = "a$(ext)")) ==
              typeof(model)
        @test typeof(MOI.FileFormats.Model(filename = "a$(ext).gz")) ==
              typeof(model)
    end
    @test_throws(
        ErrorException("Unable to automatically detect format of a.b."),
        MOI.FileFormats.Model(filename = "a.b")
    )
end

function test_generic_names()
    # These methods were added to MOI, but then we changed the REW model to not
    # need them.
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variables(model, 2)
    MOI.set.(model, MOI.VariableName(), x, "x")
    c = MOI.add_constraint.(model, 1.0 .* x, MOI.EqualTo(1.0))
    MOI.set.(model, MOI.ConstraintName(), c, "c")
    c2 = MOI.add_constraint(model, 1.0 * x[2], MOI.LessThan(1.0))
    MOI.set(model, MOI.ConstraintName(), c2, "R2")
    MOI.FileFormats.create_generic_names(model)
    @test MOI.get.(model, MOI.VariableName(), x) == ["C1", "C2"]
    c_name = MOI.get.(model, MOI.ConstraintName(), c)
    c2_name = MOI.get(model, MOI.ConstraintName(), c2)
    names = vcat(c_name, c2_name)
    # The names depend on the order that the constraints are parsed. Either is
    # okay.
    @test names == ["R1", "R2", "R3"] || names == ["R1", "R3", "R2"]
    return
end

function test_unique_names()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variables(model, 3)
    MOI.set.(model, MOI.VariableName(), x, "x")
    c = MOI.add_constraint.(model, 1.0 .* x, MOI.EqualTo(1.0))
    MOI.set.(model, MOI.ConstraintName(), c, "c")
    MOI.set(model, MOI.ConstraintName(), c[2], "c_1")
    MOI.FileFormats.create_unique_names(model)
    @test MOI.get.(model, MOI.VariableName(), x) == ["x", "x_1", "x_2"]
    @test MOI.get.(model, MOI.ConstraintName(), c) == ["c", "c_1", "c_2"]
    return
end

end

TestFileFormats.runtests()
