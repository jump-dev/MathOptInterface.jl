# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function _test_minlptests(
    filename::String,
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T <: Float64
    @requires _supports(config, MOI.optimize!)
    data = JSON.parsefile(filename)
    io = IOBuffer()
    write(io, JSON.json(data["model"]))
    seekstart(io)
    mof_model = MOI.FileFormats.MOF.Model(; use_nlp_block = false)
    read!(io, mof_model)
    index_map = MOI.copy_to(model, mof_model)
    MOI.optimize!(model)
    if data["is_feasible"]
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test ≈(MOI.get(model, MOI.ObjectiveValue()), data["objective"], config)
        for (name, value) in data["primal"]
            mof_x = MOI.get(mof_model, MOI.VariableIndex, name)
            @test ≈(
                MOI.get(model, MOI.VariablePrimal(), index_map[mof_x]),
                value,
                config,
            )
        end
    else
        status = MOI.get(model, MOI.TerminationStatus())
        @test status == config.infeasible_status
    end
    return
end

function _setup_minlptests(
    filename::String,
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    if !(T <: Float64)
        return
    end
    data = JSON.parsefile(filename)
    if data["is_feasible"]
        primal = map(data["model"]["variables"]) do v
            return data["primal"][v["name"]]
        end
        MOIU.set_mock_optimize!(
            model,
            mock -> MOIU.mock_optimize!(
                mock,
                config.optimal_status,
                (MOI.FEASIBLE_POINT, primal),
            ),
        )
    else
        MOIU.set_mock_optimize!(
            model,
            mock -> MOIU.mock_optimize!(mock, config.infeasible_status),
        )
    end
    return
end

for filename in readdir(joinpath(@__DIR__, "MINLPTests"))
    full_filename = joinpath(@__DIR__, "MINLPTests", filename)
    fn_name = Symbol("test_mi" * replace(filename, ".json" => ""))
    @eval begin
        function $fn_name(model::MOI.ModelLike, config::Config)
            _test_minlptests($full_filename, model, config)
            return
        end
        function setup_test(
            ::typeof($fn_name),
            model::MOIU.MockOptimizer,
            config::Config,
        )
            _setup_minlptests($full_filename, model, config)
            return
        end
    end
end
