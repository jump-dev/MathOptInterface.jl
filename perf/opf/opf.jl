import MathOptInterface as MOI
ROSETTA_OPF_DIR = "/home/blegat/git/rosetta-opf"
include(joinpath(ROSETTA_OPF_DIR, "jump.jl"))
model = opf_model(parse_data(joinpath(ROSETTA_OPF_DIR, "data/opf_warmup.m")))
JuMP.set_attribute(model, "max_iter", 1)
JuMP.optimize!(model)
ipopt = JuMP.unsafe_backend(model)
ipopt.nlp_model
ad = ipopt.nlp_data.evaluator
x = ipopt.inner.x
g = zeros(length(ipopt.nlp_data.constraint_bounds))

function bench0(ad, x, y)
    if !isnothing(ad.backend.objective)
        @time MOI.eval_objective(ad, x)
    end
    @time MOI.eval_constraint(ad, g, x)
    return
end

function bench1(ad, x, y)
    J = @time MOI.jacobian_structure(ad)
    V = zeros(length(J))
    @time MOI.eval_constraint_jacobian(ad, V, x)
end

function bench2(ad, x, y)
    J = @time MOI.hessian_lagrangian_structure(ad)
    V = zeros(length(J))
    σ = 1.0
    @time MOI.eval_hessian_lagrangian(ad, V, x, σ, y)
end

function bench_opt(model)
    @time JuMP.optimize!(model)
end

JuMP.set_attribute(model, "max_iter", 1)
@profview_allocs JuMP.optimize!(model)

bench_opt(model)
bench0(ad, x, g)
bench1(ad, x, g)
bench2(ad, x, g)
