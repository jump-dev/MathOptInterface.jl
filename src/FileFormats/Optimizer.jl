# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

struct Optimizer{M<:MOI.ModelLike} <: MOI.AbstractOptimizer
    inner::M
end

function Optimizer(; kwargs...)
    model = Model(; kwargs...)
    return Optimizer{typeof(model)}(model)
end

MOI.is_empty(model::Optimizer) = MOI.is_empty(model.inner)

MOI.empty!(model::Optimizer) = MOI.empty!(model.inner)

MOI.add_variable(model::Optimizer) = MOI.add_variable(model.inner)

function MOI.supports(model::Optimizer, attr::MOI.AbstractModelAttribute)
    return MOI.supports(model.inner, attr)
end

function MOI.get(model::Optimizer, attr::MOI.AbstractModelAttribute)
    return MOI.get(model.inner, attr)
end

function MOI.set(model::Optimizer, attr::MOI.AbstractModelAttribute, value)
    return MOI.set(model.inner, attr, value)
end

function MOI.get(model::Optimizer, attr::MOI.AbstractOptimizerAttribute)
    return MOI.get(model.inner, attr)
end

function MOI.get(model::Optimizer, ::MOI.SolverName)
    return "FileFormats.Optimizer"
end

function MOI.supports_constraint(
    model::Optimizer,
    ::Type{F},
    ::Type{S},
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    return MOI.supports_constraint(model.inner, F, S)
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.AbstractFunction,
    s::MOI.AbstractSet,
)
    return MOI.add_constraint(model.inner, f, s)
end

function MOI.write_to_file(model::Optimizer, filename::String)
    return MOI.write_to_file(model.inner, filename)
end
