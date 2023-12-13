# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# This file implements models similar to the SDPA format. It gives a good
# example because it does not support a lot of functions, hence the need for
# a lot of bridges.

MOI.Utilities.@model(
    StandardSDPAModel,
    (),
    (MOI.EqualTo,),
    (MOI.Nonnegatives, MOI.PositiveSemidefiniteConeTriangle),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

function MOI.supports_constraint(
    ::StandardSDPAModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{
        <:Union{
            MOI.GreaterThan{T},
            MOI.LessThan{T},
            MOI.EqualTo{T},
            MOI.Interval{T},
            MOI.ZeroOne,
            MOI.Integer,
        },
    },
) where {T}
    return false
end

function MOI.supports_constraint(
    ::StandardSDPAModel{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Reals},
) where {T}
    return false
end

function MOI.supports_add_constrained_variables(
    ::StandardSDPAModel,
    ::Type{<:Union{MOI.Nonnegatives,MOI.PositiveSemidefiniteConeTriangle}},
)
    return true
end

function MOI.supports_add_constrained_variables(
    ::StandardSDPAModel,
    ::Type{MOI.Reals},
)
    return false
end

function MOI.supports(
    ::StandardSDPAModel{T},
    ::MOI.ObjectiveFunction{
        <:Union{
            MOI.VariableIndex,
            MOI.ScalarQuadraticFunction{T},
            MOI.ScalarNonlinearFunction,
            MOI.VectorOfVariables,
            MOI.VectorAffineFunction{T},
            MOI.VectorQuadraticFunction{T},
            MOI.VectorNonlinearFunction,
        },
    },
) where {T}
    return false
end

MOI.Utilities.@model(
    GeometricSDPAModel,
    (),
    (),
    (MOI.Zeros, MOI.Nonnegatives, MOI.PositiveSemidefiniteConeTriangle),
    (),
    (),
    (),
    (),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::GeometricSDPAModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{
        <:Union{
            MOI.GreaterThan{T},
            MOI.LessThan{T},
            MOI.EqualTo{T},
            MOI.Interval{T},
            MOI.ZeroOne,
            MOI.Integer,
        },
    },
) where {T}
    return false
end

function MOI.supports(
    ::GeometricSDPAModel{T},
    ::MOI.ObjectiveFunction{
        <:Union{MOI.VariableIndex,MOI.ScalarQuadraticFunction{T}},
    },
) where {T}
    return false
end
