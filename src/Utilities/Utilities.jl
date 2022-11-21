# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Utilities

using LinearAlgebra # For dot
using OrderedCollections # for OrderedDict in UniversalFallback

import MutableArithmetics
const MA = MutableArithmetics

using MathOptInterface
const MOI = MathOptInterface

const MOIU = MOI.Utilities # used in macro

const SVF = MOI.VariableIndex
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

const VI = MOI.VariableIndex
const CI{F,S} = MOI.ConstraintIndex{F,S}

function print_with_acronym(io::IO, s::AbstractString)
    return print(io, replace_acronym(s))
end

function replace_acronym(s::AbstractString)
    s = replace(s, "MathOptInterface.Utilities" => "MOIU")
    s = replace(s, "MathOptInterface.Bridges" => "MOIB")
    s = replace(s, "MathOptInterface.Test" => "MOIT")
    s = replace(s, "MathOptInterface" => "MOI")
    return s
end

"""
    _reverse_dict(src::AbstractDict)

Reverse dictionary `src` so that values of the new dictionary are keys of `src`
and vice-versa. Also the values of `src` are assumed to be unique.

    _reverse_dict(dest::AbstractDict, src::AbstractDict)

Reverse dictionary so that values of `src` are key of `dest` and vice-versa.
`dest` must be empty. Also the values of `src` are assumed to be unique.
"""
function _reverse_dict end

include("CleverDicts.jl")
include("DoubleDicts.jl")

include("functions.jl")
include("mutable_arithmetics.jl")
include("sets.jl")
include("constraints.jl")
include("copy.jl")
include("results.jl")
include("variables.jl")

include("objective_container.jl")
include("variables_container.jl")
include("free_variables.jl")
include("vector_of_constraints.jl")
include("struct_of_constraints.jl")
include("model.jl")
include("sparse_matrix.jl")
include("product_of_sets.jl")
include("matrix_of_constraints.jl")
include("parser.jl")
include("mockoptimizer.jl")
include("cachingoptimizer.jl")
include("universalfallback.jl")
include("print.jl")
include("feasibility_relaxation.jl")
include("lazy_iterators.jl")

include("precompile.jl")
_precompile_()

end # module
