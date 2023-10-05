# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    QuadratizeBridge{T}

`QuadratizeBridge` implements the following reformulations:

 * ``\\min \\{a^\\top x + b\\}`` into ``\\min\\{x^\\top \\mathbf{0} x + a^\\top x + b\\}``
 * ``\\max \\{a^\\top x + b\\}`` into ``\\max\\{x^\\top \\mathbf{0} x + a^\\top x + b\\}``

where `T` is the coefficient type of `0`.

## Source node

`QuadratizeBridge` supports:

 * [`MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}`](@ref)

## Target nodes

`QuadratizeBridge` creates:

 * One objective node: [`MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}`](@ref)
"""
const QuadratizeBridge{T,G} = FunctionConversionBridge{
    T,
    MOI.ScalarQuadraticFunction{T},
    G,
}

const Quadratize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{QuadratizeBridge{T},OT}
