```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# SymbolicAD

The `Nonlinear.SymbolicAD` submodule contains data structures and functions for
working with the symbolic derivatives of a nonlinear optimization problem. This
page explains the API and describes the rationale behind its design.

## Background

The code in `SymbolicAD` is inspired by Hassan Hijazi's work on
[coin-or/gravity](https://github.com/coin-or/Gravity), a high-performance
algebraic modeling language in C++.

Hassan made the following observations:

 * For large scale models, symbolic differentiation is slower than other
   automatic differentiation techniques, such as the reverse mode algorithm
   implemented in `MOI.Nonlinear.ReverseAD`.
 * However, most large-scale nonlinear programs have a lot of structure.
 * Gravity asks the user to provide structure in the form of
   _template constraints_, where the user gives the symbolic form of the
   constraint as well as a set of data to convert from a symbolic form to the
   numerical form.
 * Instead of differentiating each constraint in its numerical form, we can
   compute one symbolic derivative of the constraint in symbolic form, and then
   plug in the data in to get the numerical derivative of each function.
 * As a final step, if users don't provide the structure, we can still infer it
   --perhaps with less accuracy--by comparing the expression tree of each
   constraint.

The symbolic differentiation approach of Gravity works well when the problem is
large with few unique constraints. For example, a model like:
```julia
model = Model()
@variable(model, 0 <= x[1:10_000] <= 1)
@constraint(model, [i=1:10_000], sin(x[i]) <= 1)
@objective(model, Max, sum(x))
```
is ideal, because although the Jacobian matrix has 10,000 rows, we can compute
the derivative of `sin(x[i])` as `cos(x[i])`, and then fill in the Jacobian by
evaluating the derivative function instead of having to differentiation 10,000
expressions.

The symbolic differentiation approach of Gravity works poorly if there are a
large number of unique constraints in the model (which would require a lot of
expressions to be symbolically differentiated), or if the nonlinear functions
contain a large number of nonlinear terms (which would make the symbolic
derivative expensive to compute).

`SymbolicAD` started life as [MathOptSymbolicAD.jl](https://github.com/lanl-ansi/MathOptSymbolicAD.jl),
development of which began in early 2022. This initial version of `SymbolicAD`
used the `Symbolics.jl` package to compute the symbolic derivatives. In 2025, we
rewrote MathOptSymbolicAD.jl to remove the dependence on `Symbolics.jl`, and,
since the rewrite depended only on MathOptInterface, we added it to
`MOI.Nonlinear` as a new submodule.

For more details on `MathOptSymbolicAD.jl`, see Oscar's [JuMP-dev 2022 talk](https://www.youtube.com/watch?v=d_X3gj3Iz-k),
although note that the syntax has changed since the original recording.

## Use SymbolicAD with JuMP

To use `SymbolicAD` with JuMP, set the [`AutomaticDifferentiationBackend`](@ref)
attribute to [`Nonlinear.SymbolicMode`](@ref):

```julia
using JuMP, Ipopt
model = Model(Ipopt.Optimizer)
set_attribute(
    model,
    MOI.AutomaticDifferentiationBackend(),
    MOI.Nonlinear.SymbolicMode(),
)
@variable(model, x[1:2])
@objective(model, Min, (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2)
optimize!(model)
```

To revert back to the default sparse reverse mode algorithm, set the
[`AutomaticDifferentiationBackend`](@ref) attribute to
[`Nonlinear.SparseReverseMode`](@ref).

## [`simplify`](@id symbolic_ad_manual_simplify)

Use [`Nonlinear.SymbolicAD.simplify`](@ref) to simplify nonlinear expressions.
The simplification algorithm performs simple rewrites such as lifting nested
summations:

```jldoctest
julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> f = MOI.ScalarNonlinearFunction(
           :+,
           Any[MOI.ScalarNonlinearFunction(:+, Any[1.0, x]), 2.0 * x + 3.0],
       )
+(+(1.0, MOI.VariableIndex(1)), 3.0 + 2.0 MOI.VariableIndex(1))

julia> MOI.Nonlinear.SymbolicAD.simplify(f)
4.0 + 3.0 MOI.VariableIndex(1)
```

and trivial identities such as ``x^1 = x``:

```jldoctest
julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> f = MOI.ScalarNonlinearFunction(:^, Any[x, 1])
^(MOI.VariableIndex(1), (1))

julia> MOI.Nonlinear.SymbolicAD.simplify(f)
MOI.VariableIndex(1)
```

The list of rewrites that will be made is intentionally limited to keep the
codebase simple. `Nonlinear.SymbolicAD` is not a substitute for a Computer
Algebraic System (CAS). For example, we do not detect the relationship
``sin(x)^2+cos(x)^2=1``:

```jldoctest
julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> sin_x = MOI.ScalarNonlinearFunction(:sin, Any[x])
sin(MOI.VariableIndex(1))

julia> cos_x = MOI.ScalarNonlinearFunction(:cos, Any[x])
cos(MOI.VariableIndex(1))

julia> f = MOI.ScalarNonlinearFunction(
           :+,
           Any[
               MOI.ScalarNonlinearFunction(:^, Any[sin_x, 2]),
               MOI.ScalarNonlinearFunction(:^, Any[cos_x, 2]),
           ],
       )
+(^(sin(MOI.VariableIndex(1)), (2)), ^(cos(MOI.VariableIndex(1)), (2)))

julia> MOI.Nonlinear.SymbolicAD.simplify(f)
+(^(sin(MOI.VariableIndex(1)), (2)), ^(cos(MOI.VariableIndex(1)), (2)))
```

In addition to [`Nonlinear.SymbolicAD.simplify`](@ref), there is an in-place
version [`Nonlinear.SymbolicAD.simplify!`](@ref) that may make changes to the
existing function.

## [`variables`](@id symbolic_ad_manual_variables)

Use [`Nonlinear.SymbolicAD.variables`](@ref) to return a sorted list of the
variables that appear in the function:

```jldoctest
julia> x = MOI.VariableIndex.(1:3)
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)

julia> f = MOI.ScalarNonlinearFunction(:atan, Any[x[3], 2.0 * x[1]])
atan(MOI.VariableIndex(3), 0.0 + 2.0 MOI.VariableIndex(1))

julia> MOI.Nonlinear.SymbolicAD.variables(f)
2-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(3)
```

## [`derivative`](@id symbolic_ad_manual_derivative)

Use [`Nonlinear.SymbolicAD.derivative`](@ref) to compute the symbolic derivative
of a function with respect to a decision variable:

```jldoctest
julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> f = MOI.ScalarNonlinearFunction(:sin, Any[x])
sin(MOI.VariableIndex(1))

julia> MOI.Nonlinear.SymbolicAD.derivative(f, x)
cos(MOI.VariableIndex(1))
```

Note that the resultant expression can often be simplified. Thus, in most cases
you should call [`Nonlinear.SymbolicAD.simplify`](@ref) on the expression before
using it in other places:

```jldoctest
julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> f = MOI.ScalarNonlinearFunction(:sin, Any[x + 1.0])
sin(1.0 + 1.0 MOI.VariableIndex(1))

julia> df_dx = MOI.Nonlinear.SymbolicAD.derivative(f, x)
*(cos(1.0 + 1.0 MOI.VariableIndex(1)), 1.0)

julia> MOI.Nonlinear.SymbolicAD.simplify!(df_dx)
cos(1.0 + 1.0 MOI.VariableIndex(1))
```

## `gradient_and_hessian`

In some cases, you may want to compute the gradient and (sparse) Hessian matrix
of a function. One way to achieve this is by recursively calling
[`Nonlinear.SymbolicAD.derivative`](@ref) on the result of [`Nonlinear.SymbolicAD.derivative`](@ref)
for each variable in the list of [`Nonlinear.SymbolicAD.variables`](@ref). But,
to simplify the process, you should use [`Nonlinear.SymbolicAD.gradient_and_hessian`](@ref):

```jldoctest
julia> x = MOI.VariableIndex.(1:2);

julia> f = MOI.ScalarNonlinearFunction(:sin, Any[1.0 * x[1] + 2.0 * x[2]])
sin(0.0 + 1.0 MOI.VariableIndex(1) + 2.0 MOI.VariableIndex(2))

julia> y, ∇f, H, ∇²f = MOI.Nonlinear.SymbolicAD.gradient_and_hessian(f);

julia> y
2-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)

julia> ∇f
2-element Vector{Any}:
 cos(0.0 + 1.0 MOI.VariableIndex(1) + 2.0 MOI.VariableIndex(2))
 *(cos(0.0 + 1.0 MOI.VariableIndex(1) + 2.0 MOI.VariableIndex(2)), 2.0)

julia> H
3-element Vector{Tuple{Int64, Int64}}:
 (1, 1)
 (1, 2)
 (2, 2)

julia> ∇²f
3-element Vector{Any}:
 -(sin(0.0 + 1.0 MOI.VariableIndex(1) + 2.0 MOI.VariableIndex(2)))
 *(-(sin(0.0 + 1.0 MOI.VariableIndex(1) + 2.0 MOI.VariableIndex(2))), 2.0)
 *(-(sin(0.0 + 1.0 MOI.VariableIndex(1) + 2.0 MOI.VariableIndex(2))), 4.0)
```

where:

 * `y` is the list of variables that appear in `f`
 * `∇f` is the first partial derivatives of `f` with respect to each variable in
   `y`
 * `H` and `∇²f` form a sparse Hessian matrix, were `H` is the `(row, column)`
   index of each element, and the corresponding element in `∇²f` is the second
   partial derivative of `f` with respect to `y[row]` and `y[column]`.

Unlike [`Nonlinear.SymbolicAD.derivative`](@ref), the gradient and Hessian
expressions have already been simplified; you do not need to call
[`Nonlinear.SymbolicAD.simplify`](@ref).
