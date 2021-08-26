```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Problem modification

In addition to adding and deleting constraints and variables, MathOptInterface
supports modifying, in-place, coefficients in the constraints and the objective
function of a model.

These modifications can be grouped into two categories:
 * modifications which replace the set of function of a constraint with a new
   set or function
 * modifications which change, in-place, a component of a function

!!! warning
    Solve `ModelLike` objects do not support problem modification.

## Modify the set of a constraint

Use [`set`](@ref) and [`ConstraintSet`](@ref) to modify the set of a constraint
by replacing it with a new instance of the same type.

```jldoctest modify_set; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variable(model))
julia> c = MOI.add_constraint(
           model,
           MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
           MOI.EqualTo(1.0),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64},MathOptInterface.EqualTo{Float64}}(1)

julia> MOI.set(model, MOI.ConstraintSet(), c, MOI.EqualTo(2.0));

julia> MOI.get(model, MOI.ConstraintSet(), c) == MOI.EqualTo(2.0)
true
```


However, the following will fail as the new set is of a different type to the
original set:
```julia
julia> MOI.set(model, MOI.ConstraintSet(), c, MOI.GreaterThan(2.0))
ERROR: [...]
```

### Special cases: set transforms

If our constraint is an affine inequality, then this corresponds to modifying
the right-hand side of a constraint in linear programming.

In some special cases, solvers may support efficiently changing the set of a
constraint (for example, from [`LessThan`](@ref) to [`GreaterThan`](@ref)). For
these cases, MathOptInterface provides the [`transform`](@ref) method.

The [`transform`](@ref) function returns a new constraint index, and the old
constraint index (i.e., `c`) is no longer valid.

```jldoctest transform_set; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variable(model))
julia> c = MOI.add_constraint(
           model,
           MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
           MOI.LessThan(1.0),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64},MathOptInterface.LessThan{Float64}}(1)

julia> new_c = MOI.transform(model, c, MOI.GreaterThan(2.0));

julia> MOI.is_valid(model, c)
false

julia> MOI.is_valid(model, new_c)
true
```

!!! note
    [`transform`](@ref) cannot be called with a set of the same type. Use
    [`set`](@ref) instead.

## Modify the function of a constraint

Use [`set`](@ref) and [`ConstraintFunction`](@ref) to modify the function of a
constraint by replacing it with a new instance of the same type.

```jldoctest modify_function; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variable(model))
julia> c = MOI.add_constraint(
           model,
           MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
           MOI.EqualTo(1.0),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64},MathOptInterface.EqualTo{Float64}}(1)

julia> new_f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0);

julia> MOI.set(model, MOI.ConstraintFunction(), c, new_f);

julia> MOI.get(model, MOI.ConstraintFunction(), c) ≈ new_f
true
```

However, the following will fail as the new function is of a different type to
the original function:
```julia
julia> MOI.set(model, MOI.ConstraintFunction(), c, x)
ERROR: [...]
```

## Modify constant term in a scalar function

Use [`modify`](@ref) and [`ScalarConstantChange`](@ref) to modify the constant
term in a [`ScalarAffineFunction`](@ref) or [`ScalarQuadraticFunction`](@ref).

```jldoctest scalar_constant_change; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variable(model))
julia> c = MOI.add_constraint(
           model,
           MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
           MOI.EqualTo(1.0),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64},MathOptInterface.EqualTo{Float64}}(1)

julia> MOI.modify(model, c, MOI.ScalarConstantChange(1.0));

julia> new_f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 1.0);

julia> MOI.get(model, MOI.ConstraintFunction(), c) ≈ new_f
true
```

!!! tip
    [`ScalarConstantChange`](@ref) can also be used to modify the objective
    function by passing an instance of [`ObjectiveFunction`](@ref) instead of
    the constraint index `c` as we saw above.

```jldoctest scalar_constant_change
julia> MOI.set(
           model,
           MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
           new_f,
       );

julia> MOI.modify(
           model,
           MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
           MOI.ScalarConstantChange(-1.0)
       );

julia> MOI.get(
           model,
           MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
       ) ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], -1.0)
true
```

## Modify constant terms in a vector function

Use [`modify`](@ref) and [`VectorConstantChange`](@ref) to modify the constant
vector in a [`VectorAffineFunction`](@ref) or [`VectorQuadraticFunction`](@ref).

```jldoctest vector_constant_change; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variable(model))
julia> c = MOI.add_constraint(
           model,
           MOI.VectorAffineFunction([
                   MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
                   MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x)),
               ],
               [0.0, 0.0],
           ),
           MOI.Nonnegatives(2),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64},MathOptInterface.Nonnegatives}(1)

julia> MOI.modify(model, c, MOI.VectorConstantChange([3.0, 4.0]));

julia> new_f = MOI.VectorAffineFunction(
           [
        MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
        MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x)),
           ],
           [3.0, 4.0],
       );

julia> MOI.get(model, MOI.ConstraintFunction(), c) ≈ new_f
true
```

## Modify affine coefficients in a scalar function

Use [`modify`](@ref) and [`ScalarCoefficientChange`](@ref) to modify the affine
coefficient of a [`ScalarAffineFunction`](@ref) or [`ScalarQuadraticFunction`](@ref).

```jldoctest scalar_coefficient_change; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variable(model))
julia> c = MOI.add_constraint(
           model,
           MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
           MOI.EqualTo(1.0),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64},MathOptInterface.EqualTo{Float64}}(1)

julia> MOI.modify(model, c, MOI.ScalarCoefficientChange(x, 2.0));

julia> new_f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 0.0);

julia> MOI.get(model, MOI.ConstraintFunction(), c) ≈ new_f
true
```

!!! tip
    [`ScalarCoefficientChange`](@ref) can also be used to modify the objective
    function by passing an instance of [`ObjectiveFunction`](@ref) instead of
    the constraint index `c` as we saw above.

## Modify affine coefficients in a vector function

Use [`modify`](@ref) and [`MultirowChange`](@ref) to modify a vector of affine
coefficients in a [`VectorAffineFunction`](@ref) or a [`VectorQuadraticFunction`](@ref).

```jldoctest multirow_change; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variable(model))
julia> c = MOI.add_constraint(
           model,
           MOI.VectorAffineFunction([
                   MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
                   MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x)),
               ],
               [0.0, 0.0],
           ),
           MOI.Nonnegatives(2),
       )
MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64},MathOptInterface.Nonnegatives}(1)

julia> MOI.modify(model, c, MOI.MultirowChange(x, [(1, 3.0), (2, 4.0)]));

julia> new_f = MOI.VectorAffineFunction(
           [
        MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(3.0, x)),
        MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(4.0, x)),
           ],
           [0.0, 0.0],
       );

julia> MOI.get(model, MOI.ConstraintFunction(), c) ≈ new_f
true
```
