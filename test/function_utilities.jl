# Utilities for comparing functions
# Define isapprox so that we can use ≈ in tests

function Base.isapprox(f1::MOI.ScalarAffineFunction{T}, f2::MOI.ScalarAffineFunction{T}; rtol = sqrt(eps), atol = 0, nans = false) where {T}
    function canonicalize(f)
        d = Dict{MOI.VariableReference,T}()
        @assert length(f.variables) == length(f.coefficients)
        for k in 1:length(f.variables)
            d[f.variables[k]] = f.coefficients[k] + get(d, f.variables[k], zero(T))
        end
        return (d,f.constant)
    end
    d1, c1 = canonicalize(f1)
    d2, c2 = canonicalize(f2)
    for (var,coef) in d2
        d1[var] = get(d1,var,zero(T)) - coef
    end
    return isapprox([c2-c1;collect(values(d1))], zeros(T,length(d1)+1), rtol=rtol, atol=atol, nans=nans)
end

function Base.isapprox(f1::MOI.ScalarQuadraticFunction{T}, f2::MOI.ScalarQuadraticFunction{T}; rtol = sqrt(eps), atol = 0, nans = false) where {T}
    function canonicalize(f)
        affine_d = Dict{MOI.VariableReference,T}()
        @assert length(f.affine_variables) == length(f.affine_coefficients)
        for k in 1:length(f.affine_variables)
            affine_d[f.affine_variables[k]] = f.affine_coefficients[k] + get(affine_d, f.affine_variables[k], zero(T))
        end
        quadratic_d = Dict{Set{MOI.VariableReference},T}()
        @assert length(f.quadratic_rowvariables) == length(f.quadratic_coefficients)
        @assert length(f.quadratic_colvariables) == length(f.quadratic_coefficients)
        for k in 1:length(f.quadratic_rowvariables)
            quadratic_d[Set([f.quadratic_rowvariables[k],f.quadratic_colvariables[k]])] = f.quadratic_coefficients[k] + get(quadratic_d, Set([f.quadratic_rowvariables[k],f.quadratic_colvariables[k]]), zero(T))
        end
        return (quadratic_d,affine_d,f.constant)
    end
    quad_d1, aff_d1, c1 = canonicalize(f1)
    quad_d2, aff_d2, c2 = canonicalize(f2)
    for (var,coef) in aff_d2
        aff_d1[var] = get(aff_d1,var,zero(T)) - coef
    end
    for (vars,coef) in quad_d2
        quad_d1[vars] = get(quad_d1,vars,zero(T)) - coef
    end
    return isapprox([c2-c1;collect(values(aff_d1));collect(values(quad_d1))], zeros(T,length(quad_d1)+length(aff_d1)+1), rtol=rtol, atol=atol, nans=nans)
end
