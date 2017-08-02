# Inspired from JuMP/test/solvers.jl

function try_import(name::Symbol)
    try
        @eval import $name
        return true
    catch e
        return false
    end
end

# Load available solvers
csd = try_import(:CSDP)
sda = try_import(:SDPA)

# Create solver lists
contlinear_solvers = Tuple{MOI.AbstractSolver, Float64}[]
csd && push!(contlinear_solvers, (CSDP.CSDPSolver(), 1e-7))
sda && push!(contlinear_solvers, (SDPA.SDPASolver(), 1e-5))

contquadratic_solvers = Tuple{MOI.AbstractSolver, Float64}[]

contconic_solvers = Tuple{MOI.AbstractSolver, Float64}[]
csd && push!(contconic_solvers, (CSDP.CSDPSolver(), 1e-7))
sda && push!(contconic_solvers, (SDPA.SDPASolver(), 1e-5))

intlinear_solvers = Tuple{MOI.AbstractSolver, Float64}[]
