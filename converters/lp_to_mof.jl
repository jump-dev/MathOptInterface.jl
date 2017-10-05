# Pkg.clone("https://github.com/odow/LPWriter.jl")
using LPWriter, MathOptFormat

const MOI = MathOptFormat.MathOptInterface
const MOF = MathOptFormat

function LPtoMOF(inputfile::String, outputfile::String)
    (A, collb, colub, c, rowlb, rowub, sense, colcat, sos, Q, modelname,
        colnames, rownames) = LPWriter.readlp(inputfile)

    m = MOF.MOFFile()
    v = MOI.addvariables!(m, length(c), colnames)

    #=
        Objective
    =#
    objsense = sense == :Min?MOI.MinSense:MOI.MaxSense
    if length(nonzeros(Q)) == 0 # linear objective
        MOI.setobjective!(m, objsense, MOI.ScalarAffineFunction(v, c, 0.0))
    else
        cols = zeros(Int, length(Q.nzval))
        col = 1
        for i in 1:length(cols)
            if Q.colptr[col+1] == i
                col += 1
            end
            cols[i] = col
        end
        MOI.setobjective!(m, objsense, MOI.ScalarQuadraticFunction(v, c, v[rowvals(Q)], cols, nonzeros(Q), 0.0))
    end

    #=
        Constraints
    =#
    At = A'
    Anz = nonzeros(At)
    Acv = rowvals(At)
    for (row, (lb, ub)) in enumerate(zip(rowlb, rowub))
        set = if lb == ub
            MOI.EqualTo(ub)
        elseif lb == -Inf && ub != Inf
            MOI.LessThan(ub)
        elseif lb != -Inf && ub == Inf
            MOI.GreaterThan(lb)
        else
            MOI.Interval(lb, ub)
        end
        idx = [i for i  in nzrange(At, row)]
        func = MOI.ScalarAffineFunction(v[Acv[idx]], Anz[idx], 0.0)
        MOI.addconstraint!(m, func, set, rownames[row])
    end

    #=
        Variable bounds
    =#
    for (i, (lb, ub)) in enumerate(zip(collb, colub))
        if lb != -Inf
            if ub != Inf
                # interval
                MOI.addconstraint!(m, v[i], MOI.Interval(lb, ub))
            else
                MOI.addconstraint!(m, v[i], MOI.GreaterThan(lb))
            end
        else
            if ub != Inf
                MOI.addconstraint!(m, v[i], MOI.LessThan(ub))
            else
                # free variable
            end
        end
    end

    #=
        Variable types
    =#
    for (i, cat) in enumerate(colcat)
        if cat == :Cont
        elseif cat == :Bin
            MOI.addconstraint!(m, v[i], MOI.ZeroOne())
        elseif cat == :Int
            MOI.addconstraint!(m, v[i], MOI.Integer())
        end
    end

    #=
        Handle Special Ordered Sets
    =#
    for s in sos
        if s.order == 1
            MOI.addconstraint!(m, v[s.indices], MOI.SOS1(s.weights))
        elseif s.order == 2
            MOI.addconstraint!(m, v[s.indices], MOI.SOS2(s.weights))
        end
    end

    #=
        Save the file
    =#
    MOF.save(outputfile, m, 1)
end

LPtoMOF(
    joinpath(Pkg.dir("LPWriter"), "test", "model2.lp"),
    joinpath(@__DIR__, "model2.mof.json")
)
