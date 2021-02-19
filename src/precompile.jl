const __bodyfunction__ = Dict{Method,Any}()

# Find keyword "body functions" (the function that contains the body
# as written by the developer, called after all missing keyword-arguments
# have been assigned values), in a manner that doesn't depend on
# gensymmed names.
# `mnokw` is the method that gets called when you invoke it without
# supplying any keywords.
function __lookup_kwbody__(mnokw::Method)
    function getsym(arg)
        isa(arg, Symbol) && return arg
        @assert isa(arg, GlobalRef)
        return arg.name
    end

    f = get(__bodyfunction__, mnokw, nothing)
    if f === nothing
        fmod = mnokw.module
        # The lowered code for `mnokw` should look like
        #   %1 = mkw(kwvalues..., #self#, args...)
        #        return %1
        # where `mkw` is the name of the "active" keyword body-function.
        ast = Base.uncompressed_ast(mnokw)
        if isa(ast, Core.CodeInfo) && length(ast.code) >= 2
            callexpr = ast.code[end-1]
            if isa(callexpr, Expr) && callexpr.head == :call
                fsym = callexpr.args[1]
                if isa(fsym, Symbol)
                    f = getfield(fmod, fsym)
                elseif isa(fsym, GlobalRef)
                    if fsym.mod === Core && fsym.name === :_apply
                        f = getfield(mnokw.module, getsym(callexpr.args[2]))
                    elseif fsym.mod === Core && fsym.name === :_apply_iterate
                        f = getfield(mnokw.module, getsym(callexpr.args[3]))
                    else
                        f = getfield(fsym.mod, fsym.name)
                    end
                else
                    f = missing
                end
            else
                f = missing
            end
        else
            f = missing
        end
        __bodyfunction__[mnokw] = f
    end
    return f
end

function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},VariablePrimalStart,Vector{VariableIndex}})   # time: 0.07465396
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{VectorOfVariables, PositiveSemidefiniteConeTriangle}}})   # time: 0.06941362
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{_A} where _A})   # time: 0.06315293
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{VectorAffineFunction{Float64}, PositiveSemidefiniteConeSquare}}})   # time: 0.062237397
    let fbody = try __lookup_kwbody__(which(instantiate, (Type,))) catch missing end
        if !ismissing(fbody)
            precompile(fbody, (Type{Float64},Bool,typeof(instantiate),Type,))
        end
    end   # time: 0.05891838
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, GreaterThan{Float64}}}})   # time: 0.051366724
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, EqualTo{Float64}}}})   # time: 0.05011797
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{SingleVariable, Integer}}})   # time: 0.047489766
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{SingleVariable, ZeroOne}}})   # time: 0.043587953
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{VectorOfVariables, PositiveSemidefiniteConeTriangle}}})   # time: 0.04207921
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{ScalarQuadraticFunction{Float64}, LessThan{Float64}}}})   # time: 0.041189477
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{VectorAffineFunction{Float64}, PositiveSemidefiniteConeSquare}}})   # time: 0.039949134
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{VectorAffineFunction{Float64}, SecondOrderCone}}})   # time: 0.036591787
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{SingleVariable, GreaterThan{Float64}}}})   # time: 0.034482736
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, LessThan{Float64}}}})   # time: 0.032348134
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{VectorAffineFunction{Float64}, SecondOrderCone}}})   # time: 0.031941403
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{ScalarQuadraticFunction{Float64}, LessThan{Float64}}}})   # time: 0.029012412
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, EqualTo{Float64}}}})   # time: 0.028307851
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{SingleVariable, GreaterThan{Float64}}}})   # time: 0.0268003
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{SingleVariable, LessThan{Float64}}}})   # time: 0.026435265
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintFunction,Vector{ConstraintIndex{SingleVariable, EqualTo{Float64}}}})   # time: 0.026390325
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{SingleVariable, LessThan{Float64}}}})   # time: 0.025676165
    # TODO: Base.precompile(Tuple{Type{VectorQuadraticFunction},Core.Array{MathOptInterface.VectorAffineTerm{T}, 1},Core.Array{MathOptInterface.VectorQuadraticTerm{T}, 1},Core.Array{T, 1}})   # time: 0.02562763
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, GreaterThan{Float64}}}})   # time: 0.024989743
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{SingleVariable, Integer}}})   # time: 0.024261666
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, LessThan{Float64}}}})   # time: 0.024140714
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{SingleVariable, ZeroOne}}})   # time: 0.023562236
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},ConstraintSet,Vector{ConstraintIndex{SingleVariable, EqualTo{Float64}}}})   # time: 0.023147725
    Base.precompile(Tuple{Type{VectorAffineFunction{Float64}},VectorOfVariables})   # time: 0.020000555
    Base.precompile(Tuple{typeof(convert),Type{VectorAffineFunction{Float64}},SingleVariable})   # time: 0.008697798
    Base.precompile(Tuple{typeof(convert),Type{VectorAffineFunction{Float64}},ScalarAffineFunction{Float64}})   # time: 0.007289221
    Base.precompile(Tuple{typeof(convert),Type{ScalarAffineFunction{_A}} where _A,SingleVariable})   # time: 0.007221313
    Base.precompile(Tuple{typeof(convert),Type{ScalarAffineFunction{Float64}},SingleVariable})   # time: 0.006756334
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{VectorAffineFunction{Float64}, PositiveSemidefiniteConeSquare}}})   # time: 0.00662328
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractVariableAttribute,Vector{VariableIndex}})   # time: 0.006028682
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{VectorAffineFunction{Float64}, SecondOrderCone}}})   # time: 0.005972562
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{ScalarQuadraticFunction{Float64}, LessThan{Float64}}}})   # time: 0.00574646
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{VectorOfVariables, PositiveSemidefiniteConeTriangle}}})   # time: 0.005563445
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Array{ConstraintIndex{F, S}, 1} where {F, S}})   # time: 0.005523676
    # TODO: Base.precompile(Tuple{Type{VectorAffineFunction},Core.Array{MathOptInterface.VectorAffineTerm{T}, 1},Core.Array{T, 1}})   # time: 0.005085834
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{SingleVariable, EqualTo{Float64}}}})   # time: 0.00483048
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, EqualTo{Float64}}}})   # time: 0.004200286
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{SingleVariable, ZeroOne}}})   # time: 0.003953963
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, GreaterThan{Float64}}}})   # time: 0.00390162
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{SingleVariable, Integer}}})   # time: 0.003883528
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{SingleVariable, GreaterThan{Float64}}}})   # time: 0.003878481
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{ScalarAffineFunction{Float64}, LessThan{Float64}}}})   # time: 0.003820131
    Base.precompile(Tuple{typeof(get),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},AbstractConstraintAttribute,Vector{ConstraintIndex{SingleVariable, LessThan{Float64}}}})   # time: 0.00379635
    Base.precompile(Tuple{typeof(add_constrained_variables),MathOptInterface.Utilities.UniversalFallback{MathOptInterface.Utilities.Model{Float64}},Reals})   # time: 0.003498349
    Base.precompile(Tuple{Type{InvalidIndex},Union{ConstraintIndex{F, GreaterThan{T}}, ConstraintIndex{F, LessThan{T}}} where {T, F<:AbstractScalarFunction}})   # time: 0.002815791
    Base.precompile(Tuple{Type{InvalidIndex},ConstraintIndex{SingleVariable, S} where S})   # time: 0.001883546
    Base.precompile(Tuple{Type{InvalidIndex},ConstraintIndex{VectorOfVariables, S} where S})   # time: 0.001882338
    Base.precompile(Tuple{Type{SetAttributeNotAllowed},ObjectiveFunction})   # time: 0.001743249
    # TODO: Base.precompile(Tuple{Type{LessThan},T<:Core.Real})   # time: 0.001174288
    # TODO: Base.precompile(Tuple{Type{GreaterThan},T<:Core.Real})   # time: 0.001148143
    Base.precompile(Tuple{Type{SetAttributeNotAllowed},ObjectiveSense})   # time: 0.001080585
end
