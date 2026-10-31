var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Introduction",
    "title": "Introduction",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#MathOptInterface-1",
    "page": "Introduction",
    "title": "MathOptInterface",
    "category": "section",
    "text": "MathOptInterface.jl is a standardized API for mathematical optimization solvers.Pages = [\"apimanual.md\", \"apireference.md\"]\nDepth = 3"
},

{
    "location": "apimanual.html#",
    "page": "Manual",
    "title": "Manual",
    "category": "page",
    "text": ""
},

{
    "location": "apimanual.html#Manual-1",
    "page": "Manual",
    "title": "Manual",
    "category": "section",
    "text": ""
},

{
    "location": "apimanual.html#Purpose-1",
    "page": "Manual",
    "title": "Purpose",
    "category": "section",
    "text": "Each mathematical optimization solver API has its own concepts and data structures for representing optimization models and obtaining results. However, it is often desirable to represent an instance of an optimization problem at a higher level so that it is easy to try using different solvers. MathOptInterface (MOI) is an abstraction layer designed to provide a unified interface to mathematical optimization solvers so that users do not need to understand multiple solver-specific APIs. MOI can be used directly, or through a higher-level modeling interface like JuMP.MOI has been designed to replace MathProgBase, which has been used by modeling packages such as JuMP and Convex.jl. This second-generation abstraction layer addresses a number of limitations of MathProgBase. MOI is designed to:Be simple and extensible, unifying linear, quadratic, and conic optimization, and seamlessly facilitate extensions to essentially arbitrary constraints and functions (e.g., indicator constraints, complementarity constraints, and piecewise linear functions)\nBe fast by allowing access to a solver\'s in-memory representation of a problem without writing intermediate files (when possible) and by using multiple dispatch and avoiding requiring containers of nonconcrete types\nAllow a solver to return multiple results (e.g., a pool of solutions)\nAllow a solver to return extra arbitrary information via attributes (e.g., variable- and constraint-wise membership in an irreducible inconsistent subset for infeasibility analysis)\nProvide a greatly expanded set of status codes explaining what happened during the optimization procedure\nEnable a solver to more precisely specify which problem classes it supports\nEnable both primal and dual warm starts\nEnable adding and removing both variables and constraints by indices that are not required to be consecutive\nEnable any modification that the solver supports to an existing model\nAvoid requiring the solver wrapper to store an additional copy of the problem dataThis manual introduces the concepts needed to understand MOI and give a high-level picture of how all of the pieces fit together. The primary focus is on MOI from the perspective of a user of the interface. At the end of the manual we have a section on Implementing a solver interface. The API Reference page lists the complete API.MOI does not export functions, but for brevity we often omit qualifying names with the MOI module. Best practice is to haveusing MathOptInterface\nconst MOI = MathOptInterfaceand prefix all MOI methods with MOI. in user code. If a name is also available in base Julia, we always explicitly use the module prefix, for example, with MOI.get."
},

{
    "location": "apimanual.html#Standard-form-problem-1",
    "page": "Manual",
    "title": "Standard form problem",
    "category": "section",
    "text": "The standard form problem is:beginalign\n     min_x in mathbbR^n  f_0(x)\n    \n     textst  f_i(x)  in mathcalS_i  i = 1 ldots m\nendalignwhere:the functions f_0 f_1 ldots f_m are specified by AbstractFunction objects\nthe sets mathcalS_1 ldots mathcalS_m are specified by AbstractSet objectsThe current function types are:SingleVariable: x_j, i.e., projection onto a single coordinate defined by a variable index j\nVectorOfVariables: projection onto multiple coordinates (i.e., extracting a subvector)\nScalarAffineFunction: a^T x + b, where a is a vector and b scalar\nVectorAffineFunction: A x + b, where A is a matrix and b is a vector\nScalarQuadraticFunction: frac12 x^T Q x + a^T x + b, where Q is a symmetric matrix, a is a vector, and b is a constant\nVectorQuadraticFunction: a vector of scalar-valued quadratic functionsExtensions for nonlinear programming are present but not yet well documented.MOI defines some commonly used sets, but the interface is extensible to other sets recognized by the solver.LessThan(upper):  x in mathbbR  x le mboxupper \nGreaterThan(lower):  x in mathbbR  x ge mboxlower \nEqualTo(value):  x in mathbbR  x = mboxvalue \nInterval(lower, upper):  x in mathbbR  x in mboxlowermboxupper \nReals(dimension): mathbbR^mboxdimension\nZeros(dimension): 0^mboxdimension\nNonnegatives(dimension):  x in mathbbR^mboxdimension  x ge 0 \nNonpositives(dimension):  x in mathbbR^mboxdimension  x le 0 \nSecondOrderCone(dimension):  (tx) in mathbbR^mboxdimension  t ge x_2 \nRotatedSecondOrderCone(dimension):  (tux) in mathbbR^mboxdimension  2tu ge x_2^2 tu ge 0 \nGeometricMeanCone(dimension):  (tx) in mathbbR^n+1  x ge 0 t le sqrtnx_1 x_2 cdots x_n  where n is dimension - 1\nExponentialCone():  (xyz) in mathbbR^3  y exp (xy) le z y  0 \nDualExponentialCone():  (uvw) in mathbbR^3  -u exp (vu) le exp(1) w u  0 \nPowerCone(exponent):  (xyz) in mathbbR^3  x^mboxexponent y^1-mboxexponent ge z xy ge 0 \nDualPowerCone(exponent):  (uvw) in mathbbR^3  fracumboxexponent^mboxexponent fracv1-mboxexponent^1-mboxexponent ge w uv ge 0 \nPositiveSemidefiniteConeTriangle(dimension):  X in mathbbR^mboxdimension(mboxdimension+1)2  X mboxis the upper triangle of a PSD matrix\nPositiveSemidefiniteConeSquare(dimension):  X in mathbbR^mboxdimension^2  X mboxis a PSD matrix\nLogDetConeTriangle(dimension):  (tX) in mathbbR^1+mboxdimension(1+mboxdimension)2  t le log(det(X)) X mboxis the upper triangle of a PSD matrix\nLogDetConeSquare(dimension):  (tX) in mathbbR^1+mboxdimension^2  t le log(det(X)) X mboxis a PSD matrix\nRootDetConeTriangle(dimension):  (tX) in mathbbR^1+mboxdimension(1+mboxdimension)2  t le det(X)^1mboxdimension X mboxis the upper triangle of a PSD matrix\nRootDetConeSquare(dimension):  (tX) in mathbbR^1+mboxdimension^2  t le det(X)^1mboxdimension X mboxis a PSD matrix\nInteger(): mathbbZ\nZeroOne():  0 1 \nSemicontinuous(lower,upper):  0 cup lowerupper\nSemiinteger(lower,upper):  0 cup lowerlower+1ldotsupper-1upper"
},

{
    "location": "apimanual.html#The-ModelLike-and-AbstractOptimizer-APIs-1",
    "page": "Manual",
    "title": "The ModelLike and AbstractOptimizer APIs",
    "category": "section",
    "text": "The most significant part of MOI is the definition of the model API that is used to specify an instance of an optimization problem (e.g., by adding variables and constraints). Objects that implement the model API should inherit from the ModelLike abstract type.Notably missing from the model API is the method to solve an optimization problem. ModelLike objects may store an instance (e.g., in memory or backed by a file format) without being linked to a particular solver. In addition to the model API, MOI defines AbstractOptimizer. Optimizers (or solvers) implement the model API (inheriting from ModelLike) and additionally provide methods to solve the model.Through the rest of the manual, model is used as a generic ModelLike, and optimizer is used as a generic AbstractOptimizer.[Discuss how models are constructed, optimizer attributes.]"
},

{
    "location": "apimanual.html#Variables-1",
    "page": "Manual",
    "title": "Variables",
    "category": "section",
    "text": "All variables in MOI are scalar variables. New scalar variables are created with addvariable! or addvariables!, which return a VariableIndex or Vector{VariableIndex} respectively. VariableIndex objects are type-safe wrappers around integers that refer to a variable in a particular model.One uses VariableIndex objects to set and get variable attributes. For example, the VariablePrimalStart attribute is used to provide an initial starting point for a variable or collection of variables:v = addvariable!(model)\nset!(model, VariablePrimalStart(), v, 10.5)\nv2 = addvariables!(model, 3)\nset!(model, VariablePrimalStart(), v2, [1.3,6.8,-4.6])A variable can be deleted from a model with delete!(::ModelLike, ::VariableIndex). Not all models support deleting variables; the candelete method is used to check if this is supported."
},

{
    "location": "apimanual.html#Functions-1",
    "page": "Manual",
    "title": "Functions",
    "category": "section",
    "text": "MOI defines six functions as listed in the definition of the Standard form problem. The simplest function is SingleVariable defined as:struct SingleVariable <: AbstractFunction\n    variable::VariableIndex\nendIf v is a VariableIndex object, then SingleVariable(v) is simply the scalar-valued function from the complete set of variables in a model that returns the value of variable v. One may also call this function a coordinate projection, which is more useful for defining constraints than as an objective function.A more interesting function is ScalarAffineFunction, defined asstruct ScalarAffineFunction{T} <: AbstractScalarFunction\n    terms::Vector{ScalarAffineTerm{T}}\n    constant::T\nendThe ScalarAffineTerm struct defines a variable-coefficient pair:struct ScalarAffineTerm{T}\n    coefficient::T\n    variable_index::VariableIndex\nendIf x is a vector of VariableIndex objects, then ScalarAffineFunction(ScalarAffineTerm.([5.0,-2.3],[x[1],x[2]]),1.0) represents the function 5x_1 - 23x_2 + 1.note: Note\nScalarAffineTerm.([5.0,-2.3],[x[1],x[2]]) is a shortcut for [ScalarAffineTerm(5.0, x[1]), ScalarAffineTerm(-2.3, x[2])]. This is Julia\'s broadcast syntax and is used quite often.Objective functions are assigned to a model by setting the ObjectiveFunction attribute. The ObjectiveSense attribute is used for setting the optimization sense. For example,x = addvariables!(model, 2)\nset!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(),\n            ScalarAffineFunction(ScalarAffineTerm.([5.0,-2.3],[x[1],x[2]]),1.0))\nset!(model, ObjectiveSense(), MinSense)sets the objective to the function just discussed in the minimization sense.See Functions and function modifications for the complete list of functions."
},

{
    "location": "apimanual.html#Sets-and-Constraints-1",
    "page": "Manual",
    "title": "Sets and Constraints",
    "category": "section",
    "text": "All constraints are specified with addconstraint! by restricting the output of some function to a set. The interface allows an arbitrary combination of functions and sets, but of course solvers may decide to support only a small number of combinations.For example, linear programming solvers should support, at least, combinations of affine functions with the LessThan and GreaterThan sets. These are simply linear constraints. SingleVariable functions combined with these same sets are used to specify upper and lower bounds on variables.The code example below encodes the linear optimization problem:beginalign\n max_x in mathbbR^2  3x_1 + 2x_2 \n\n textst  x_1 + x_2 le 5\n\n x_1  ge 0\n\nx_2  ge -1\nendalignx = addvariables!(model, 2)\nset!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(),\n            ScalarAffineFunction(ScalarAffineTerm.([3.0, 2.0], x), 0.0))\nset!(model, ObjectiveSense(), MaxSense)\naddconstraint!(model, ScalarAffineFunction(ScalarAffineTerm.(1.0, x), 0.0),\n                      LessThan(5.0))\naddconstraint!(model, SingleVariable(x[1]), GreaterThan(0.0))\naddconstraint!(model, SingleVariable(x[2]), GreaterThan(-1.0))Besides scalar-valued functions in scalar-valued sets it possible to use vector-valued functions and sets.The code example below encodes the convex optimization problem:beginalign\n max_xyz in mathbbR  y + z \n\n textst  3x = 2\n\n x  ge (yz)_2\nendalignx,y,z = addvariables!(model, 3)\nset!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(),\n            ScalarAffineFunction(ScalarAffineTerm.(1.0, [y,z]), 0.0))\nset!(model, ObjectiveSense(), MaxSense)\nvector_terms = [VectorAffineTerm(1, ScalarAffineTerm(3.0, x))]\naddconstraint!(model, VectorAffineFunction(vector_terms,[-2.0]), Zeros(1))\naddconstraint!(model, VectorOfVariables([x,y,z]), SecondOrderCone(3))[Describe ConstraintIndex objects.]"
},

{
    "location": "apimanual.html#Constraints-by-function-set-pairs-1",
    "page": "Manual",
    "title": "Constraints by function-set pairs",
    "category": "section",
    "text": "Below is a list of common constraint types and how they are represented as function-set pairs in MOI. In the notation below, x is a vector of decision variables, x_i is a scalar decision variable, and all other terms are fixed constants.[Define notation more precisely. a vector; A matrix; don\'t reuse ulb as scalar and vector]"
},

{
    "location": "apimanual.html#Linear-constraints-1",
    "page": "Manual",
    "title": "Linear constraints",
    "category": "section",
    "text": "Mathematical Constraint MOI Function MOI Set\na^Tx le u ScalarAffineFunction LessThan\na^Tx ge l ScalarAffineFunction GreaterThan\na^Tx = b ScalarAffineFunction EqualTo\nl le a^Tx le u ScalarAffineFunction Interval\nx_i le u SingleVariable LessThan\nx_i ge l SingleVariable GreaterThan\nx_i = b SingleVariable EqualTo\nl le x_i le u SingleVariable Interval\nAx + b in mathbbR_+^n VectorAffineFunction Nonnegatives\nAx + b in mathbbR_-^n VectorAffineFunction Nonpositives\nAx + b = 0 VectorAffineFunction ZerosBy convention, solvers are not expected to support nonzero constant terms in the ScalarAffineFunctions the first four rows above, because they are redundant with the parameters of the sets. For example, 2x + 1 le 2 should be encoded as 2x le 1.Constraints with SingleVariable in LessThan, GreaterThan, EqualTo, or Interval sets have a natural interpretation as variable bounds. As such, it is typically not natural to impose multiple lower or upper bounds on the same variable, and by convention we do not ask solver interfaces to support this. It is natural, however, to impose upper and lower bounds separately as two different constraints on a single variable. The difference between imposing bounds by using a single Interval constraint and by using separate LessThan and GreaterThan constraints is that the latter will allow the solver to return separate dual multipliers for the two bounds, while the former will allow the solver to return only a single dual for the interval constraint.[Define mathbbR_+ mathbbR_-]"
},

{
    "location": "apimanual.html#Conic-constraints-1",
    "page": "Manual",
    "title": "Conic constraints",
    "category": "section",
    "text": "Mathematical Constraint MOI Function MOI Set\nlVert Ax + brVert_2 le c^Tx + d VectorAffineFunction SecondOrderCone\ny ge lVert x rVert_2 VectorOfVariables SecondOrderCone\n2yz ge lVert x rVert_2^2 yz ge 0 VectorOfVariables RotatedSecondOrderCone\n(a_1^Tx + b_1a_2^Tx + b_2a_3^Tx + b_3) in mathcalE VectorAffineFunction ExponentialCone\nA(x) in mathcalS_+ VectorAffineFunction PositiveSemidefiniteConeTriangle\nA(x) in mathcalS_+ VectorAffineFunction PositiveSemidefiniteConeSquare\nx in mathcalS_+ VectorOfVariables PositiveSemidefiniteConeTriangle\nx in mathcalS_+ VectorOfVariables PositiveSemidefiniteConeSquare[Define mathcalE (exponential cone), mathcalS_+ (smat), mathcalS_+ (svec). A(x) is an affine function of x that outputs a matrix.]"
},

{
    "location": "apimanual.html#Quadratic-constraints-1",
    "page": "Manual",
    "title": "Quadratic constraints",
    "category": "section",
    "text": "Mathematical Constraint MOI Function MOI Set\nx^TQx + a^Tx + b ge 0 ScalarQuadraticFunction GreaterThan\nx^TQx + a^Tx + b le 0 ScalarQuadraticFunction LessThan\nx^TQx + a^Tx + b = 0 ScalarQuadraticFunction EqualTo\nBilinear matrix inequality VectorQuadraticFunction PositiveSemidefiniteCone..."
},

{
    "location": "apimanual.html#Discrete-and-logical-constraints-1",
    "page": "Manual",
    "title": "Discrete and logical constraints",
    "category": "section",
    "text": "Mathematical Constraint MOI Function MOI Set\nx_i in mathbbZ SingleVariable Integer\nx_i in 01 SingleVariable ZeroOne\nx_i in 0 cup lu SingleVariable Semicontinuous\nx_i in 0 cup ll+1ldotsu-1u SingleVariable Semiinteger\nAt most one component of x can be nonzero VectorOfVariables SOS1\nAt most two components of x can be nonzero, and if so they must be adjacent components VectorOfVariables SOS2"
},

{
    "location": "apimanual.html#Solving-and-retrieving-the-results-1",
    "page": "Manual",
    "title": "Solving and retrieving the results",
    "category": "section",
    "text": "Once an optimizer is loaded with the objective function and all of the constraints, we can ask the solver to solve the model by calling optimize!.optimize!(optimizer)The optimization procedure may terminate for a number of reasons. The TerminationStatus attribute of the optimizer returns a TerminationStatusCode object which explains why the solver stopped. Some statuses indicate generally successful termination, some termination because of limit, and some termination because of something unexpected like invalid problem data or failure to converge. A typical usage of the TerminationStatus attribute is as follows:status = MOI.get(optimizer, TerminationStatus())\nif status == Success\n    # Ok, the solver has a result to return\nelse\n    # Handle other cases\n    # The solver may or may not have a result\nendThe Success status code specifically implies that the solver has a \"result\" to return. In the case that the solver converged to an optimal solution, this result will just be the optimal solution vector. The PrimalStatus attribute returns a ResultStatusCode that explains how to interpret the result. In the case that the solver is known to return globally optimal solutions (up to numerical tolerances), the combination of Success termination status and FeasiblePoint primal result status implies that the primal result vector should be interpreted as a globally optimal solution. A result may be available even if the status is not Success, for example, if the solver stopped because of a time limit and has a feasible but nonoptimal solution. Use the ResultCount attribute to check if one or more results are available.In addition to the primal status, the DualStatus provides important information for primal-dual solvers.If a result is available, it may be retrieved with the VariablePrimal attribute:MOI.get(optimizer, VariablePrimal(), x)If x is a VariableIndex then the function call returns a scalar, and if x is a Vector{VariableIndex} then the call returns a vector of scalars. VariablePrimal() is equivalent to VariablePrimal(1), i.e., the variable primal vector of the first result. Use VariablePrimal(N) to access the Nth result.See also the attributes ConstraintPrimal, and ConstraintDual. See Duals for a discussion of the MOI conventions for primal-dual pairs and certificates."
},

{
    "location": "apimanual.html#Common-status-situations-1",
    "page": "Manual",
    "title": "Common status situations",
    "category": "section",
    "text": "The sections below describe how to interpret different status cases for three common classes of solvers. Most importantly, it is essential to know if a solver is expected to provide a global or only locally optimal solution when interpreting the result statuses. Solver wrappers may provide additional information on how the solver\'s statuses map to MOI statuses."
},

{
    "location": "apimanual.html#Primal-dual-convex-solver-1",
    "page": "Manual",
    "title": "Primal-dual convex solver",
    "category": "section",
    "text": "A typical primal-dual solver is capable of certifying optimality of a solution to a convex optimization problem by providing a primal-dual feasible solution with matching objective values. It may also certify that either the primal or dual problem is infeasible by providing a certain ray of the dual or primal, respectively. Typically two solves are required to certify unboundedness, one to find a ray and a second to find a feasible point. A solver may also provide a facial reduction certificate. When a primal-dual solver terminates with Success status, it is reasonable to assume that a primal and dual statuses of FeasiblePoint imply that the corresponding primal-dual results are a (numerically) optimal primal-dual pair. The AlmostSuccess status implies that the solve has completed to relaxed tolerances, so in this case FeasiblePoint or NearlyFeasiblePoint statuses would imply a near-optimal primal-dual pair. For all other termination statuses, there are no specific guarantees on the results returned."
},

{
    "location": "apimanual.html#Global-mixed-integer-or-nonconvex-solver-1",
    "page": "Manual",
    "title": "Global mixed-integer or nonconvex solver",
    "category": "section",
    "text": "When a global solver returns Success and the primal result is a FeasiblePoint, then it is implied that the primal result is indeed a globally optimal solution up to the specified tolerances. Typically, no dual certificate is available to certify optimality. The ObjectiveBound should provide extra information on the optimality gap."
},

{
    "location": "apimanual.html#Local-solver-1",
    "page": "Manual",
    "title": "Local solver",
    "category": "section",
    "text": "For solvers which perform a search based only on local criteria (for example, gradient descent), without additional knowledge of the structure of the problem, we can say only that Success and FeasiblePoint imply that the primal result belongs to the class of points which the chosen algorithm is known to converge to. Gradient descent algorithms may converge to saddle points, for example. It is also possible for such algorithms to converge to infeasible points, in which case the termination status would be Success and the primal result status would be InfeasiblePoint. This does not imply that the problem is infeasible and so cannot be called a certificate of infeasibility."
},

{
    "location": "apimanual.html#A-complete-example:-solving-a-knapsack-problem-1",
    "page": "Manual",
    "title": "A complete example: solving a knapsack problem",
    "category": "section",
    "text": "[ needs formatting help, doc tests ]using MathOptInterface\nconst MOI = MathOptInterface\nusing MathOptInterfaceGLPK\n\n# Solve the binary-constrained knapsack problem: max c\'x: w\'x <= C, x binary using GLPK.\n\nc = [1.0, 2.0, 3.0]\nw = [0.3, 0.5, 1.0]\nC = 3.2\n\nnumvariables = length(c)\n\noptimizer = MathOptInterfaceGLPK.GLPKOptimizerMIP()\n\n# create the variables in the problem\nx = MOI.addvariables!(optimizer, numvariables)\n\n# set the objective function\nobjective_function = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(c, x), 0.0)\nMOI.set!(optimizer, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objective_function)\nMOI.set!(optimizer, MOI.ObjectiveSense(), MOI.MaxSense)\n\n# add the knapsack constraint\nknapsack_function = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(w, x), 0.0)\nMOI.addconstraint!(optimizer, knapsack_function, MOI.LessThan(C))\n\n# add integrality constraints\nfor i in 1:numvariables\n    MOI.addconstraint!(optimizer, MOI.SingleVariable(x[i]), MOI.ZeroOne())\nend\n\n# all set\nMOI.optimize!(optimizer)\n\ntermination_status = MOI.get(optimizer, TerminationStatus())\nobjvalue = MOI.canget(optimizer, MOI.ObjectiveValue()) ? MOI.get(optimizer, MOI.ObjectiveValue()) : NaN\nif termination_status != MOI.Success\n    error(\"Solver terminated with status $termination_status\")\nend\n\n@assert MOI.get(optimizer, MOI.ResultCount()) > 0\n\nresult_status = MOI.get(optimizer, MOI.PrimalStatus())\nif result_status != MOI.FeasiblePoint\n    error(\"Solver ran successfully did not return a feasible point. The problem may be infeasible.\")\nend\nprimal_variable_result = MOI.get(optimizer, MOI.VariablePrimal(), x)\n\n@show objvalue\n@show primal_variable_result"
},

{
    "location": "apimanual.html#Advanced-1",
    "page": "Manual",
    "title": "Advanced",
    "category": "section",
    "text": ""
},

{
    "location": "apimanual.html#Duals-1",
    "page": "Manual",
    "title": "Duals",
    "category": "section",
    "text": "Conic duality is the starting point for MOI\'s duality conventions. When all functions are affine (or coordinate projections), and all constraint sets are closed convex cones, the model may be called a conic optimization problem. For conic-form minimization problems, the primal is:beginalign\n min_x in mathbbR^n  a_0^T x + b_0\n\n textst  A_i x + b_i  in mathcalC_i  i = 1 ldots m\nendalignand the dual is:beginalign\n max_y_1 ldots y_m  -sum_i=1^m b_i^T y_i + b_0\n\n textst  a_0 - sum_i=1^m A_i^T y_i  = 0\n\n  y_i  in mathcalC_i^*  i = 1 ldots m\nendalignwhere each mathcalC_i is a closed convex cone and mathcalC_i^* is its dual cone.For conic-form maximization problems, the primal is:beginalign\n max_x in mathbbR^n  a_0^T x + b_0\n\n textst  A_i x + b_i  in mathcalC_i  i = 1 ldots m\nendalignand the dual is:beginalign\n min_y_1 ldots y_m  sum_i=1^m b_i^T y_i + b_0\n\n textst  a_0 + sum_i=1^m A_i^T y_i  = 0\n\n  y_i  in mathcalC_i^*  i = 1 ldots m\nendalignA linear inequality constraint a^T x + b ge c should be interpreted as a^T x + b - c in mathbbR_+, and similarly a^T x + b le c should be interpreted as a^T x + b - c in mathbbR_-. Variable-wise constraints should be interpreted as affine constraints with the appropriate identity mapping in place of A_i.For the special case of minimization LPs, the MOI primal form can be stated asbeginalign\n min_x in mathbbR^n  a_0^T x + b_0\n\n textst\nA_1 x  ge b_1\n A_2 x  le b_2\n A_3 x  = b_3\nendalignBy applying the stated transformations to conic form, taking the dual, and transforming back into linear inequality form, one obtains the following dual:beginalign\n max_y_1y_2y_3  b_1^Ty_1 + b_2^Ty_2 + b_3^Ty_3 + b_0\n\n textst\nA_1^Ty_1 + A_2^Ty_2 + A_3^Ty_3  = a_0\n y_1 ge 0\n y_2 le 0\nendalignFor maximization LPs, the MOI primal form can be stated as:beginalign\n max_x in mathbbR^n  a_0^T x + b_0\n\n textst\nA_1 x  ge b_1\n A_2 x  le b_2\n A_3 x  = b_3\nendalignand similarly, the dual is:beginalign\n min_y_1y_2y_3  -b_1^Ty_1 - b_2^Ty_2 - b_3^Ty_3 + b_0\n\n textst\nA_1^Ty_1 + A_2^Ty_2 + A_3^Ty_3  = -a_0\n y_1 ge 0\n y_2 le 0\nendalignAn important note for the LP case is that the signs of the feasible duals depend only on the sense of the inequality and not on the objective sense.Currently, a convention for duals is not defined for problems with non-conic sets mathcalS_i or quadratic functions f_0 f_i."
},

{
    "location": "apimanual.html#Duality-and-scalar-product-1",
    "page": "Manual",
    "title": "Duality and scalar product",
    "category": "section",
    "text": "The scalar product is different from the canonical one for the sets PositiveSemidefiniteConeTriangle, LogDetConeTriangle, RootDetConeTriangle. If the set C_i of the section Duals is one of these three cones, then the rows of the matrix A_i corresponding to off-diagonal entries are twice the value of the coefficients field in the VectorAffineFunction for the corresponding rows. See PositiveSemidefiniteConeTriangle for details."
},

{
    "location": "apimanual.html#Modifying-a-model-1",
    "page": "Manual",
    "title": "Modifying a model",
    "category": "section",
    "text": "[Explain modifyconstraint! and modifyobjective!.]"
},

{
    "location": "apimanual.html#Constraint-bridges-1",
    "page": "Manual",
    "title": "Constraint bridges",
    "category": "section",
    "text": "A constraint often possess different equivalent formulations, but a solver may only support one of them. It would be duplicate work to implement rewritting rules in every solver wrapper for every different formulation of the constraint to express it in the form supported by the solver. Constraint bridges provide a way to define a rewritting rule on top of the MOI interface which can be used by any optimizer. Some rules also implement constraint modifications and constraint primal and duals translations.For example, the SplitIntervalBridge defines the reformulation of a ScalarAffineFunction-in-Interval constraint into a ScalarAffineFunction-in-GreaterThan and a ScalarAffineFunction-in-LessThan constraint. The SplitInterval is the bridge optimizer that applies the SplitIntervalBridge rewritting rule. Given an optimizer optimizer implementing ScalarAffineFunction-in-GreaterThan and ScalarAffineFunction-in-LessThan, the optimizerbridgedoptimizer = SplitInterval(optimizer)will additionally support ScalarAffineFunction-in-Interval."
},

{
    "location": "apimanual.html#Implementing-a-solver-interface-1",
    "page": "Manual",
    "title": "Implementing a solver interface",
    "category": "section",
    "text": "[The interface is designed for multiple dispatch, e.g., attributes, combinations of sets and functions.][Avoid storing extra copies of the problem when possible.][copy!]"
},

{
    "location": "apimanual.html#JuMP-mapping-1",
    "page": "Manual",
    "title": "JuMP mapping",
    "category": "section",
    "text": "MOI defines a very general interface, with multiple possible ways to describe the same constraint. This is considered a feature, not a bug. MOI is designed to make it possible to experiment with alternative representations of an optimization problem at both the solving and modeling level. When implementing an interface, it is important to keep in mind that the constraints which a solver supports via MOI will have a near 1-to-1 correspondence with how users can express problems in JuMP, because JuMP does not perform automatic transformations. (Alternative systems like Convex.jl do.) The following bullet points show examples of how JuMP constraints are translated into MOI function-set pairs:@constraint(m, 2x + y <= 10) becomes ScalarAffineFunction-in-LessThan;\n@constraint(m, 2x + y >= 10) becomes ScalarAffineFunction-in-GreaterThan;\n@constraint(m, 2x + y == 10) becomes ScalarAffineFunction-in-EqualTo;\n@constraint(m, 0 <= 2x + y <= 10) becomes ScalarAffineFunction-in-Interval;\n@constraint(m, 2x + y in ArbitrarySet()) becomes ScalarAffineFunction-in-ArbitrarySet.Variable bounds are handled in a similar fashion:@variable(m, x <= 1) becomes SingleVariable-in-LessThan;\n@variable(m, x >= 1) becomes SingleVariable-in-GreaterThan.One notable difference is that a variable with an upper and lower bound is translated into two constraints, rather than an interval. i.e.:@variable(m, 0 <= x <= 1) becomes SingleVariable-in-LessThan and SingleVariable-in-GreaterThan.Therefore, if a solver wrapper does not support ScalarAffineFunction-in-LessThan constraints, users will not be able to write: @constraint(m, 2x + y <= 10) in JuMP. With this in mind, developers should support all the constraint types that they want to be usable from JuMP. That said, from the perspective of JuMP, solvers can safely choose to not support the following constraints:ScalarAffineFunction in GreaterThan, LessThan, or EqualTo with a nonzero constant in the function. Constants in the affine function should instead be moved into the parameters of the corresponding sets.\nScalarAffineFunction in Nonnegative, Nonpositive or Zeros. Alternative constraints are available by using a VectorAffineFunction with one output row or ScalarAffineFunction with GreaterThan, LessThan, or EqualTo.\nTwo SingleVariable-in-LessThan constraints applied to the same variable (similarly with GreaterThan). These should be interpreted as variable bounds, and each variable naturally has at most one upper or lower bound."
},

{
    "location": "apimanual.html#Column-Generation-1",
    "page": "Manual",
    "title": "Column Generation",
    "category": "section",
    "text": "There is no special interface for column generation. If the solver has a special API for setting coefficients in existing constraints when adding a new variable, it is possible to queue modifications and new variables and then call the solver\'s API once all of the new coefficients are known."
},

{
    "location": "apimanual.html#Problem-data-1",
    "page": "Manual",
    "title": "Problem data",
    "category": "section",
    "text": "All data passed to the solver should be copied immediately to internal data structures. Solvers may not modify any input vectors and should assume that input vectors may be modified by users in the future. This applies, for example, to the terms vector in ScalarAffineFunction. Vectors returned to the user, e.g., via ObjectiveFunction or ConstraintFunction attributes, should not be modified by the solver afterwards. The in-place version of get! can be used by users to avoid extra copies in this case."
},

{
    "location": "apimanual.html#Statuses-1",
    "page": "Manual",
    "title": "Statuses",
    "category": "section",
    "text": "Solver wrappers should document how the low-level solver statuses map to the MOI statuses. In particular, the characterization of a result with status FeasiblePoint and termination status Success is entirely solver defined. It may or may not be a globally optimal solution. Solver wrappers are not responsible for verifying the feasibility of results. Statuses like NearlyFeasiblePoint, InfeasiblePoint, NearlyInfeasiblePoint, and NearlyReductionCertificate are designed to be used when the solver explicitly indicates as much."
},

{
    "location": "apimanual.html#canXXX-1",
    "page": "Manual",
    "title": "canXXX",
    "category": "section",
    "text": "For most operations, MOI provides a function canXXX that can be used to check if the operation XXX is allowed. For example, addconstraint!(model::ModelLike, func::F, set::S) has the corresponding function canaddconstraint(model, ::Type{F}, ::Type{S})::Bool. If canaddconstraint returns false, then calling addconstraint! must throw an error (likewise with all XXX and canXXX pairs). Note that even if canaddconstraint returns true, addconstraint! may still throw an error if, for example, the constraint function does not meet some sparsity conditions, one of the coefficients is NaN, or an invalid variable index is provided. This is because (in most cases) the canXXX method receives type information instead of the actual arguments."
},

{
    "location": "apimanual.html#Package-Naming-1",
    "page": "Manual",
    "title": "Package Naming",
    "category": "section",
    "text": "MOI solver interfaces may be in the same package as the solver itself (either the C wrapper if the solver is accessible through C, or the Julia code if the solver is written in Julia, for example). The guideline for naming the file containing the MOI wrapper is src/MOIWrapper.jl and test/MOIWrapper.jl for the tests. If the MOI wrapper implementation is spread in several files, they should be stored in a src/MOIWrapper folder and included by a src/MOIWrapper/MOIWrapper.jl file. In some cases it may be more appropriate to host the MOI wrapper in its own package; in this case it is recommended that the MOI wrapper package be named MathOptInterfaceXXX where XXX is the solver name."
},

{
    "location": "apimanual.html#Testing-guideline-1",
    "page": "Manual",
    "title": "Testing guideline",
    "category": "section",
    "text": "The skeleton below can be used for the wrapper test file of a solver name FooBar. A few bridges are used to give examples, you can find more bridges in the Bridges section.using MathOptInterface\nconst MOI = MathOptInterface\nconst MOIT = MOI.Test\nconst MOIB = MOI.Bridges\n\nconst optimizer = FooBarOptimizer()\nconst config = MOIT.TestConfig(atol=1e-6, rtol=1e-6)\n\n@testset \"MOI Continuous Linear\" begin\n    # If `optimizer` does not support the `Interval` set,\n    # the `SplitInterval` bridge can be used to split each `f`-in-`Interval(lb, ub)` constraint into\n    # a constraint `f`-in-`GreaterThan(lb)` and a constraint `f`-in-`LessThan(ub)`\n    MOIT.contlineartest(MOIB.SplitInterval{Float64}(optimizer), config)\nend\n\n@testset \"MOI Continuous Conic\" begin\n    # If the solver supports rotated second order cone, the `GeoMean` bridge can be used to make it support geometric mean cone constraints.\n    # If it additionally support positive semidefinite cone constraints, the `RootDet` bridge can be used to make it support root-det cone constraints.\n    MOIT.contlineartest(MOIB.RootDet{Float64}(MOIB.GeoMean{Float64}(optimizer)), config)\nend\n\n@testset \"MOI Integer Conic\" begin\n    MOIT.intconictest(optimizer, config)\nendIf the wrapper does not support building the model incrementally (i.e. with addvariable! and addconstraint!), the line const optimizer = FooBarOptimizer() can be replaced withconst MOIU = MOI.Utilities\n# Include here the functions/sets supported by the solver wrapper (not those that are supported through bridges)\nMOIU.@model FooBarModelData () (EqualTo, GreaterThan, LessThan) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)\nconst optimizer = MOIU.CachingOptimizer(FooBarModelData{Float64}(), FooBarOptimizer())"
},

{
    "location": "apireference.html#",
    "page": "Reference",
    "title": "Reference",
    "category": "page",
    "text": "CurrentModule = MathOptInterface"
},

{
    "location": "apireference.html#API-Reference-1",
    "page": "Reference",
    "title": "API Reference",
    "category": "section",
    "text": "[Some introduction to API. List basic standalone methods.]"
},

{
    "location": "apireference.html#MathOptInterface.AbstractOptimizerAttribute",
    "page": "Reference",
    "title": "MathOptInterface.AbstractOptimizerAttribute",
    "category": "type",
    "text": "AbstractOptimizerAttribute\n\nAbstract supertype for attribute objects that can be used to set or get attributes (properties) of the optimizer.\n\nNote\n\nThe difference between AbstractOptimizerAttribute and AbstractModelAttribute lies in the behavior of isempty, empty! and copy!. Typically optimizer attributes only affect how the model is solved.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.AbstractModelAttribute",
    "page": "Reference",
    "title": "MathOptInterface.AbstractModelAttribute",
    "category": "type",
    "text": "AbstractModelAttribute\n\nAbstract supertype for attribute objects that can be used to set or get attributes (properties) of the model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.AbstractVariableAttribute",
    "page": "Reference",
    "title": "MathOptInterface.AbstractVariableAttribute",
    "category": "type",
    "text": "AbstractVariableAttribute\n\nAbstract supertype for attribute objects that can be used to set or get attributes (properties) of variables in the model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.AbstractConstraintAttribute",
    "page": "Reference",
    "title": "MathOptInterface.AbstractConstraintAttribute",
    "category": "type",
    "text": "AbstractConstraintAttribute\n\nAbstract supertype for attribute objects that can be used to set or get attributes (properties) of constraints in the model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.canget",
    "page": "Reference",
    "title": "MathOptInterface.canget",
    "category": "function",
    "text": "canget(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute)::Bool\n\nReturn a Bool indicating whether optimizer currently has a value for the attribute specified by attr type attr.\n\ncanget(model::ModelLike, attr::AbstractModelAttribute)::Bool\n\nReturn a Bool indicating whether model currently has a value for the attribute specified by attribute type attr.\n\ncanget(model::ModelLike, attr::AbstractVariableAttribute, ::Type{VariableIndex})::Bool\n\nReturn a Bool indicating whether model currently has a value for the attribute specified by attribute type attr applied to every variable of the model.\n\ncanget(model::ModelLike, attr::AbstractConstraintAttribute, ::Type{ConstraintIndex{F,S}})::Bool where {F<:AbstractFunction,S<:AbstractSet}\n\nReturn a Bool indicating whether model currently has a value for the attribute specified by attribute type attr applied to every F-in-S constraint.\n\ncanget(model::ModelLike, ::Type{VariableIndex}, name::String)::Bool\n\nReturn a Bool indicating if a variable with the name name exists in model.\n\ncanget(model::ModelLike, ::Type{ConstraintIndex{F,S}}, name::String)::Bool where {F<:AbstractFunction,S<:AbstractSet}\n\nReturn a Bool indicating if an F-in-S constraint with the name name exists in model.\n\ncanget(model::ModelLike, ::Type{ConstraintIndex}, name::String)::Bool\n\nReturn a Bool indicating if a constraint of any kind with the name name exists in model.\n\nExamples\n\ncanget(model, ObjectiveValue())\ncanget(model, VariablePrimalStart(), VariableIndex)\ncanget(model, VariablePrimal(), VariableIndex)\ncanget(model, ConstraintPrimal(), ConstraintIndex{SingleVariable,EqualTo{Float64}})\ncanget(model, VariableIndex, \"var1\")\ncanget(model, ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}, \"con1\")\ncanget(model, ConstraintIndex, \"con1\")\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.get",
    "page": "Reference",
    "title": "MathOptInterface.get",
    "category": "function",
    "text": "get(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute)\n\nReturn an attribute attr of the optimizer optimizer.\n\nget(model::ModelLike, attr::AbstractModelAttribute)\n\nReturn an attribute attr of the model model.\n\nget(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex)\n\nReturn an attribute attr of the variable v in model model.\n\nget(model::ModelLike, attr::AbstractVariableAttribute, v::Vector{VariableIndex})\n\nReturn a vector of attributes corresponding to each variable in the collection v in the model model.\n\nget(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex)\n\nReturn an attribute attr of the constraint c in model model.\n\nget(model::ModelLike, attr::AbstractConstraintAttribute, c::Vector{ConstraintIndex{F,S}})\n\nReturn a vector of attributes corresponding to each constraint in the collection c in the model model.\n\nget(model::ModelLike, ::Type{VariableIndex}, name::String)\n\nIf a variable with name name exists in the model model, return the corresponding index, otherwise throw a KeyError.\n\nget(model::ModelLike, ::Type{ConstraintIndex{F,S}}, name::String) where {F<:AbstractFunction,S<:AbstractSet}\n\nIf an F-in-S constraint with name name exists in the model model, return the corresponding index, otherwise throw a KeyError.\n\nget(model::ModelLike, ::Type{ConstraintIndex}, name::String)\n\nIf any constraint with name name exists in the model model, return the corresponding index, otherwise throw a KeyError. This version is available for convenience but may incur a performance penalty because it is not type stable.\n\nExamples\n\nget(model, ObjectiveValue())\nget(model, VariablePrimal(), ref)\nget(model, VariablePrimal(5), [ref1, ref2])\nget(model, OtherAttribute(\"something specific to cplex\"))\nget(model, VariableIndex, \"var1\")\nget(model, ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}, \"con1\")\nget(model, ConstraintIndex, \"con1\")\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.get!",
    "page": "Reference",
    "title": "MathOptInterface.get!",
    "category": "function",
    "text": "get!(output, model::ModelLike, args...)\n\nAn in-place version of get. The signature matches that of get except that the the result is placed in the vector output.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.canset",
    "page": "Reference",
    "title": "MathOptInterface.canset",
    "category": "function",
    "text": "canset(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute)::Bool\n\nReturn a Bool indicating whether it is possible to set the attribute attr to the optimizer optimizer.\n\ncanset(model::ModelLike, attr::AbstractModelAttribute)::Bool\n\nReturn a Bool indicating whether it is possible to set the attribute attr to the model model.\n\ncanset(model::ModelLike, attr::AbstractVariableAttribute, R::Type{VariableIndex})::Bool\ncanset(model::ModelLike, attr::AbstractConstraintAttribute, R::Type{ConstraintIndex{F,S})::Bool\n\nReturn a Bool indicating whether it is possible to set attribute attr applied to the index type R in the model model.\n\nExamples\n\ncanset(model, ObjectiveValue())\ncanset(model, VariablePrimalStart(), VariableIndex)\ncanset(model, ConstraintPrimal(), ConstraintIndex{VectorAffineFunction{Float64},Nonnegatives})\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.set!",
    "page": "Reference",
    "title": "MathOptInterface.set!",
    "category": "function",
    "text": "set!(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute, value)\n\nAssign value to the attribute attr of the optimizer optimizer.\n\nset!(model::ModelLike, attr::AbstractModelAttribute, value)\n\nAssign value to the attribute attr of the model model.\n\nset!(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)\n\nAssign value to the attribute attr of variable v in model model.\n\nset!(model::ModelLike, attr::AbstractVariableAttribute, v::Vector{VariableIndex}, vector_of_values)\n\nAssign a value respectively to the attribute attr of each variable in the collection v in model model.\n\nset!(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)\n\nAssign a value to the attribute attr of constraint c in model model.\n\nset!(model::ModelLike, attr::AbstractConstraintAttribute, c::Vector{ConstraintIndex{F,S}}, vector_of_values)\n\nAssign a value respectively to the attribute attr of each constraint in the collection c in model model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.supports",
    "page": "Reference",
    "title": "MathOptInterface.supports",
    "category": "function",
    "text": "supports(model::ModelLike, attr::AbstractOptimizerAttribute)::Bool\n\nReturn a Bool indicating whether model supports the optimizer attribute attr.\n\nsupports(model::ModelLike, attr::AbstractModelAttribute)::Bool\n\nReturn a Bool indicating whether model supports the model attribute attr.\n\nsupports(model::ModelLike, attr::AbstractVariableAttribute, ::Type{VariableIndex})::Bool\n\nReturn a Bool indicating whether model supports the variable attribute attr.\n\nsupports(model::ModelLike, attr::AbstractConstraintAttribute, ::Type{ConstraintIndex{F,S}})::Bool where {F,S}\n\nReturn a Bool indicating whether model supports the constraint attribute attr applied to an F-in-S constraint.\n\nIn other words, it should return true if copy!(model, src) does not return CopyUnsupportedAttribute when the attribute attr is set to src. If the attribute is only not supported in specific circumstances, it should still return true.\n\n\n\n"
},

{
    "location": "apireference.html#Attributes-1",
    "page": "Reference",
    "title": "Attributes",
    "category": "section",
    "text": "List of attribute categories.AbstractOptimizerAttribute\nAbstractModelAttribute\nAbstractVariableAttribute\nAbstractConstraintAttributeFunctions for getting and setting attributes.canget\nget\nget!\ncanset\nset!\nsupports"
},

{
    "location": "apireference.html#MathOptInterface.ModelLike",
    "page": "Reference",
    "title": "MathOptInterface.ModelLike",
    "category": "type",
    "text": "ModelLike\n\nAbstract supertype for objects that implement the \"Model\" interface for defining an optimization problem.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.isempty",
    "page": "Reference",
    "title": "MathOptInterface.isempty",
    "category": "function",
    "text": "isempty(model::ModelLike)\n\nReturns false if the model has any model attribute set or has any variables or constraints. Note that an empty model can have optimizer attributes set.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.empty!",
    "page": "Reference",
    "title": "MathOptInterface.empty!",
    "category": "function",
    "text": "empty!(model::ModelLike)\n\nEmpty the model, that is, remove all variables, constraints and model attributes but not optimizer attributes.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.write",
    "page": "Reference",
    "title": "MathOptInterface.write",
    "category": "function",
    "text": "write(model::ModelLike, filename::String)\n\nWrites the current model data to the given file. Supported file types depend on the model type.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.read!",
    "page": "Reference",
    "title": "MathOptInterface.read!",
    "category": "function",
    "text": "read!(model::ModelLike, filename::String)\n\nRead the file filename into the model model. If m is non-empty, this may throw an error.\n\nSupported file types depend on the model type.\n\nNote\n\nOnce the contents of the file are loaded into the model, users can query the variables via get(model, ListOfVariableIndices()). However, some filetypes, such as LP files, do not maintain an explicit ordering of the variables. Therefore, the returned list may be in an arbitrary order. To avoid depending on the order of the indices, users should look up each variable index by name: get(model, VariableIndex, \"name\").\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.copy!",
    "page": "Reference",
    "title": "MathOptInterface.copy!",
    "category": "function",
    "text": "copy!(dest::ModelLike, src::ModelLike; copynames=true, warnattributes=true)::CopyResult\n\nCopy the model from src into dest. The target dest is emptied, and all previous indices to variables or constraints in dest are invalidated. Returns a CopyResult object. If the copy is successful, the CopyResult contains a dictionary-like object that translates variable and constraint indices from the src model to the corresponding indices in the dest model.\n\nIf copynames is false, the Name, VariableName and ConstraintName attributes are not copied even if they are set in src. If an attribute attr cannot be copied from src to dest then an error is thrown. If an optimizer attribute cannot be copied then:\n\nIf warnattributes is true, a warning is displayed, otherwise,\nThe attribute is silently ignored.\n\nExample\n\n# Given empty `ModelLike` objects `src` and `dest`.\n\nx = addvariable!(src)\n\nisvalid(src, x)   # true\nisvalid(dest, x)  # false (`dest` has no variables)\n\ncopy_result = copy!(dest, src)\nif copy_result.status == CopySuccess\n    index_map = copy_result.indexmap\n    isvalid(dest, x) # false (unless index_map[x] == x)\n    isvalid(dest, index_map[x]) # true\nelse\n    println(\"Copy failed with status \", copy_result.status)\n    println(\"Failure message: \", copy_result.message)\nend\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.CopyResult",
    "page": "Reference",
    "title": "MathOptInterface.CopyResult",
    "category": "type",
    "text": "struct CopyResult{T}\n    status::CopyStatusCode\n    message::String # Human-friendly explanation why the copy failed\n    indexmap::T     # Only valid if status is CopySuccess\nend\n\nA struct returned by copy! to indicate success or failure. If success, also exposes a map between the variable and constraint indices of the two models.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.CopyStatusCode",
    "page": "Reference",
    "title": "MathOptInterface.CopyStatusCode",
    "category": "type",
    "text": "CopyStatusCode\n\nAn Enum of possible statuses returned by a copy! operation through the CopyResult struct.\n\nCopySuccess: The copy was successful.\nCopyUnsupportedAttribute: The copy failed because the destination does not support an attribute present in the source.\nCopyUnsupportedConstraint: The copy failed because the destination does not support a constraint present in the source.\nCopyOtherError: The copy failed for a different reason.\n\nIn the failure cases:\n\nSee the corresponding message field of the CopyResult for an explanation of the failure.\nThe state of the destination model is undefined.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Name",
    "page": "Reference",
    "title": "MathOptInterface.Name",
    "category": "type",
    "text": "Name()\n\nA string identifying the model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ObjectiveSense",
    "page": "Reference",
    "title": "MathOptInterface.ObjectiveSense",
    "category": "type",
    "text": "ObjectiveSense()\n\nThe sense of the objective function, an OptimizationSense with value MinSense, MaxSense, or FeasiblitySense.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.NumberOfVariables",
    "page": "Reference",
    "title": "MathOptInterface.NumberOfVariables",
    "category": "type",
    "text": "NumberOfVariables()\n\nThe number of variables in the model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ListOfVariableIndices",
    "page": "Reference",
    "title": "MathOptInterface.ListOfVariableIndices",
    "category": "type",
    "text": "ListOfVariableIndices()\n\nA Vector{VariableIndex} containing all variable indices present in the model (i.e., of length equal to the value of NumberOfVariables()) in the order in which they were added.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ListOfConstraints",
    "page": "Reference",
    "title": "MathOptInterface.ListOfConstraints",
    "category": "type",
    "text": "ListOfConstraints()\n\nA list of tuples of the form (F,S), where F is a function type and S is a set type indicating that the attribute NumberOfConstraints{F,S}() has value greater than zero.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.NumberOfConstraints",
    "page": "Reference",
    "title": "MathOptInterface.NumberOfConstraints",
    "category": "type",
    "text": "NumberOfConstraints{F,S}()\n\nThe number of constraints of the type F-in-S present in the model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ListOfConstraintIndices",
    "page": "Reference",
    "title": "MathOptInterface.ListOfConstraintIndices",
    "category": "type",
    "text": "ListOfConstraintIndices{F,S}()\n\nA Vector{ConstraintIndex{F,S}} containing all constraint indices of type F-in-S in the model (i.e., of length equal to the value of NumberOfConstraints{F,S}()) in the order in which they were added.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ListOfModelAttributesSet",
    "page": "Reference",
    "title": "MathOptInterface.ListOfModelAttributesSet",
    "category": "type",
    "text": "ListOfModelAttributesSet()\n\nA Vector{AbstractModelAttribute} of all model attributes that were set to the model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ListOfVariableAttributesSet",
    "page": "Reference",
    "title": "MathOptInterface.ListOfVariableAttributesSet",
    "category": "type",
    "text": "ListOfVariableAttributesSet()\n\nA Vector{AbstractVariableAttribute} of all variable attributes that were set to the model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ListOfConstraintAttributesSet",
    "page": "Reference",
    "title": "MathOptInterface.ListOfConstraintAttributesSet",
    "category": "type",
    "text": "ListOfConstraintAttributesSet{F, S}()\n\nA Vector{AbstractConstraintAttribute} of all constraint attributes that were set to F-in-S constraints.\n\n\n\n"
},

{
    "location": "apireference.html#Model-Interface-1",
    "page": "Reference",
    "title": "Model Interface",
    "category": "section",
    "text": "ModelLike\nisempty\nempty!\nwrite\nread!Copyingcopy!\nCopyResult\nCopyStatusCodeList of model attributesName\nObjectiveSense\nNumberOfVariables\nListOfVariableIndices\nListOfConstraints\nNumberOfConstraints\nListOfConstraintIndices\nListOfModelAttributesSet\nListOfVariableAttributesSet\nListOfConstraintAttributesSet"
},

{
    "location": "apireference.html#MathOptInterface.AbstractOptimizer",
    "page": "Reference",
    "title": "MathOptInterface.AbstractOptimizer",
    "category": "type",
    "text": "AbstractOptimizer\n\nAbstract supertype for objects representing an instance of an optimization problem tied to a particular solver. This is typically a solver\'s in-memory representation. In addition to ModelLike, AbstractOptimizer objects let you solve the model and query the solution.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.optimize!",
    "page": "Reference",
    "title": "MathOptInterface.optimize!",
    "category": "function",
    "text": "optimize!(optimizer::AbstractOptimizer)\n\nStart the solution procedure.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.free!",
    "page": "Reference",
    "title": "MathOptInterface.free!",
    "category": "function",
    "text": "free!(optimizer::AbstractOptimizer)\n\nRelease any resources and memory used by the optimizer. Note that the Julia garbage collector takes care of this automatically, but automatic collection cannot always be forced. This method is useful for more precise control of resources, especially in the case of commercial solvers with licensing restrictions on the number of concurrent runs. Users must discard the optimizer object after this method is invoked.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.SolverName",
    "page": "Reference",
    "title": "MathOptInterface.SolverName",
    "category": "type",
    "text": "SolverName()\n\nA string identifying the solver.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.RawSolver",
    "page": "Reference",
    "title": "MathOptInterface.RawSolver",
    "category": "type",
    "text": "RawSolver()\n\nAn object that may be used to access a solver-specific API for this optimizer.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ResultCount",
    "page": "Reference",
    "title": "MathOptInterface.ResultCount",
    "category": "type",
    "text": "ResultCount()\n\nThe number of results available.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ObjectiveFunction",
    "page": "Reference",
    "title": "MathOptInterface.ObjectiveFunction",
    "category": "type",
    "text": "ObjectiveFunction{F<:AbstractScalarFunction}()\n\nAn F model which represents the objective function. It is guaranteed to be equivalent but not necessarily identical to the function provided by the user. Throws an InexactError if the objective function cannot be converted to F, e.g. the objective function is quadratic and F is ScalarAffineFunction{Float64} or it has non-integer coefficient and F is ScalarAffineFunction{Int}.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ObjectiveValue",
    "page": "Reference",
    "title": "MathOptInterface.ObjectiveValue",
    "category": "type",
    "text": "ObjectiveValue(resultidx::Int=1)\n\nThe objective value of the resultindexth primal result.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ObjectiveBound",
    "page": "Reference",
    "title": "MathOptInterface.ObjectiveBound",
    "category": "type",
    "text": "ObjectiveBound()\n\nThe best known bound on the optimal objective value.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.RelativeGap",
    "page": "Reference",
    "title": "MathOptInterface.RelativeGap",
    "category": "type",
    "text": "RelativeGap()\n\nThe final relative optimality gap, defined as fracb-ff, where b is the best bound and f is the best feasible objective value.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.SolveTime",
    "page": "Reference",
    "title": "MathOptInterface.SolveTime",
    "category": "type",
    "text": "SolveTime()\n\nThe total elapsed solution time (in seconds) as reported by the optimizer.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.SimplexIterations",
    "page": "Reference",
    "title": "MathOptInterface.SimplexIterations",
    "category": "type",
    "text": "SimplexIterations()\n\nThe cumulative number of simplex iterations during the optimization process. In particular, for a mixed-integer program (MIP), the total simplex iterations for all nodes.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.BarrierIterations",
    "page": "Reference",
    "title": "MathOptInterface.BarrierIterations",
    "category": "type",
    "text": "BarrierIterations()\n\nThe cumulative number of barrier iterations while solving a problem.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.NodeCount",
    "page": "Reference",
    "title": "MathOptInterface.NodeCount",
    "category": "type",
    "text": "NodeCount()\n\nThe total number of branch-and-bound nodes explored while solving a mixed-integer program (MIP).\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.TerminationStatus",
    "page": "Reference",
    "title": "MathOptInterface.TerminationStatus",
    "category": "type",
    "text": "TerminationStatus()\n\nA TerminationStatusCode explaining why the optimizer stopped.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.PrimalStatus",
    "page": "Reference",
    "title": "MathOptInterface.PrimalStatus",
    "category": "type",
    "text": "PrimalStatus(N)\nPrimalStatus()\n\nThe ResultStatusCode of the primal result N. If N is omitted, it defaults to 1.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.DualStatus",
    "page": "Reference",
    "title": "MathOptInterface.DualStatus",
    "category": "type",
    "text": "DualStatus(N)\nDualStatus()\n\nThe ResultStatusCode of the dual result N. If N is omitted, it defaults to 1.\n\n\n\n"
},

{
    "location": "apireference.html#Optimizers-1",
    "page": "Reference",
    "title": "Optimizers",
    "category": "section",
    "text": "AbstractOptimizer\noptimize!\nfree!List of attributes optimizers attributesSolverNameList of attributes useful for optimizersRawSolver\nResultCount\nObjectiveFunction\nObjectiveValue\nObjectiveBound\nRelativeGap\nSolveTime\nSimplexIterations\nBarrierIterations\nNodeCount\nTerminationStatus\nPrimalStatus\nDualStatus"
},

{
    "location": "apireference.html#MathOptInterface.TerminationStatusCode",
    "page": "Reference",
    "title": "MathOptInterface.TerminationStatusCode",
    "category": "type",
    "text": "TerminationStatusCode\n\nAn Enum of possible values for the TerminationStatus attribute. This attribute is meant to explain the reason why the optimizer stopped executing.\n\nOK\n\nThese are generally OK statuses.\n\nSuccess: the algorithm ran successfully and has a result; this includes cases where the algorithm converges to an infeasible point (NLP) or converges to a solution of a homogeneous self-dual problem and has a certificate of primal/dual infeasibility\nInfeasibleNoResult: the algorithm stopped because it decided that the problem is infeasible but does not have a result to return\nUnboundedNoResult: the algorithm stopped because it decided that the problem is unbounded but does not have a result to return\nInfeasibleOrUnbounded: the algorithm stopped because it decided that the problem is infeasible or unbounded (no result is available); this occasionally happens during MIP presolve\n\nLimits\n\nThe optimizer stopped because of some user-defined limit. To be documented: IterationLimit, TimeLimit, NodeLimit, SolutionLimit, MemoryLimit, ObjectiveLimit, NormLimit, OtherLimit.\n\nProblematic\n\nThis group of statuses means that something unexpected or problematic happened.\n\nSlowProgress: the algorithm stopped because it was unable to continue making progress towards the solution\nAlmostSuccess should be used if there is additional information that relaxed convergence tolerances are satisfied\n\nTo be documented: NumericalError, InvalidModel, InvalidOption, Interrupted, OtherError.\n\n\n\n"
},

{
    "location": "apireference.html#Termination-Status-1",
    "page": "Reference",
    "title": "Termination Status",
    "category": "section",
    "text": "The TerminationStatus attribute indicates why the optimizer stopped executing. The value of the attribute is of type TerminationStatusCode.TerminationStatusCode"
},

{
    "location": "apireference.html#MathOptInterface.ResultStatusCode",
    "page": "Reference",
    "title": "MathOptInterface.ResultStatusCode",
    "category": "type",
    "text": "ResultStatusCode\n\nAn Enum of possible values for the PrimalStatus and DualStatus attributes. The values indicate how to interpret the result vector.\n\nFeasiblePoint\nNearlyFeasiblePoint\nInfeasiblePoint\nInfeasibilityCertificate\nNearlyInfeasibilityCertificate\nReductionCertificate\nNearlyReductionCertificate\nUnknownResultStatus\nOtherResultStatus\n\n\n\n"
},

{
    "location": "apireference.html#Result-Status-1",
    "page": "Reference",
    "title": "Result Status",
    "category": "section",
    "text": "The PrimalStatus and DualStatus attributes indicate how to interpret the result returned by the solver. The value of the attribute is of type ResultStatusCode.ResultStatusCode"
},

{
    "location": "apireference.html#Variables-and-Constraints-1",
    "page": "Reference",
    "title": "Variables and Constraints",
    "category": "section",
    "text": ""
},

{
    "location": "apireference.html#MathOptInterface.BasisStatusCode",
    "page": "Reference",
    "title": "MathOptInterface.BasisStatusCode",
    "category": "type",
    "text": "BasisStatusCode\n\nAn Enum of possible values for the VariableBasisStatus and ConstraintBasisStatus attribute. This explains the status of a given element with respect to an optimal solution basis. Possible values are:\n\nBasic: element is in the basis\nNonbasic: element is not in the basis\nNonbasicAtLower: element is not in the basis and is at its lower bound\nNonbasicAtUpper: element is not in the basis and is at its upper bound\nSuperBasic: element is not in the basis but is also not at one of its bounds\n\n\n\n"
},

{
    "location": "apireference.html#Basis-Status-1",
    "page": "Reference",
    "title": "Basis Status",
    "category": "section",
    "text": "The BasisStatus attribute of a variable or constraint describes its status with respect to a basis, if one is known. The value of the attribute is of type BasisStatusCode.BasisStatusCode"
},

{
    "location": "apireference.html#MathOptInterface.VariableIndex",
    "page": "Reference",
    "title": "MathOptInterface.VariableIndex",
    "category": "type",
    "text": "VariableIndex\n\nA type-safe wrapper for Int64 for use in referencing variables in a model. To allow for deletion, indices need not be consecutive.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintIndex",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintIndex",
    "category": "type",
    "text": "ConstraintIndex{F,S}\n\nA type-safe wrapper for Int64 for use in referencing F-in-S constraints in a model. The parameter F is the type of the function in the constraint, and the parameter S is the type of set in the constraint. To allow for deletion, indices need not be consecutive. Indices within a constraint type (i.e. F-in-S) must be unique, but non-unique indices across different constraint types are allowed.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.candelete",
    "page": "Reference",
    "title": "MathOptInterface.candelete",
    "category": "function",
    "text": "MOI.candelete(model::MOI.ModelLike, b::AbstractBridge)\n\nReturn a Bool indicating whether the bridge b can be removed from the model model.\n\n\n\ncandelete(model::ModelLike, index::Index)::Bool\n\nReturn a Bool indicating whether the object referred to by index can be removed from the model model.\n\n\n\ncandelete(model::ModelLike, indices::Vector{<:Index})::Bool\n\nReturn a Bool indicating whether all the objects referred to by indices can be removed from the model model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.isvalid",
    "page": "Reference",
    "title": "MathOptInterface.isvalid",
    "category": "function",
    "text": "isvalid(model::ModelLike, index::Index)::Bool\n\nReturn a Bool indicating whether this index refers to a valid object in the model model.\n\n\n\n"
},

{
    "location": "apireference.html#Base.delete!-Tuple{MathOptInterface.ModelLike,Union{MathOptInterface.ConstraintIndex, MathOptInterface.VariableIndex}}",
    "page": "Reference",
    "title": "Base.delete!",
    "category": "method",
    "text": "delete!(model::ModelLike, index::Index)\n\nDelete the referenced object from the model.\n\n\n\n"
},

{
    "location": "apireference.html#Index-types-1",
    "page": "Reference",
    "title": "Index types",
    "category": "section",
    "text": "VariableIndex\nConstraintIndex\ncandelete\nisvalid\ndelete!(::ModelLike,::Index)"
},

{
    "location": "apireference.html#MathOptInterface.canaddvariable",
    "page": "Reference",
    "title": "MathOptInterface.canaddvariable",
    "category": "function",
    "text": "canaddvariable(model::ModelLike)::Bool\n\nReturn a Bool indicating whether it is possible to add a variable to the model model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.addvariables!",
    "page": "Reference",
    "title": "MathOptInterface.addvariables!",
    "category": "function",
    "text": "addvariables!(model::ModelLike, n::Int)::Vector{VariableIndex}\n\nAdd n scalar variables to the model, returning a vector of variable indices.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.addvariable!",
    "page": "Reference",
    "title": "MathOptInterface.addvariable!",
    "category": "function",
    "text": "addvariable!(model::ModelLike)::VariableIndex\n\nAdd a scalar variable to the model, returning a variable index.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VariableName",
    "page": "Reference",
    "title": "MathOptInterface.VariableName",
    "category": "type",
    "text": "VariableName()\n\nA string identifying the variable. It is invalid for two variables to have the same name.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VariablePrimalStart",
    "page": "Reference",
    "title": "MathOptInterface.VariablePrimalStart",
    "category": "type",
    "text": "VariablePrimalStart()\n\nAn initial assignment of the variables that the optimizer may use to warm-start the solve.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VariablePrimal",
    "page": "Reference",
    "title": "MathOptInterface.VariablePrimal",
    "category": "type",
    "text": "VariablePrimal(N)\nVariablePrimal()\n\nThe assignment to the primal variables in result N. If N is omitted, it is 1 by default.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VariableBasisStatus",
    "page": "Reference",
    "title": "MathOptInterface.VariableBasisStatus",
    "category": "type",
    "text": "VariableBasisStatus()\n\nReturns the BasisStatusCode of a given variable, with respect to an available optimal solution basis.\n\n\n\n"
},

{
    "location": "apireference.html#Variables-1",
    "page": "Reference",
    "title": "Variables",
    "category": "section",
    "text": "Functions for adding variables. For deleting, see index types section.canaddvariable\naddvariables!\naddvariable!List of attributes associated with variables. [category AbstractVariableAttribute] Calls to get and set! should include as an argument a single VariableIndex or a vector of VariableIndex objects.VariableName\nVariablePrimalStart\nVariablePrimal\nVariableBasisStatus"
},

{
    "location": "apireference.html#MathOptInterface.isvalid-Tuple{MathOptInterface.ModelLike,MathOptInterface.ConstraintIndex}",
    "page": "Reference",
    "title": "MathOptInterface.isvalid",
    "category": "method",
    "text": "isvalid(model::ModelLike, index::Index)::Bool\n\nReturn a Bool indicating whether this index refers to a valid object in the model model.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.canaddconstraint",
    "page": "Reference",
    "title": "MathOptInterface.canaddconstraint",
    "category": "function",
    "text": "canaddconstraint(model::ModelLike, ::Type{F}, ::Type{S})::Bool where {F<:AbstractFunction,S<:AbstractSet}\n\nReturn a Bool indicating whether it is possible to add a constraint f(x) in mathcalS where f is of type F, and mathcalS is of type S.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.addconstraint!",
    "page": "Reference",
    "title": "MathOptInterface.addconstraint!",
    "category": "function",
    "text": "addconstraint!(model::ModelLike, func::F, set::S)::ConstraintIndex{F,S} where {F,S}\n\nAdd the constraint f(x) in mathcalS where f is defined by func, and mathcalS is defined by set.\n\naddconstraint!(model::ModelLike, v::VariableIndex, set::S)::ConstraintIndex{SingleVariable,S} where {S}\naddconstraint!(model::ModelLike, vec::Vector{VariableIndex}, set::S)::ConstraintIndex{VectorOfVariables,S} where {S}\n\nAdd the constraint v in mathcalS where v is the variable (or vector of variables) referenced by v and mathcalS is defined by set.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.addconstraints!",
    "page": "Reference",
    "title": "MathOptInterface.addconstraints!",
    "category": "function",
    "text": "addconstraints!(model::ModelLike, funcs::Vector{F}, sets::Vector{S})::Vector{ConstraintIndex{F,S}} where {F,S}\n\nAdd the set of constraints specified by each function-set pair in funcs and sets. F and S should be concrete types. This call is equivalent to addconstraint!.(model, funcs, sets) but may be more efficient.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.modifyconstraint!",
    "page": "Reference",
    "title": "MathOptInterface.modifyconstraint!",
    "category": "function",
    "text": "Modify Function\n\nmodifyconstraint!(model::ModelLike, c::ConstraintIndex{F,S}, func::F)\n\nReplace the function in constraint c with func. F must match the original function type used to define the constraint.\n\nExamples\n\nIf c is a ConstraintIndex{ScalarAffineFunction,S} and v1 and v2 are VariableIndex objects,\n\nmodifyconstraint!(model, c, ScalarAffineFunction([v1,v2],[1.0,2.0],5.0))\nmodifyconstraint!(model, c, SingleVariable(v1)) # Error\n\nModify Set\n\nmodifyconstraint!(model::ModelLike, c::ConstraintIndex{F,S}, set::S)\n\nChange the set of constraint c to the new set set which should be of the same type as the original set.\n\nExamples\n\nIf c is a ConstraintIndex{F,Interval}\n\nmodifyconstraint!(model, c, Interval(0, 5))\nmodifyconstraint!(model, c, NonPositives) # Error\n\nPartial Modifications\n\nmodifyconstraint!(model::ModelLike, c::ConstraintIndex, change::AbstractFunctionModification)\n\nApply the modification specified by change to the function of constraint c.\n\nExamples\n\nmodifyconstraint!(model, c, ScalarConstantChange(10.0))\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.canmodifyconstraint",
    "page": "Reference",
    "title": "MathOptInterface.canmodifyconstraint",
    "category": "function",
    "text": "Modify Function\n\ncanmodifyconstraint(model::ModelLike, c::ConstraintIndex{F,S}, ::Type{F})::Bool\n\nReturn a Bool indicating whether the function in constraint c can be replaced by another function of the same type F as the original function.\n\nModify Set\n\ncanmodifyconstraint(model::ModelLike, c::ConstraintIndex{F,S}, ::Type{S})::Bool\n\nReturn a Bool indicating whether the set in constraint c can be replaced by another set of the same type S as the original set.\n\nPartial Modifications\n\ncanmodifyconstraint(model::ModelLike, c::ConstraintIndex, ::Type{M})::Bool where M<:AbstractFunctionModification\n\nReturn a Bool indicating whether it is possible to apply a modification of type M to the function of constraint c.\n\nExamples\n\ncanmodifyconstraint(model, c, ScalarConstantChange{Float64})\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.transformconstraint!",
    "page": "Reference",
    "title": "MathOptInterface.transformconstraint!",
    "category": "function",
    "text": "Transform Constraint Set\n\ntransformconstraint!(model::ModelLike, c::ConstraintIndex{F,S1}, newset::S2)::ConstraintIndex{F,S2}\n\nReplace the set in constraint c with newset. The constraint index c will no longer be valid, and the function returns a new constraint index with the correct type.\n\nSolvers may only support a subset of constraint transforms that they perform efficiently (for example, changing from a LessThan to GreaterThan set). In addition, set modification (where S1 = S2) should be performed via the modifyconstraint! function.\n\nTypically, the user should delete the constraint and add a new one.\n\nExamples\n\nIf c is a ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}},\n\nc2 = transformconstraint!(model, c, GreaterThan(0.0))\ntransformconstraint!(model, c, LessThan(0.0)) # errors\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.cantransformconstraint",
    "page": "Reference",
    "title": "MathOptInterface.cantransformconstraint",
    "category": "function",
    "text": "Transform Constraint Set\n\ncantransformconstraint(model::ModelLike, c::ConstraintIndex{F,S1}, ::Type{S2})::Bool where S2<:AbstractSet\n\nReturn a Bool indicating whether the set of type S1 in constraint c can be replaced by a set of type S2.\n\nExamples\n\nIf c is a ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}},\n\ncantransformconstraint(model, c, GreaterThan(0.0)) # true\ncantransformconstraint(model, c, ZeroOne())        # false\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.supportsconstraint",
    "page": "Reference",
    "title": "MathOptInterface.supportsconstraint",
    "category": "function",
    "text": "supportsconstraint(model::ModelLike, ::Type{F}, ::Type{S})::Bool where {F<:AbstractFunction,S<:AbstractSet}\n\nReturn a Bool indicating whether model supports F-in-S constraints, that is, copy!(model, src) does not return CopyUnsupportedConstraint when src contains F-in-S constraints. If F-in-S constraints are only not supported in specific circumstances, e.g. F-in-S constraints cannot be combined with another type of constraint, it should still return true.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintName",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintName",
    "category": "type",
    "text": "ConstraintName()\n\nA string identifying the constraint. It is invalid for two constraints of any kind to have the same name.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintPrimalStart",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintPrimalStart",
    "category": "type",
    "text": "ConstraintPrimalStart()\n\nAn initial assignment of the constraint primal values that the optimizer may use to warm-start the solve.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintDualStart",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintDualStart",
    "category": "type",
    "text": "ConstraintDualStart()\n\nAn initial assignment of the constraint duals that the optimizer may use to warm-start the solve.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintPrimal",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintPrimal",
    "category": "type",
    "text": "ConstraintPrimal(N)\nConstraintPrimal()\n\nThe assignment to the constraint primal values in result N. If N is omitted, it is 1 by default.\n\nGiven a constraint function-in-set, the ConstraintPrimal is the value of the function evaluated at the primal solution of the variables. For example, given the constraint ScalarAffineFunction([x,y], [1, 2], 3)-in-Interval(0, 20) and  a primal solution of (x,y) = (4,5), the ConstraintPrimal solution of the  constraint is 1 * 4 + 2 * 5 + 3 = 17.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintDual",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintDual",
    "category": "type",
    "text": "ConstraintDual(N)\nConstraintDual()\n\nThe assignment to the constraint dual values in result N. If N is omitted, it is 1 by default.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintBasisStatus",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintBasisStatus",
    "category": "type",
    "text": "ConstraintBasisStatus()\n\nReturns the BasisStatusCode of a given constraint, with respect to an available optimal solution basis.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintFunction",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintFunction",
    "category": "type",
    "text": "ConstraintFunction()\n\nReturn the AbstractFunction object used to define the constraint. It is guaranteed to be equivalent but not necessarily identical to the function provided by the user.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ConstraintSet",
    "page": "Reference",
    "title": "MathOptInterface.ConstraintSet",
    "category": "type",
    "text": "ConstraintSet()\n\nReturn the AbstractSet object used to define the constraint.\n\n\n\n"
},

{
    "location": "apireference.html#Constraints-1",
    "page": "Reference",
    "title": "Constraints",
    "category": "section",
    "text": "Functions for adding and modifying constraints.isvalid(::ModelLike,::ConstraintIndex)\ncanaddconstraint\naddconstraint!\naddconstraints!\nmodifyconstraint!\ncanmodifyconstraint\ntransformconstraint!\ncantransformconstraint\nsupportsconstraintList of attributes associated with constraints. [category AbstractConstraintAttribute] Calls to get and set! should include as an argument a single ConstraintIndex or a vector of ConstraintIndex{F,S} objects.ConstraintName\nConstraintPrimalStart\nConstraintDualStart\nConstraintPrimal\nConstraintDual\nConstraintBasisStatus\nConstraintFunction\nConstraintSet"
},

{
    "location": "apireference.html#MathOptInterface.AbstractFunction",
    "page": "Reference",
    "title": "MathOptInterface.AbstractFunction",
    "category": "type",
    "text": "AbstractFunction\n\nAbstract supertype for function objects.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.SingleVariable",
    "page": "Reference",
    "title": "MathOptInterface.SingleVariable",
    "category": "type",
    "text": "SingleVariable(variable)\n\nThe function that extracts the scalar variable referenced by variable, a VariableIndex. This function is naturally be used for single variable bounds or integrality constraints.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VectorOfVariables",
    "page": "Reference",
    "title": "MathOptInterface.VectorOfVariables",
    "category": "type",
    "text": "VectorOfVariables(variables)\n\nThe function that extracts the vector of variables referenced by variables, a Vector{VariableIndex}. This function is naturally be used for constraints that apply to groups of variables, such as an \"all different\" constraint, an indicator constraint, or a complementarity constraint.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ScalarAffineTerm",
    "page": "Reference",
    "title": "MathOptInterface.ScalarAffineTerm",
    "category": "type",
    "text": "struct ScalarAffineTerm{T}\n    coefficient::T\n    variable_index::VariableIndex\nend\n\nRepresents c x_i where c is coefficient and x_i is the variable identified by variable_index.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ScalarAffineFunction",
    "page": "Reference",
    "title": "MathOptInterface.ScalarAffineFunction",
    "category": "type",
    "text": "ScalarAffineFunction{T}(terms, constant)\n\nThe scalar-valued affine function a^T x + b, where:\n\na is a sparse vector specified by a list of ScalarAffineTerm structs.\nb is a scalar specified by constant::T\n\nDuplicate variable indices in terms are accepted, and the corresponding coefficients are summed together.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VectorAffineTerm",
    "page": "Reference",
    "title": "MathOptInterface.VectorAffineTerm",
    "category": "type",
    "text": "struct VectorAffineTerm{T}\n    output_index::Int64\n    scalar_term::ScalarAffineTerm{T}\nend\n\nA ScalarAffineTerm plus its index of the output component of a VectorAffineFunction or VectorQuadraticFunction. output_index can also be interpreted as a row index into a sparse matrix, where the scalar_term contains the column index and coefficient.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VectorAffineFunction",
    "page": "Reference",
    "title": "MathOptInterface.VectorAffineFunction",
    "category": "type",
    "text": "VectorAffineFunction{T}(terms, constants)\n\nThe vector-valued affine function A x + b, where:\n\nA is a sparse matrix specified by a list of VectorAffineTerm objects.\nb is a vector specified by constants\n\nDuplicate indices in the A are accepted, and the corresponding coefficients are summed together.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ScalarQuadraticTerm",
    "page": "Reference",
    "title": "MathOptInterface.ScalarQuadraticTerm",
    "category": "type",
    "text": "struct ScalarQuadraticTerm{T}\n    coefficient::T\n    variable_index_1::VariableIndex\n    variable_index_2::VariableIndex\nend\n\nRepresents c x_i x_j where c is coefficient, x_i is the variable identified by variable_index_1 and x_j is the variable identified by variable_index_2.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ScalarQuadraticFunction",
    "page": "Reference",
    "title": "MathOptInterface.ScalarQuadraticFunction",
    "category": "type",
    "text": "ScalarQuadraticFunction{T}(affine_terms, quadratic_terms, constant)\n\nThe scalar-valued quadratic function frac12x^TQx + a^T x + b, where:\n\na is a sparse vector specified by a list of ScalarAffineTerm structs.\nb is a scalar specified by constant.\nQ is a symmetric matrix specified by a list of ScalarQuadraticTerm structs.\n\nDuplicate indices in a or Q are accepted, and the corresponding coefficients are summed together. \"Mirrored\" indices (q,r) and (r,q) (where r and q are VariableIndexes) are considered duplicates; only one need be specified.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VectorQuadraticTerm",
    "page": "Reference",
    "title": "MathOptInterface.VectorQuadraticTerm",
    "category": "type",
    "text": "struct VectorQuadraticTerm{T}\n    output_index::Int64\n    scalar_term::ScalarQuadraticTerm{T}\nend\n\nA ScalarQuadraticTerm plus its index of the output component of a VectorQuadraticFunction. Each output component corresponds to a distinct sparse matrix Q_i.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VectorQuadraticFunction",
    "page": "Reference",
    "title": "MathOptInterface.VectorQuadraticFunction",
    "category": "type",
    "text": "VectorQuadraticFunction{T}(affine_terms, quadratic_terms, constant)\n\nThe vector-valued quadratic function with ith component (\"output index\") defined as frac12x^TQ_ix + a_i^T x + b_i, where:\n\na_i is a sparse vector specified by the VectorAffineTerms with output_index == i.\nb_i is a scalar specified by constants[i]\nQ_i is a symmetric matrix specified by the VectorQuadraticTerm with output_index == i.\n\nDuplicate indices in a_i or Q_i are accepted, and the corresponding coefficients are summed together. \"Mirrored\" indices (q,r) and (r,q) (where r and q are VariableIndexes) are considered duplicates; only one need be specified.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ScalarConstantChange",
    "page": "Reference",
    "title": "MathOptInterface.ScalarConstantChange",
    "category": "type",
    "text": "ScalarConstantChange{T}(new_constant)\n\nA struct used to request a change in the constant term of a scalar-valued function. Applicable to ScalarAffineFunction and ScalarQuadraticFunction.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.VectorConstantChange",
    "page": "Reference",
    "title": "MathOptInterface.VectorConstantChange",
    "category": "type",
    "text": "VectorConstantChange{T}(new_constant)\n\nA struct used to request a change in the constant vector of a vector-valued function. Applicable to VectorAffineFunction and VectorQuadraticFunction.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ScalarCoefficientChange",
    "page": "Reference",
    "title": "MathOptInterface.ScalarCoefficientChange",
    "category": "type",
    "text": "ScalarCoefficientChange{T}(variable, new_coefficient)\n\nA struct used to request a change in the linear coefficient of a single variable in a scalar-valued function. Applicable to ScalarAffineFunction and ScalarQuadraticFunction.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.MultirowChange",
    "page": "Reference",
    "title": "MathOptInterface.MultirowChange",
    "category": "type",
    "text": "MultirowChange{T}(variable, new_coefficients)\n\nA struct used to request a change in the linear coefficients of a single variable in a vector-valued function. New coefficients are specified by (output_index, coefficient) tuples. Applicable to VectorAffineFunction and VectorQuadraticFunction.\n\n\n\n"
},

{
    "location": "apireference.html#Functions-and-function-modifications-1",
    "page": "Reference",
    "title": "Functions and function modifications",
    "category": "section",
    "text": "List of recognized functions.AbstractFunction\nSingleVariable\nVectorOfVariables\nScalarAffineTerm\nScalarAffineFunction\nVectorAffineTerm\nVectorAffineFunction\nScalarQuadraticTerm\nScalarQuadraticFunction\nVectorQuadraticTerm\nVectorQuadraticFunctionList of function modifications.ScalarConstantChange\nVectorConstantChange\nScalarCoefficientChange\nMultirowChange"
},

{
    "location": "apireference.html#MathOptInterface.AbstractSet",
    "page": "Reference",
    "title": "MathOptInterface.AbstractSet",
    "category": "type",
    "text": "AbstractSet\n\nAbstract supertype for set objects used to encode constraints.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Reals",
    "page": "Reference",
    "title": "MathOptInterface.Reals",
    "category": "type",
    "text": "Reals(dimension)\n\nThe set mathbbR^dimension (containing all points) of dimension dimension.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Zeros",
    "page": "Reference",
    "title": "MathOptInterface.Zeros",
    "category": "type",
    "text": "Zeros(dimension)\n\nThe set  0 ^dimension (containing only the origin) of dimension dimension.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Nonnegatives",
    "page": "Reference",
    "title": "MathOptInterface.Nonnegatives",
    "category": "type",
    "text": "Nonnegatives(dimension)\n\nThe nonnegative orthant  x in mathbbR^dimension  x ge 0  of dimension dimension.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Nonpositives",
    "page": "Reference",
    "title": "MathOptInterface.Nonpositives",
    "category": "type",
    "text": "Nonpositives(dimension)\n\nThe nonpositive orthant  x in mathbbR^dimension  x le 0  of dimension dimension.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.GreaterThan",
    "page": "Reference",
    "title": "MathOptInterface.GreaterThan",
    "category": "type",
    "text": "GreaterThan{T <: Real}(lower::T)\n\nThe set lowerinfty) subseteq mathbbR.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.LessThan",
    "page": "Reference",
    "title": "MathOptInterface.LessThan",
    "category": "type",
    "text": "LessThan{T <: Real}(upper::T)\n\nThe set (-inftyupper subseteq mathbbR.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.EqualTo",
    "page": "Reference",
    "title": "MathOptInterface.EqualTo",
    "category": "type",
    "text": "EqualTo{T <: Number}(value::T)\n\nThe set containing the single point x in mathbbR where x is given by value.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Interval",
    "page": "Reference",
    "title": "MathOptInterface.Interval",
    "category": "type",
    "text": "Interval{T <: Real}(lower::T,upper::T)\n\nThe interval lower upper subseteq mathbbR. If lower or upper is -Inf or Inf, respectively, the set is interpreted as a one-sided interval.\n\nInterval(s::GreaterThan{<:AbstractFloat})\n\nConstruct a (right-unbounded) Interval equivalent to the given GreaterThan set.\n\nInterval(s::LessThan{<:AbstractFloat})\n\nConstruct a (left-unbounded) Interval equivalent to the given LessThan set.\n\nInterval(s::EqualTo{<:Real})\n\nConstruct a (degenerate) Interval equivalent to the given EqualTo set.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.SecondOrderCone",
    "page": "Reference",
    "title": "MathOptInterface.SecondOrderCone",
    "category": "type",
    "text": "SecondOrderCone(dimension)\n\nThe second-order cone (or Lorenz cone)  (tx) in mathbbR^dimension  t ge  x _2  of dimension dimension.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.RotatedSecondOrderCone",
    "page": "Reference",
    "title": "MathOptInterface.RotatedSecondOrderCone",
    "category": "type",
    "text": "RotatedSecondOrderCone(dimension)\n\nThe rotated second-order cone  (tux) in mathbbR^dimension  2tu ge  x _2^2 tu ge 0  of dimension dimension.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.GeometricMeanCone",
    "page": "Reference",
    "title": "MathOptInterface.GeometricMeanCone",
    "category": "type",
    "text": "GeometricMeanCone(dimension)\n\nThe geometric mean cone  (tx) in mathbbR^n+1  x ge 0 t le sqrtnx_1 x_2 cdots x_n  of dimension dimension=n+1.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ExponentialCone",
    "page": "Reference",
    "title": "MathOptInterface.ExponentialCone",
    "category": "type",
    "text": "ExponentialCone()\n\nThe 3-dimensional exponential cone  (xyz) in mathbbR^3  y exp (xy) le z y  0 .\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.DualExponentialCone",
    "page": "Reference",
    "title": "MathOptInterface.DualExponentialCone",
    "category": "type",
    "text": "DualExponentialCone()\n\nThe 3-dimensional dual exponential cone  (uvw) in mathbbR^3  -u exp (vu) le exp(1) w u  0 .\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.PowerCone",
    "page": "Reference",
    "title": "MathOptInterface.PowerCone",
    "category": "type",
    "text": "PowerCone{T <: Real}(exponent::T)\n\nThe 3-dimensional power cone  (xyz) in mathbbR^3  x^exponent y^1-exponent = z x ge 0 y ge 0  with parameter exponent.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.DualPowerCone",
    "page": "Reference",
    "title": "MathOptInterface.DualPowerCone",
    "category": "type",
    "text": "DualPowerCone{T <: Real}(exponent::T)\n\nThe 3-dimensional power cone  (uvw) in mathbbR^3  (fracuexponent)^exponent (fracv1-exponent)^1-exponent ge w u ge 0 v ge 0  with parameter exponent.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.PositiveSemidefiniteConeTriangle",
    "page": "Reference",
    "title": "MathOptInterface.PositiveSemidefiniteConeTriangle",
    "category": "type",
    "text": "PositiveSemidefiniteConeTriangle(dimension)\n\nThe (vectorized) cone of symmetric positive semidefinite matrices, with side length dimension and with off-diagonals unscaled. The entries of the upper triangular part of the matrix are given column by column (or equivalently, the entries of the lower triangular part are given row by row). An dimension times dimension matrix has dimension(dimension+1)2 lower-triangular elements, so for the vectorized cone of dimension n, the corresponding symmetric matrix has side dimension sqrt14 + 2 n - 12 elements.\n\nExamples\n\nThe matrix\n\nbeginbmatrix\n  1  2  4\n  2  3  5\n  4  5  6\nendbmatrix\n\ncorresponds to (1 2 3 4 5 6) for PositiveSemidefiniteConeTriangle(3)\n\nNote\n\nTwo packed storage formats exist for symmetric matrices, the respective orders of the entries are:\n\nupper triangular column by column (or lower triangular row by row);\nlower triangular column by column (or upper triangular row by row).\n\nThe advantage of the first format is the mapping between the (i, j) matrix indices and the k index of the vectorized form. It is simpler and does not depend on the dimension of the matrix. Indeed,\n\nthe entry of matrix indices (i, j) has vectorized index k = div((j-1)*j, 2) + i if i leq j and k = div((i-1)*i, 2) + j if j leq i;\nand the entry with vectorized index k has matrix indices i = isqrt(2k) and j = k - div((i-1)*i, 2) or j = isqrt(2k) and i = k - div((j-1)*j, 2).\n\nDuality note\n\nThe scalar product for the symmetric matrix in its vectorized form is the sum of the pairwise product of the diagonal entries plus twice the sum of the pairwise product of the upper diagonal entries; see [p. 634, 1]. This has important consequence for duality. Consider for example the following problem\n\nbeginalign*\n     max_x in mathbbR  x\n    \n     textst \n    (1 -x 1)  in textPositiveSemidefiniteConeTriangle(2)\nendalign*\n\nThe dual is the following problem\n\nbeginalign*\n     min_x in mathbbR^3  y_1 + y_3\n    \n     textst  2y_2  = 1\n      y  in textPositiveSemidefiniteConeTriangle(2)\nendalign*\n\nWhy do we use 2y_2 in the dual constraint instead of y_2 ? The reason is that 2y_2 is the scalar product between y and the symmetric matrix whose vectorized form is (0 1 0). Indeed, with our modified scalar products we have\n\nlangle\n(0 1 0)\n(y_1 y_2 y_3)\nrangle\n=\nmathrmtrace\nbeginpmatrix\n  0  1\n  1  0\nendpmatrix\nbeginpmatrix\n  y_1  y_2\n  y_2  y_3\nendpmatrix\n= 2y_2\n\nReferences\n\n[1] Boyd, S. and Vandenberghe, L.. Convex optimization. Cambridge university press, 2004.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.PositiveSemidefiniteConeSquare",
    "page": "Reference",
    "title": "MathOptInterface.PositiveSemidefiniteConeSquare",
    "category": "type",
    "text": "PositiveSemidefiniteConeSquare(dimension)\n\nThe cone of symmetric positive semidefinite matrices, with side length dimension. The entries of the matrix are given column by column (or equivalently, row by row). The matrix is both constrained to be symmetric and to be positive semidefinite. That is, if the functions in entries (i j) and (j i) are different, then a constraint will be added to make sure that the entries are equal.\n\nExamples\n\nConstraining the matrix\n\nbeginbmatrix\n  1  -y\n  -z  0\nendbmatrix\n\nto be symmetric positive semidefinite can be achieved by constraining the vector (1 -z -y 0) (or (1 -y -z 0)) to belong to the PositiveSemidefiniteConeSquare(2). It both constrains y = z and (1 -y 0) (or (1 -z 0)) to be in PositiveSemidefiniteConeTriangle(2).\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.LogDetConeTriangle",
    "page": "Reference",
    "title": "MathOptInterface.LogDetConeTriangle",
    "category": "type",
    "text": "LogDetConeTriangle(dimension)\n\nThe Log-Determinant cone  (t X) in mathbbR^1 + d(d+1)2  t le log(det(X))  where the matrix X is represented in the same symmetric packed format as in the PositiveSemidefiniteConeTriangle. The argument dimension is the side dimension of the matrix X, i.e., its number of rows or columns.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.LogDetConeSquare",
    "page": "Reference",
    "title": "MathOptInterface.LogDetConeSquare",
    "category": "type",
    "text": "LogDetConeSquare(dimension)\n\nThe Log-Determinant cone  (t X) in mathbbR^1 + d^2  t le log(det(X)) X text symmetric  where the matrix X is represented in the same format as in the PositiveSemidefiniteConeSquare. Similarly to PositiveSemidefiniteConeSquare, constraints are added to ensures that X is symmetric. The argument dimension is the side dimension of the matrix X, i.e., its number of rows or columns.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.RootDetConeTriangle",
    "page": "Reference",
    "title": "MathOptInterface.RootDetConeTriangle",
    "category": "type",
    "text": "RootDetConeTriangle(dimension)\n\nThe Root-Determinant cone  (t X) in mathbbR^1 + d(d+1)2  t le det(X)^1d  where the matrix X is represented in the same symmetric packed format as in the PositiveSemidefiniteConeTriangle. The argument dimension is the side dimension of the matrix X, i.e., its number of rows or columns.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.RootDetConeSquare",
    "page": "Reference",
    "title": "MathOptInterface.RootDetConeSquare",
    "category": "type",
    "text": "RootDetConeSquare(dimension)\n\nThe Root-Determinant cone  (t X) in mathbbR^1 + d^2  t le det(X)^1d X text symmetric  where the matrix X is represented in the same format as in the PositiveSemidefiniteConeSquare. Similarly to PositiveSemidefiniteConeSquare, constraints are added to ensures that X is symmetric. The argument dimension is the side dimension of the matrix X, i.e., its number of rows or columns.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Integer",
    "page": "Reference",
    "title": "MathOptInterface.Integer",
    "category": "type",
    "text": "Integer()\n\nThe set of integers mathbbZ.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.ZeroOne",
    "page": "Reference",
    "title": "MathOptInterface.ZeroOne",
    "category": "type",
    "text": "ZeroOne()\n\nThe set  0 1 .\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Semicontinuous",
    "page": "Reference",
    "title": "MathOptInterface.Semicontinuous",
    "category": "type",
    "text": "Semicontinuous{T <: Real}(lower::T,upper::T)\n\nThe set 0 cup lowerupper.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Semiinteger",
    "page": "Reference",
    "title": "MathOptInterface.Semiinteger",
    "category": "type",
    "text": "Semiinteger{T <: Real}(lower::T,upper::T)\n\nThe set 0 cup lowerlower+1ldotsupper-1upper.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.SOS1",
    "page": "Reference",
    "title": "MathOptInterface.SOS1",
    "category": "type",
    "text": "SOS1{T <: Real}(weights::Vector{T})\n\nThe set corresponding to the special ordered set (SOS) constraint of type 1. Of the variables in the set, at most one can be nonzero. The weights induce an ordering of the variables; as such, they should be unique values. The kth element in the set corresponds to the kth weight in weights. See here for a description of SOS constraints and their potential uses.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.SOS2",
    "page": "Reference",
    "title": "MathOptInterface.SOS2",
    "category": "type",
    "text": "SOS2{T <: Real}(weights::Vector{T})\n\nThe set corresponding to the special ordered set (SOS) constraint of type 2. Of the variables in the set, at most two can be nonzero, and if two are nonzero, they must be adjacent in the ordering of the set. The weights induce an ordering of the variables; as such, they should be unique values. The kth element in the set corresponds to the kth weight in weights. See here for a description of SOS constraints and their potential uses.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.dimension",
    "page": "Reference",
    "title": "MathOptInterface.dimension",
    "category": "function",
    "text": "dimension(s::AbstractVectorSet)\n\nReturn the underlying dimension (number of vector components) in the set s, i.e., n if the set is a subset of mathbbR^n.\n\n\n\n"
},

{
    "location": "apireference.html#Sets-1",
    "page": "Reference",
    "title": "Sets",
    "category": "section",
    "text": "List of recognized sets.AbstractSet\nReals\nZeros\nNonnegatives\nNonpositives\nGreaterThan\nLessThan\nEqualTo\nInterval\nSecondOrderCone\nRotatedSecondOrderCone\nGeometricMeanCone\nExponentialCone\nDualExponentialCone\nPowerCone\nDualPowerCone\nPositiveSemidefiniteConeTriangle\nPositiveSemidefiniteConeSquare\nLogDetConeTriangle\nLogDetConeSquare\nRootDetConeTriangle\nRootDetConeSquare\nInteger\nZeroOne\nSemicontinuous\nSemiinteger\nSOS1\nSOS2Functions for getting and setting properties of sets.dimension"
},

{
    "location": "apireference.html#MathOptInterface.modifyobjective!",
    "page": "Reference",
    "title": "MathOptInterface.modifyobjective!",
    "category": "function",
    "text": "modifyobjective!(model::ModekLike, change::AbstractFunctionModification)\n\nApply the modification specified by change to the objective function of model. To change the function completely, call setobjective! instead.\n\nExamples\n\nmodifyobjective!(model, ScalarConstantChange(10.0))\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.canmodifyobjective",
    "page": "Reference",
    "title": "MathOptInterface.canmodifyobjective",
    "category": "function",
    "text": "canmodifyobjective(model::ModelLike, ::Type{M})::Bool where M<:AbstractFunctionModification\n\nReturn a Bool indicating whether it is possible to apply a modification of type M to the objective function of model model.\n\nExamples\n\ncanmodifyobjective(model, ScalarConstantChange{Float64})\n\n\n\n"
},

{
    "location": "apireference.html#Objective-modifications-1",
    "page": "Reference",
    "title": "Objective modifications",
    "category": "section",
    "text": "Functions for modifying objective functions. Use ObjectiveFunction and ObjectiveSense to set and query the objective function.modifyobjective!\ncanmodifyobjective"
},

{
    "location": "apireference.html#Nonlinear-programming-(NLP)-1",
    "page": "Reference",
    "title": "Nonlinear programming (NLP)",
    "category": "section",
    "text": ""
},

{
    "location": "apireference.html#MathOptInterface.NLPBlock",
    "page": "Reference",
    "title": "MathOptInterface.NLPBlock",
    "category": "type",
    "text": "NLPBlock()\n\nHolds the NLPBlockData that represents a set of nonlinear constraints, and optionally a nonlinear objective.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.NLPBoundsPair",
    "page": "Reference",
    "title": "MathOptInterface.NLPBoundsPair",
    "category": "type",
    "text": "NLPBoundsPair(lower,upper)\n\nA struct holding a pair of lower and upper bounds. -Inf and Inf can be used to indicate no lower or upper bound, respectively.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.NLPBlockData",
    "page": "Reference",
    "title": "MathOptInterface.NLPBlockData",
    "category": "type",
    "text": "struct NLPBlockData\n    constraint_bounds::Vector{NLPBoundsPair}\n    evaluator::AbstractNLPEvaluator\n    has_objective::Bool\nend\n\nA struct encoding a set of nonlinear constraints of the form lb le g(x) le ub and, if has_objective == true, a nonlinear objective function f(x). constraint_bounds holds the pairs of lb and ub elements. It is an error to set both a nonlinear objective function and another objective function using an ObjectiveFunction attribute. The evaluator is a callback object that is used to query function values, derivatives, and expression graphs. If has_objective == false, then it is an error to query properties of the objective function, and in Hessian-of-the-Lagrangian queries, σ must be set to zero. Throughout the evaluator, all variables are ordered according to ListOfVariableIndices(). \n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.NLPBlockDual",
    "page": "Reference",
    "title": "MathOptInterface.NLPBlockDual",
    "category": "type",
    "text": "NLPBlockDual(N)\nNLPBlockDual()\n\nThe Lagrange multipliers on the constraints from the NLPBlock in result N. If N is omitted, it is 1 by default.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.NLPBlockDualStart",
    "page": "Reference",
    "title": "MathOptInterface.NLPBlockDualStart",
    "category": "type",
    "text": "NLPBlockDualStart()\n\nAn initial assignment of the Lagrange multipliers on the constraints from the NLPBlock that the solver may use to warm-start the solve.\n\n\n\n"
},

{
    "location": "apireference.html#Attributes-2",
    "page": "Reference",
    "title": "Attributes",
    "category": "section",
    "text": "NLPBlock\nNLPBoundsPair\nNLPBlockData\nNLPBlockDual\nNLPBlockDualStart"
},

{
    "location": "apireference.html#MathOptInterface.AbstractNLPEvaluator",
    "page": "Reference",
    "title": "MathOptInterface.AbstractNLPEvaluator",
    "category": "type",
    "text": "AbstractNLPEvaluator\n\nAbstract supertype for the callback object used in NLPBlock.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.initialize!",
    "page": "Reference",
    "title": "MathOptInterface.initialize!",
    "category": "function",
    "text": "initialize!(d::AbstractNLPEvaluator, requested_features::Vector{Symbol})\n\nMust be called before any other methods. The vector requested_features lists features requested by the solver. These may include :Grad for gradients of f, :Jac for explicit Jacobians of g, :JacVec for Jacobian-vector products, :HessVec for Hessian-vector and Hessian-of-Lagrangian-vector products, :Hess for explicit Hessians and Hessian-of-Lagrangians, and :ExprGraph for expression graphs.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.features_available",
    "page": "Reference",
    "title": "MathOptInterface.features_available",
    "category": "function",
    "text": "features_available(d::AbstractNLPEvaluator)\n\nReturns the subset of features available for this problem instance, as a list of symbols in the same format as in initialize.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.eval_objective",
    "page": "Reference",
    "title": "MathOptInterface.eval_objective",
    "category": "function",
    "text": "eval_objective(d::AbstractNLPEvaluator, x)\n\nEvaluate the objective f(x), returning a scalar value.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.eval_constraint",
    "page": "Reference",
    "title": "MathOptInterface.eval_constraint",
    "category": "function",
    "text": "eval_constraint(d::AbstractNLPEvaluator, g, x)\n\nEvaluate the constraint function g(x), storing the result in the vector g which must be of the appropriate size.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.eval_objective_gradient",
    "page": "Reference",
    "title": "MathOptInterface.eval_objective_gradient",
    "category": "function",
    "text": "eval_objective_gradient(d::AbstractNLPEvaluator, g, x)\n\nEvaluate nabla f(x) as a dense vector, storing the result in the vector g which must be of the appropriate size.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.jacobian_structure",
    "page": "Reference",
    "title": "MathOptInterface.jacobian_structure",
    "category": "function",
    "text": "jacobian_structure(d::AbstractNLPEvaluator)::Vector{Tuple{Int64,Int64}}\n\nReturns the sparsity structure of the Jacobian matrix J_g(x) = left beginarrayc nabla g_1(x)  nabla g_2(x)  vdots  nabla g_m(x) endarrayright where g_i is the itextth component of g. The sparsity structure is assumed to be independent of the point x. Returns a vector of tuples, (row, column), where each indicates the position of a structurally nonzero element. These indices are not required to be sorted and can contain duplicates, in which case the solver should combine the corresponding elements by adding them together.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.hessian_lagrangian_structure",
    "page": "Reference",
    "title": "MathOptInterface.hessian_lagrangian_structure",
    "category": "function",
    "text": "hessian_lagrangian_structure(d::AbstractNLPEvaluator)::Vector{Tuple{Int64,Int64}}\n\nReturns the sparsity structure of the Hessian-of-the-Lagrangian matrix nabla^2 f + sum_i=1^m nabla^2 g_i as a vector of tuples, where each indicates the position of a structurally nonzero element. These indices are not required to be sorted and can contain duplicates, in which case the solver should combine the corresponding elements by adding them together. Any mix of lower and upper-triangular indices is valid. Elements (i,j) and (j,i), if both present, should be treated as duplicates.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.eval_constraint_jacobian",
    "page": "Reference",
    "title": "MathOptInterface.eval_constraint_jacobian",
    "category": "function",
    "text": "eval_constraint_jacobian(d::AbstractNLPEvaluator, J, x)\n\nEvaluates the sparse Jacobian matrix J_g(x) = left beginarrayc nabla g_1(x)  nabla g_2(x)  vdots  nabla g_m(x) endarrayright. The result is stored in the vector J in the same order as the indices returned by jacobian_structure.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.eval_constraint_jacobian_product",
    "page": "Reference",
    "title": "MathOptInterface.eval_constraint_jacobian_product",
    "category": "function",
    "text": "eval_constraint_jacobian_product(d::AbstractNLPEvaluator, y, x, w)\n\nComputes the Jacobian-vector product J_g(x)w, storing the result in the vector y.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.eval_constraint_jacobian_transpose_product",
    "page": "Reference",
    "title": "MathOptInterface.eval_constraint_jacobian_transpose_product",
    "category": "function",
    "text": "eval_constraint_jacobian_transpose_product(d::AbstractNLPEvaluator, y, x, w)\n\nComputes the Jacobian-transpose-vector product J_g(x)^Tw, storing the result in the vector y.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.eval_hessian_lagrangian",
    "page": "Reference",
    "title": "MathOptInterface.eval_hessian_lagrangian",
    "category": "function",
    "text": "eval_hessian_lagrangian(d::AbstractNLPEvaluator, H, x, σ, μ)\n\nGiven scalar weight σ and vector of constraint weights μ, computes the sparse Hessian-of-the-Lagrangian matrix sigmanabla^2 f(x) + sum_i=1^m mu_i nabla^2 g_i(x), storing the result in the vector H in the same order as the indices returned by hessian_lagrangian_structure.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.eval_hessian_lagrangian_product",
    "page": "Reference",
    "title": "MathOptInterface.eval_hessian_lagrangian_product",
    "category": "function",
    "text": "eval_hessian_lagrangian_prod(d::AbstractNLPEvaluator, h, x, v, σ, μ)\n\nGiven scalar weight σ and vector of constraint weights μ, computes the Hessian-of-the-Lagrangian-vector product left(sigmanabla^2 f(x) + sum_i=1^m mu_i nabla^2 g_i(x)right)v, storing the result in the vector h.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.objective_expr",
    "page": "Reference",
    "title": "MathOptInterface.objective_expr",
    "category": "function",
    "text": "objective_expr(d::AbstractNLPEvaluator)\n\nReturns an expression graph for the objective function as a standard Julia Expr object. All sums and products are flattened out as simple Expr(:+,...) and Expr(:*,...) objects. The symbol x is used as a placeholder for the vector of decision variables. No other undefined symbols are permitted; coefficients are embedded as explicit values. For example, the expression x_1+sin(x_2exp(x_3)) would be represented as the Julia object :(x[1] + sin(x[2]/exp(x[3]))). See the Julia manual for more information on the structure of Expr objects. There are currently no restrictions on recognized functions; typically these will be built-in Julia functions like ^, exp, log, cos, tan, sqrt, etc., but modeling interfaces may choose to extend these basic functions.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.constraint_expr",
    "page": "Reference",
    "title": "MathOptInterface.constraint_expr",
    "category": "function",
    "text": "constraint_expr(d::AbstractNLPEvaluator, i)\n\nReturns an expression graph for the itextth constraint in the same format as described above, with an additional comparison operator indicating the sense of and bounds on the constraint. The right-hand side of the comparison must be a constant; that is, :(x[1]^3 <= 1) is allowed, while :(1 <= x[1]^3) is not valid. Double-sided constraints are allowed, in which case both the lower bound and upper bounds should be constants; for example, :(-1 <= cos(x[1]) + sin(x[2]) <= 1) is valid.\n\n\n\n"
},

{
    "location": "apireference.html#NLP-evaluator-methods-1",
    "page": "Reference",
    "title": "NLP evaluator methods",
    "category": "section",
    "text": "AbstractNLPEvaluator\ninitialize!\nfeatures_available\neval_objective\neval_constraint\neval_objective_gradient\njacobian_structure\nhessian_lagrangian_structure\neval_constraint_jacobian\neval_constraint_jacobian_product\neval_constraint_jacobian_transpose_product\neval_hessian_lagrangian\neval_hessian_lagrangian_product\nobjective_expr\nconstraint_expr"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.AbstractBridge",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.AbstractBridge",
    "category": "type",
    "text": "AbstractBridge\n\nA bridge represents a bridged constraint in an AbstractBridgeOptimizer. It contains the indices of the constraints that it has created in the model. These can be obtained using MOI.NumberOfConstraints and MOI.ListOfConstraintIndices and using the bridge in place of a ModelLike. Attributes of the bridged model such as MOI.ConstraintDual and MOI.ConstraintPrimal, can be obtained using the bridge in place of the constraint index. These calls are used by the AbstractBridgeOptimizer to communicate with the bridge so they should be implemented by the bridge.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.AbstractBridgeOptimizer",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.AbstractBridgeOptimizer",
    "category": "type",
    "text": "AbstractBridgeOptimizer\n\nA bridge optimizer applies a given constraint bridge to a given optimizer. The attributes of the bridge optimizer are automatically computed to make the bridges transparent, e.g. the variables and constraints created by the bridges are hidden.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.SingleBridgeOptimizer",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.SingleBridgeOptimizer",
    "category": "type",
    "text": "SingleBridgeOptimizer{BT<:AbstractBridge, MT<:MOI.ModelLike, OT<:MOI.ModelLike} <: AbstractBridgeOptimizer\n\nThe SingleBridgeOptimizer bridges any constraint supported by the bridge BT. This is in contrast with the LazyBridgeOptimizer which only bridges the constraints that are unsupported by the internal model, even if they are supported by one of its bridges.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.LazyBridgeOptimizer",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.LazyBridgeOptimizer",
    "category": "type",
    "text": "LazyBridgeOptimizer{OT<:MOI.ModelLike, MT<:MOI.ModelLike} <: AbstractBridgeOptimizer\n\nThe LazyBridgeOptimizer combines several bridges, which are added using the addbridge! function. Whenever a constraint is added, it only attempts to bridge it if it is not supported by the internal model (hence its name Lazy). When bridging a constraint, it selects the minimal number of bridges needed. For instance, a constraint F-in-S can be bridged into a constraint F1-in-S1 (supported by the internal model) using bridge 1 or bridged into a constraint F2-in-S2 (unsupported by the internal model) using bridge 2 which can then be bridged into a constraint F3-in-S3 (supported by the internal model) using bridge 3, it will choose bridge 1 as it allows to bridge F-in-S using only one bridge instead of two if it uses bridge 2 and 3.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.addbridge!",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.addbridge!",
    "category": "function",
    "text": "addbridge!(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})\n\nEnable the use of the bridges of type BT by b.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.SplitIntervalBridge",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.SplitIntervalBridge",
    "category": "type",
    "text": "SplitIntervalBridge{T}\n\nThe SplitIntervalBridge splits a constraint l  a x +   u into the constraints a x +   l and a x +   u.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.RSOCBridge",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.RSOCBridge",
    "category": "type",
    "text": "RSOCBridge{T}\n\nThe RotatedSecondOrderCone is SecondOrderCone representable; see [1, p. 104]. Indeed, we have 2tu = (t2 + u2)^2 - (t2 - u2)^2 hence\n\n2tu ge  x _2^2\n\nis equivalent to\n\n(t2 + u2)^2 ge  x _2^2 + (t2 - u2)^2\n\nWe can therefore use the transformation (t u x) mapsto (t2+u2 t2-u2 x). Note that the linear transformation is a symmetric involution (i.e. it is its own transpose and its own inverse). That means in particular that the norm is of constraint primal and duals are preserved by the tranformation.\n\n[1] Ben-Tal, Aharon, and Arkadi Nemirovski. Lectures on modern convex optimization: analysis, algorithms, and engineering applications. Society for Industrial and Applied Mathematics, 2001.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.GeoMeanBridge",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.GeoMeanBridge",
    "category": "type",
    "text": "GeoMeanBridge{T}\n\nThe GeometricMeanCone is SecondOrderCone representable; see [1, p. 105]. The reformulation is best described in an example. Consider the cone of dimension 4\n\nt le sqrt3x_1 x_2 x_3\n\nThis can be rewritten as exists x_21 ge 0 such that\n\nbeginalign*\n  t  le x_21\n  x_21^4  le x_1 x_2 x_3 x_21\nendalign*\n\nNote that we need to create x_21 and not use t^4 directly as t is allowed to be negative. Now, this is equivalent to\n\nbeginalign*\n  t  le x_21sqrt4\n  x_21^2  le 2x_11 x_12\n  x_11^2  le 2x_1 x_2  x_21^2  le 2x_3(x_21sqrt4)\nendalign*\n\n[1] Ben-Tal, Aharon, and Arkadi Nemirovski. Lectures on modern convex optimization: analysis, algorithms, and engineering applications. Society for Industrial and Applied Mathematics, 2001.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.RootDetBridge",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.RootDetBridge",
    "category": "type",
    "text": "RootDetBridge{T}\n\nThe RootDetConeTriangle is representable by a PositiveSemidefiniteConeTriangle and an GeometricMeanCone constraints; see [1, p. 149]. Indeed, t le det(X)^(1n) if and only if there exists a lower triangular matrix  such that\n\nbeginalign*\n  beginpmatrix\n    X  \n    ^top  mathrmDiag()\n  endpmatrix  succeq 0\n  t  le (_11 _22 cdots _nn)^1n\nendalign*\n\n[1] Ben-Tal, Aharon, and Arkadi Nemirovski. Lectures on modern convex optimization: analysis, algorithms, and engineering applications. Society for Industrial and Applied Mathematics, 2001.\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.LogDetBridge",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.LogDetBridge",
    "category": "type",
    "text": "LogDetBridge{T}\n\nThe LogDetConeTriangle is representable by a PositiveSemidefiniteConeTriangle and ExponentialCone constraints. Indeed, logdet(X) = log(delta_1) + cdots + log(delta_n) where delta_1, ..., delta_n are the eigenvalues of X. Adapting, the method from [1, p. 149], we see that t le log(det(X)) if and only if there exists a lower triangular matrix  such that\n\nbeginalign*\n  beginpmatrix\n    X  \n    ^top  mathrmDiag()\n  endpmatrix  succeq 0\n  t  le log(_11) + log(_22) + cdots + log(_nn)\nendalign*\n\n[1] Ben-Tal, Aharon, and Arkadi Nemirovski. Lectures on modern convex optimization: analysis, algorithms, and engineering applications. Society for Industrial and Applied Mathematics, 2001. ```\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.SOCtoPSDCBridge",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.SOCtoPSDCBridge",
    "category": "type",
    "text": "The SOCtoPSDCBridge transforms the second order cone constraint lVert x rVert le t into the semidefinite cone constraints\n\nbeginpmatrix\n  t  x^top\n  x  tI\nendpmatrix succeq 0\n\nIndeed by the Schur Complement, it is positive definite iff\n\nbeginalign*\n  tI  succ 0\n  t - x^top (tI)^-1 x  succ 0\nendalign*\n\nwhich is equivalent to\n\nbeginalign*\n  t   0\n  t^2   x^top x\nendalign*\n\n\n\n"
},

{
    "location": "apireference.html#MathOptInterface.Bridges.RSOCtoPSDCBridge",
    "page": "Reference",
    "title": "MathOptInterface.Bridges.RSOCtoPSDCBridge",
    "category": "type",
    "text": "The RSOCtoPSDCBridge transforms the second order cone constraint lVert x rVert le 2tu with u ge 0 into the semidefinite cone constraints\n\nbeginpmatrix\n  t  x^top\n  x  2uI\nendpmatrix succeq 0\n\nIndeed by the Schur Complement, it is positive definite iff\n\nbeginalign*\n  uI  succ 0\n  t - x^top (2uI)^-1 x  succ 0\nendalign*\n\nwhich is equivalent to\n\nbeginalign*\n  u   0\n  2tu   x^top x\nendalign*\n\n\n\n"
},

{
    "location": "apireference.html#Bridges-1",
    "page": "Reference",
    "title": "Bridges",
    "category": "section",
    "text": "Bridges can be used for automatic reformulation of a certain constraint type into equivalent constraints.Bridges.AbstractBridge\nBridges.AbstractBridgeOptimizer\nBridges.SingleBridgeOptimizer\nBridges.LazyBridgeOptimizer\nBridges.addbridge!Below is the list of bridges implemented in this package.Bridges.SplitIntervalBridge\nBridges.RSOCBridge\nBridges.GeoMeanBridge\nBridges.RootDetBridge\nBridges.LogDetBridge\nBridges.SOCtoPSDCBridge\nBridges.RSOCtoPSDCBridgeFor each bridge defined in this package, a corresponding bridge optimizer is available with the same name but \"Bridge\" suffix, e.g. SplitInterval is an SingleBridgeOptimizer for the SplitIntervalBridge."
},

]}
