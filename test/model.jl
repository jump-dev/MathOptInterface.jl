# Needed by test spread over several files, defining it here make it easier to comment out tests
# Model supporting every MOI functions and sets
MOIU.@model(Model,
            (MOI.ZeroOne, MOI.Integer),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval,
             MOI.Semicontinuous, MOI.Semiinteger),
            (MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives,
             MOI.SecondOrderCone, MOI.RotatedSecondOrderCone,
             MOI.GeometricMeanCone, MOI.ExponentialCone, MOI.DualExponentialCone,
             MOI.PositiveSemidefiniteConeTriangle, MOI.PositiveSemidefiniteConeSquare,
             MOI.RootDetConeTriangle, MOI.RootDetConeSquare, MOI.LogDetConeTriangle,
             MOI.LogDetConeSquare),
            (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2),
            (MOI.SingleVariable,),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))
