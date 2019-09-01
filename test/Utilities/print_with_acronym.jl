using Test
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

@test sprint(MOIU.print_with_acronym, "MathOptInterface") == "MOI"
@test sprint(MOIU.print_with_acronym, "MathOptInterface.MathOptInterface") == "MOI.MOI"
@test sprint(MOIU.print_with_acronym, "MathOptInterface.Utilities.MathOptInterface") == "MOIU.MOI"
@test sprint(MOIU.print_with_acronym, "MathOptInterfaceXXBridges") == "MOIXXBridges"
@test sprint(MOIU.print_with_acronym, "MathOptInterface.BridgesXX") == "MOIBXX"
@test sprint(MOIU.print_with_acronym, "MathOptInterface.Test.x") == "MOIT.x"
@test sprint(MOIU.print_with_acronym, "MathOptInterface.x.Test") == "MOI.x.Test"
@test sprint(MOIU.print_with_acronym, "MathOptInterface.Utilities.Test") == "MOIU.Test"
@test sprint(MOIU.print_with_acronym, "MathOptInterface.Utilities.Test") == "MOIU.Test"
