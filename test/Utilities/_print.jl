using Test
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

@test sprint(MOIU._print, "MathOptInterface") == "MOI"
@test sprint(MOIU._print, "MathOptInterface.MathOptInterface") == "MOI.MOI"
@test sprint(MOIU._print, "MathOptInterface.Utilities.MathOptInterface") == "MOIU.MOI"
@test sprint(MOIU._print, "MathOptInterfaceXXBridges") == "MOIXXBridges"
@test sprint(MOIU._print, "MathOptInterface.BridgesXX") == "MOIBXX"
@test sprint(MOIU._print, "MathOptInterface.Test.x") == "MOIT.x"
@test sprint(MOIU._print, "MathOptInterface.x.Test") == "MOI.x.Test"
@test sprint(MOIU._print, "MathOptInterface.Utilities.Test") == "MOIU.Test"
@test sprint(MOIU._print, "MathOptInterface.Utilities.Test") == "MOIU.Test"