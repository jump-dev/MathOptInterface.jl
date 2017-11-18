struct TestConfig
atol::Float64 # absolute tolerance for ...
rtol::Float64 # relative tolerance for ...
query::Bool # can get objective function, and constraint functions, and constraint sets
duals::Bool # test dual solutions
infeas_certificates::Bool # check for infeasibility certificates when appropriate
end
