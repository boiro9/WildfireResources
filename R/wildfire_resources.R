#' Model to solve the Aircraft Selection and Allocation problem (ASA) for the containment of a wildfire.
#'
#' @param data a list with the needed information about aircrafts and wildfire. For more information see \code{\link{example_data}} function.
#' @param solver solver name, 'gurobi', 'Rsymphony' or 'lpSolve'.
#' @param solver_params list of gurobi options. Defaults to list(TimeLimit=600, OutputFlag = 0).
#'
#' @return information about the selection and allocation of the aircrafts.
#' @export
#'
#' @examples
#' data <- WildfireResources::example_data()
#' WildfireResources::wildfire_resources(data, solver="lpSolveAPI")
wildfire_resources <- function(data,
                               solver="gurobi", 
                               solver_params=list(TimeLimit=600, OutputFlag=0)){

  results <- WildfireResources::exact_model(
    data, solver=solver, solver_params=solver_params)

  if(results$sol_result == "OPTIMAL"){
    return(list(st_model = results, nd_model = NULL))
  }else{
    inf_model_result <- WildfireResources::infeasible_solution(data, solver, 
                                                               M_prime=1000)
  }
  
  return(list(st_model = results, nd_model = inf_model_result))
}