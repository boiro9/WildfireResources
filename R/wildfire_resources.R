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
#' 
#' resources_file <- 'example/feasible/Resources.csv'
#' fire_file <- 'example/feasible/Fire.csv'
#' csvs <- WildfireResources::load_data(resources_file, fire_file)
#' data1 <- WildfireResources::get_data(csvs$data.resources, csvs$data.fire, 10)
#' sol <- WildfireResources::wildfire_resources(data1)
#' sol
wildfire_resources <- function(data,
                               solver="gurobi", 
                               solver_options=list(TimeLimit=600, OutputFlag=0)){

  results <- WildfireResources::exact_model(
    data, solver=solver, solver_options=solver_options)

  if(results$sol_result == "OPTIMAL"){
    return(list(st_model = results, nd_model = NULL))
  }else{
    inf_model_result <- WildfireResources::inf_exact_model(
      data, solver=solver, solver_options=solver_options)
  }
  
  return(list(st_model = results, nd_model = inf_model_result))
}