# ------------------------------------------------------------------------------
#' Simulates and solves a battery of problems.
#'
#' @param list_num_air number of aircrafts list.
#' @param list_num_per number of periods list.
#' @param n_sims number of simulations.
#' @param seed seed number.
#' @param outfile output file. 
#'
#' @return two csv files: ´´results.csv´´ with iterations information and ´´summary.csv´´ with a summary of these iterations results.
#' @export
#'
#' @examples
#' execute_instances(c(5, 10, 15), c(5, 10, 15), 10)
#' num_air=5, num_mach=5, num_brig=5
simulations <- function(list_num_air = c(5, 10), 
                        list_num_mach= c(5, 10), 
                        list_num_brig= c(5, 10),
                        list_num_per=c(20, 30, 40),
                        n_sims=50, seed=1, outfile="results_less_cases.csv"){
  counter = 0
  res = expand.grid(aircraft=list_num_air, 
                    machines=list_num_mach,
                    brigades=list_num_brig)
  
  for(res_row in 1:nrow(res)){
    for(num_per in list_num_per){
      for(i in seq(n_sims)){
        set.seed(seed)
        seed = seed+1
        counter = counter+1
        num_res <- res[res_row,]
        cat(" ============================================================================== \n")
        cat("Resources:", paste(paste(names(num_res), num_res, sep=": "), 
                                collapse = "; "))
        cat(" Periods: ", num_per)
        cat(" Iteration: ", i, "\n")
        cat(" ------------------------------------------------------------------------------ \n")
        
        num_air <- num_res[['aircraft']]
        num_mach <- num_res[['machines']]
        num_brig <- num_res[['brigades']]
        
        instance <- WildfireResources::random_instance(
          num_air,
          num_mach,
          num_brig,
          num_per, 
          seed)
        
        data <- WildfireResources::get_data(
          instance[['resources']], 
          instance[['wildfire']],
          10)
        
        cat("Solving problem\n")
        results_e = WildfireResources::wildfire_resources(
          data, solver="gurobi", 
          solver_options=list(TimeLimit=300, OutputFlag=0))

        # sub_count = 1
        # while(results_e$sol_result != 'OPTIMAL'){
        #   seed = seed+1
        #   cat("Repeat: ", sub_count, "\n")
        #   data = random_instance(num_air, num_per)
        #   results_e = WildfireResources::exact_model(data, M_prime = 10000, solver="gurobi")
        #   sub_count = sub_count+1
        # }
        
        status <- results_e$st_model$solver_result$status
        cat("Status:\t", status, "\n")
        cat(" ------------------------------------------------------------------------------ \n")
        
        # cat("Solving heuristic problem\n")
        # results_h = asa::heuristic_model(data, M_prime = 10000, niters = 10, solver="gurobi")
        # cat("Status:\t", results_h$sol_result, "\n")
        # cat(" ------------------------------------------------------------------------------ \n\n")

        if(status=="OPTIMAL"){
          results <- results_e$st_model
          feas_time <- results_e$st_model$solver_result$runtime
          infeas_time <- 0
        }else{
          results <- results_e$nd_model
          feas_time <- results_e$st_model$solver_result$runtime
          infeas_time <- results_e$nd_model$solver_result$runtime
        }
        
        results_csv = data.frame(matrix(ncol=15, nrow=1))
        colnames(results_csv) <- c(
          "iter", "num_air", "num_mach", "num_brig", "num_per", 
          "status", 
          "select_air", "select_mach", "select_brig",
          "cost", "res_cost", "fire_cost",
          "feas_time", "infeas_time", "total_time")
        
        selected_G <- instance$resources[['G']][
          instance$resources[['Name']] %in% names(
            results$Selection)[results$Selection==1]]
        
        results_csv[1, "iter"] = i
        results_csv[1, "num_air"] = num_air
        results_csv[1, "num_mach"] = num_mach
        results_csv[1, "num_brig"] = num_brig
        results_csv[1, "num_per"] = num_per
        results_csv[1, "status"] = status
        results_csv[1, "select_air"] = sum(selected_G=="aircraft")
        results_csv[1, "select_mach"] = sum(selected_G=="machines")
        results_csv[1, "select_brig"] = sum(selected_G=="brigades")
        results_csv[1, "cost"] = results$cost
        results_csv[1, "res_cost"] = results$res_cost
        results_csv[1, "fire_cost"] = results$fire_cost
        results_csv[1, "feas_time"] = feas_time
        results_csv[1, "infeas_time"] = infeas_time
        results_csv[1, "total_time"] = feas_time + infeas_time
        
        if(counter==1){
          write.table(results_csv, file = outfile, sep = ";", 
                      row.names = FALSE, col.names = TRUE)
        }else{
          write.table(results_csv, file = outfile, sep = ";", 
                      row.names = FALSE, col.names = FALSE, append = TRUE)
        }
      }
    }
  }
}
# --------------------------------------------------------------------------- #


# random_instance -------------------------------------------------------------
random_instance <- function(num_air, num_mach, num_brig, num_per, seed){
  resources <- sim_resources(
    num_air=num_air, 
    num_mach=num_mach, 
    num_brig=num_brig, 
    seed=seed, 
    max_periods=num_per)
  
  min_res = list('aircraft'=round(runif(1, 0, num_air/2)), 
                 'machines'=round(runif(1, 0, num_mach/2)), 
                 'brigades'=round(runif(1, 0, num_brig/2)))
  
  max_res = list('aircraft'=round(runif(1, min_res[['aircraft']]+1, num_air)), 
                 'machines'=round(runif(1, min_res[['machines']]+1, num_mach)), 
                 'brigades'=round(runif(1, min_res[['brigades']]+1, num_brig)))
  
  initial_per = round(runif(1, 0.5, 1), 2)
  
  start <- round(runif(1, 1, 144-num_per-1))
  
  wildfire_day <- sim_wildfire_day(
    resources=resources[['Name']], 
    initial_per = initial_per,
    min_res=min_res,
    max_res=max_res, 
    seed=seed)
  
  max_performance <- sum(resources['BPR'])/10
  
  wildfire <- wildfire_day[start + 1:num_per,]
  wildfire[['PER']][1] <- min(max_performance, 
                              sum(wildfire_day[['PER']][1:(start+1)]))
  wildfire['Period'] <- 1:num_per
  
  
  
  return(list(resources=resources, wildfire=wildfire))
}
# --------------------------------------------------------------------------- #


# sim_wildfire_day ------------------------------------------------------------
sim_wildfire_day <- function(resources,
                             initial_per = 0.5,
                             min_res=list(
                               'aircraft'=0, 
                               'machines'=0, 
                               'brigades'=0),
                             max_res=list(
                               'aircraft'=1000, 
                               'machines'=1000, 
                               'brigades'=1000), 
                             seed=1){
  set.seed(seed)
  hours <- seq(ISOdate(2018, 3, 23, 0, 10), 
              ISOdate(2018, 3, 24, 0), by='10 min')
  nMin <- array(0, 
                dim = c(3, length(hours)), 
                dimnames = list(
                  c('aircraft', 'machines', 'brigades'), 
                  seq(1, length(hours))))
  nMax <- array(0, 
                dim = c(3, length(hours)), 
                dimnames = list(
                  c('aircraft', 'machines', 'brigades'), 
                  seq(1, length(hours))))
  
  # First period
  t <- 1
  per <- initial_per
  eff <- 1
  nvc <- initial_per*100
  nMin['aircraft', t] <- 0
  nMin['machines', t] <- floor(min_res[['machines']]/2)
  nMin['brigades', t] <- floor(min_res[['brigades']]/2)
  nMax['aircraft', t] <- 0
  nMax['machines', t] <- floor(max_res[['machines']]/2)
  nMax['brigades', t] <- floor(max_res[['brigades']]/2)

  for(t in seq_along(hours[-1])){
    t <- t+1
    hour <- unclass(as.POSIXlt(hours[t]))$hour
    if(0 <= hour && hour < 6){
      per <- c(per, round(runif(1, 0.2, 0.3), 2))
      eff <- c(eff, 0.7)
      nvc <- c(nvc, nvc[length(nvc)]+round(runif(1, 200, 300)))
      nMin['aircraft', t] <- 0
      nMin['machines', t] <- floor(1/2*min_res[['machines']])
      nMin['brigades', t] <- floor(1/2*min_res[['brigades']])
      nMax['aircraft', t] <- 0
      nMax['machines', t] <- floor(1/2*max_res[['machines']])
      nMax['brigades', t] <- floor(1/2*max_res[['brigades']])
    }else if(6 <= hour && hour < 11){
      per <- c(per, round(runif(1, 0.3, 0.4), 2))
      eff <- c(eff, 1)
      nvc <- c(nvc, nvc[length(nvc)]+round(runif(1, 300, 400)))
      nMin['aircraft', t] <- floor(min_res[['aircraft']])
      nMin['machines', t] <- floor(min_res[['machines']])
      nMin['brigades', t] <- floor(min_res[['brigades']])
      nMax['aircraft', t] <- floor(max_res[['aircraft']])
      nMax['machines', t] <- floor(max_res[['machines']])
      nMax['brigades', t] <- floor(max_res[['brigades']])
    }else if(11 <= hour && hour < 13){
      per <- c(per, round(runif(1, 0.4, 0.5), 2))
      eff <- c(eff, 0.7)
      nvc <- c(nvc, nvc[length(nvc)]+round(runif(1, 400, 500)))
      nMin['aircraft', t] <- floor(min_res[['aircraft']])
      nMin['machines', t] <- floor(min_res[['machines']])
      nMin['brigades', t] <- floor(min_res[['brigades']])
      nMax['aircraft', t] <- floor(max_res[['aircraft']])
      nMax['machines', t] <- floor(max_res[['machines']])
      nMax['brigades', t] <- floor(max_res[['brigades']])
    }else if(13 <= hour && hour < 15){
      per <- c(per, round(runif(1, 0.5, 0.6)))
      eff <- c(eff, 0.1)
      nvc <- c(nvc, nvc[length(nvc)]+round(runif(1, 500, 600)))
      nMin['aircraft', t] <- 0
      nMin['machines', t] <- 0
      nMin['brigades', t] <- 0
      nMax['aircraft', t] <- floor(max_res[['aircraft']])
      nMax['machines', t] <- floor(max_res[['machines']])
      nMax['brigades', t] <- floor(max_res[['brigades']])
    }else if(15 <= hour && hour < 17){
      per <- c(per, round(runif(1, 0.4, 0.5), 2))
      eff <- c(eff, 0.5)
      nvc <- c(nvc, nvc[length(nvc)]+round(runif(1, 400, 500)))
      nMin['aircraft', t] <- floor(min_res[['aircraft']])
      nMin['machines', t] <- floor(min_res[['machines']])
      nMin['brigades', t] <- floor(min_res[['brigades']])
      nMax['aircraft', t] <- floor(max_res[['aircraft']])
      nMax['machines', t] <- floor(max_res[['machines']])
      nMax['brigades', t] <- floor(max_res[['brigades']])
    }else if(17 <= hour && hour < 19){
      per <- c(per, round(runif(1, 0.4, 0.5), 2))
      eff <- c(eff, 0.8)
      nvc <- c(nvc, nvc[length(nvc)]+round(runif(1, 400, 500)))
      nMin['aircraft', t] <- floor(min_res[['aircraft']])
      nMin['machines', t] <- floor(min_res[['machines']])
      nMin['brigades', t] <- floor(min_res[['brigades']])
      nMax['aircraft', t] <- floor(max_res[['aircraft']])
      nMax['machines', t] <- floor(max_res[['machines']])
      nMax['brigades', t] <- floor(max_res[['brigades']])
    }else if(19 <= hour && hour < 22){
      per <- c(per, round(runif(1, 0.3, 0.4), 2))
      eff <- c(eff, 1)
      nvc <- c(nvc, nvc[length(nvc)]+round(runif(1, 300, 400)))
      nMin['aircraft', t] <- floor(min_res[['aircraft']])
      nMin['machines', t] <- floor(min_res[['machines']])
      nMin['brigades', t] <- floor(min_res[['brigades']])
      nMax['aircraft', t] <- floor(max_res[['aircraft']])
      nMax['machines', t] <- floor(max_res[['machines']])
      nMax['brigades', t] <- floor(max_res[['brigades']])
    }else{
      per <- c(per, round(runif(1, 0.2, 0.3), 2))
      eff <- c(eff, 1)
      nvc <- c(nvc, nvc[length(nvc)]+round(runif(1, 200, 300)))
      nMin['aircraft', t] <- 0
      nMin['machines', t] <- floor(1/2*min_res[['machines']])
      nMin['brigades', t] <- floor(1/2*min_res[['brigades']])
      nMax['aircraft', t] <- 0
      nMax['machines', t] <- floor(1/2*max_res[['machines']])
      nMax['brigades', t] <- floor(1/2*max_res[['brigades']])
    }
  }
  
  fire <- data.frame(
    hour=hours,
    PER=per,
    NVC=nvc,
    nMin.aircraft=nMin['aircraft', ],
    nMin.machines=nMin['machines', ],
    nMin.brigades=nMin['brigades', ],
    nMax.aircraft=nMax['aircraft', ],
    nMax.machines=nMax['machines', ],
    nMax.brigades=nMax['brigades', ]
  )
  for(r in resources){
    fire[paste('EF', r, sep=".")] <- eff
  }
  
  return(fire)
}
# --------------------------------------------------------------------------- #


# sim_resources ---------------------------------------------------------------
sim_resources <- function(num_air=5, num_mach=5, num_brig=5,
                             seed=1,
                             max_periods=10){
  set.seed(seed)
  aircraft = data.frame(Name=c("helicopter", "aircraft"), 
                        G=rep("aircraft", 2),
                        BPR=c(2.7, 3.6),
                        P=c(0, 0),
                        C=c(2880, 3120),
                        TRP=c(10, 10),
                        WP=c(120, 120),
                        RP=c(40, 40),
                        UP=c(480, 480)
                        )
  machines = data.frame(Name=c("dozer", "tractor", "machine"),
                        G=rep("machines", 3),
                        BPR=c(0.36, 0.45, 2.7),
                        P=c(0, 0, 0),
                        C=c(175, 150, 48),
                        TRP=c(0, 0, 0),
                        WP=c(480, 480, 480),
                        RP=c(0, 0, 0),
                        UP=c(480, 480, 480)
  )
  brigades = data.frame(Name=c("7brigade", "12brigade"),
                        G=rep("brigades", 2),
                        BPR=c(0.36, 0.6),
                        P=c(0, 0),
                        C=c(181, 181),
                        TRP=c(0, 0),
                        WP=c(480, 480),
                        RP=c(0, 0),
                        UP=c(480, 480)
  )
  resources <- data.frame()
  resources <- rbind(resources, 
                     aircraft[sample(1:2, num_air, replace=T),],
                     machines[sample(1:2, num_mach, replace=T),],
                     brigades[sample(1:2, num_brig, replace=T),]
                     )
  resources[['G']] <- as.character(resources[['G']])
  num_res <- num_air + num_mach + num_brig
  row.names(resources) <- 1:num_res
  
  resources['Name'] <- paste(as.character(resources[['Name']]), 1:num_res, 
                             sep="_")
    
  resources["ITW"] <- sample(c(0,1), num_res, prob=c(0.9,0.1), replace=TRUE)
  res_itw <- resources["ITW"]==1
  
  resources["IOW"] <- rep(0, num_res)
  resources["IOW"][!res_itw] <- sample(
    c(0,1), sum(!res_itw), prob=c(0.95,0.05), replace=TRUE)
  res_iow <- resources["IOW"]==1
  
  resources["A"] <- rep(0, num_res)
  resources["A"][!res_itw] <- round(
    c(
      runif(num_air, 1, max_periods),
      runif(num_mach, 3, max_periods*2),
      runif(num_brig, 2, max_periods)
      )[!res_itw]
    )*10
  
  resources["CWP"] <- rep(0, num_res)
  resources["CWP"][res_itw | res_iow] <- round(
      c(runif(num_air, 1, 16),
        runif(num_mach, 1, 48),
        runif(num_brig, 1, 48)
        )[res_itw | res_iow]
  )*10
  
  resources["CRP"] <- c(
    pmax(0, resources[["CWP"]][1:num_air]-120),
    rep(0, num_mach),
    rep(0, num_brig)
  )
  
  resources["CUP"] <- rep(0, num_res)
  resources["CUP"][res_itw | res_iow] <- c(
    sample(c(0,1,2), num_air, prob=c(0.7,0.2,0.1), replace=T)*160+
      resources[["CWP"]][1:num_air],
    resources[["CWP"]][num_air+1:num_mach],
    resources[["CWP"]][num_air+num_mach+1:num_brig]
  )[res_itw | res_iow]
  
  return(resources)
}
# --------------------------------------------------------------------------- #


#' Summarize simulation results.
#'
#' @param csv_file csv file input.
#' @param out_file csv file output
#'
#' @return csv with the summary of each instance.
#' @export
summary_table <- function(csv_file, out_file="summary.csv"){
  
  results <- read.table(csv_file, header = T, sep = ";", dec = ",")
  
  opt_e <- results["status"]=="OPTIMAL"
  optimality_e <- numeric(dim(results)[1])
  optimality_e[opt_e] <- 100
  results["optimal"] <- optimality_e
  
  results["solved"] <- numeric(dim(results)[1])
  results["solved"][!is.na(results['cost'])] <- 100
  
  summary_results = aggregate(results[, 7:17], 
                              list(num_air=results$num_air,
                                   num_mach=results$num_mach,
                                   num_brig=results$num_brig, 
                                   num_per=results$num_per), 
                              mean, na.rm=T)
  
  write.table(summary_results, file = out_file, sep = ";", dec=",",
              row.names = FALSE, col.names = TRUE)
}
# --------------------------------------------------------------------------- #