
# -----------------------------------------------------------------------------
# Infeasible Exact model
# -----------------------------------------------------------------------------

#' Model to solve the exact problem of Aircraft Selection and Allocation (ASA) for the containment of a wildfire.
#'
#' @param data a list with the needed information about aircrafts and wildfire. For more information see \code{\link{example_data}} function.
#' @param M_prime penalization for the breach of the minimum aircrafts on a wildfire.
#' @param solver solver name, 'gurobi', 'Rsymphony' or 'lpSolve'.
#' @param solver_params list of gurobi options. Defaults to list(TimeLimit=600, OutputFlag = 0).
#'
#' @return information about the selection and allocation of the aircrafts.
#'
#' @export
#'
#' @examples
#' data <- WildfireResources::example_data()
#' WildfireResources::inf_exact_model(data, solver="gurobi")
#' 
#' resources_file <- 'example/example1/Aeronaves1.csv'
#' fire_file <- 'example/example1/Incendio4.csv'
#' csvs <- WildfireResources::load_data(resources_file, fire_file)
#' data1 <- WildfireResources::get_data(csvs$data.resources, csvs$data.fire, 10)
#' sol1 <- WildfireResources::inf_exact_model(data1)
#' sol1
inf_exact_model <- function(
  data, solver="gurobi", solver_options=list(TimeLimit=600, OutputFlag=0)){
  # ---------------------------------------------------------------------------
  # Start time
  # ---------------------------------------------------------------------------
  
  start.time <- Sys.time()
  
  # ---------------------------------------------------------------------------
  # Load model
  # ---------------------------------------------------------------------------
  inf_exact_mod <- scheduling_model(data)
  
  results <- romo::Solve(inf_exact_mod, solver, solver_options=solver_options)
  
  if(results$status=="OPTIMAL"){
    objects <- romo::get_objects(inf_exact_mod)
    x <- objects$variables$value
    
    # S[i,t]
    S <- matrix(nrow = length(inf_exact_mod$I@elements), 
                ncol = length(inf_exact_mod$T@elements))
    row.names(S) <- inf_exact_mod$I@elements
    colnames(S) <- inf_exact_mod$T@elements
    for(i in inf_exact_mod$I@elements){
      for(t in inf_exact_mod$T@elements){
        S[i, t] <- inf_exact_mod$s[i,t]@value
      }
    }
    
    
    # TR[i,t]
    TR <- matrix(nrow = length(inf_exact_mod$I@elements), 
                 ncol = length(inf_exact_mod$T@elements))
    row.names(TR) <- inf_exact_mod$I@elements
    colnames(TR) <- inf_exact_mod$T@elements
    for(i in inf_exact_mod$I@elements){
      for(t in inf_exact_mod$T@elements){
        TR[i, t] <- inf_exact_mod$tr[i,t]@value
      }
    }
    
    # R[i,t]
    R <- matrix(nrow = length(inf_exact_mod$I@elements), 
                ncol = length(inf_exact_mod$T@elements))
    row.names(R) <- inf_exact_mod$I@elements
    colnames(R) <- inf_exact_mod$T@elements
    for(i in inf_exact_mod$I@elements){
      for(t in inf_exact_mod$T@elements){
        R[i, t] <- inf_exact_mod$r[i,t]@value
      }
    }
    
    # ER[i,t]
    ER <- matrix(nrow = length(inf_exact_mod$I@elements), 
                 ncol = length(inf_exact_mod$T@elements))
    row.names(ER) <- inf_exact_mod$I@elements
    colnames(ER) <- inf_exact_mod$T@elements
    for(i in inf_exact_mod$I@elements){
      for(t in inf_exact_mod$T@elements){
        ER[i, t] <- inf_exact_mod$er[i,t]@value
      }
    }
    
    # E[i,t]
    E <- matrix(nrow = length(inf_exact_mod$I@elements), 
                ncol = length(inf_exact_mod$T@elements))
    row.names(E) <- inf_exact_mod$I@elements
    colnames(E) <- inf_exact_mod$T@elements
    for(i in inf_exact_mod$I@elements){
      for(t in inf_exact_mod$T@elements){
        E[i, t] <- inf_exact_mod$e[i,t]@value
      }
    }
    
    # U[i,t]
    U <- matrix(nrow = length(inf_exact_mod$I@elements), 
                ncol = length(inf_exact_mod$T@elements))
    row.names(U) <- inf_exact_mod$I@elements
    colnames(U) <- inf_exact_mod$T@elements
    for(i in inf_exact_mod$I@elements){
      for(t in inf_exact_mod$T@elements){
        uexpr <- inf_exact_mod$u[i,t]@expr
        lenuexpr <- length(uexpr@variables)
        U[i, t] <- uexpr@variables%*%x[1:lenuexpr] + uexpr@independent
      }
    }
    
    # W[i,t]
    W <- matrix(nrow = length(inf_exact_mod$I@elements), 
                ncol = length(inf_exact_mod$T@elements))
    row.names(W) <- inf_exact_mod$I@elements
    colnames(W) <- inf_exact_mod$T@elements
    for(i in inf_exact_mod$I@elements){
      for(t in inf_exact_mod$T@elements){
        wexpr <- inf_exact_mod$w[i,t]@expr
        lenwexpr <- length(wexpr@variables)
        W[i, t] <- wexpr@variables%*%x[1:lenwexpr] + wexpr@independent
      }
    }
    
    # Z[i]
    Z <- numeric(length(inf_exact_mod$I@elements))
    names(Z) <- inf_exact_mod$I@elements
    for(i in inf_exact_mod$I@elements){
      zexpr <- inf_exact_mod$z[i]@expr
      lenzexpr <- length(zexpr@variables)
      Z[i] <- zexpr@variables%*%x[1:lenzexpr] + zexpr@independent
    }
    
    # MU[g,t]
    MU <- matrix(nrow = length(inf_exact_mod$G@elements), 
                 ncol = length(inf_exact_mod$T@elements))
    row.names(MU) <- inf_exact_mod$G@elements
    colnames(MU) <- inf_exact_mod$T@elements
    for(g in inf_exact_mod$G@elements){
      for(t in inf_exact_mod$T@elements){
        MU[g, t] <- inf_exact_mod$mu[g,t]@value
      }
    }
    
    # cr:
    cr <- matrix(nrow = length(inf_exact_mod$I@elements), 
                 ncol = length(inf_exact_mod$T@elements))
    row.names(cr) <- inf_exact_mod$I@elements
    colnames(cr) <- inf_exact_mod$T@elements
    for(i in inf_exact_mod$I@elements){
      for(t in inf_exact_mod$T@elements){
        cr_expr <- inf_exact_mod$cr[i,t]@expr
        len_cr_expr <- length(cr_expr@variables)
        cr[i, t] <- cr_expr@variables%*%x[1:len_cr_expr] + cr_expr@independent
      }
    }
    
    # Res_Cost:
    res_cost_expr <- inf_exact_mod$Res_Cost@expr
    len_res_cost_expr <- length(res_cost_expr@variables)
    res_cost <- (res_cost_expr@variables%*%x[1:len_res_cost_expr] + 
                   res_cost_expr@independent)[1]
    
    # Fire_Cost:
    fire_cost <- sum(inf_exact_mod$NVC)
    
    # Cost:
    cost <- res_cost + fire_cost
    
    # Efficiency:
    eff_expr <- inf_exact_mod$Efficiency@expr
    len_eff_expr <- length(eff_expr@variables)
    efficiency <- eff_expr@variables%*%x[1:len_eff_expr] + eff_expr@independent
    
    # Penalty:
    pen_expr <- inf_exact_mod$Penalty@expr
    len_pen_expr <- length(pen_expr@variables)
    penalty <- pen_expr@variables%*%x[1:len_pen_expr] + pen_expr@independent
    
    results <- list(model="inf_exact_model",
                    sol_result="OPTIMAL",
                    solver_result=results,
                    time = difftime(Sys.time(), start.time, units="secs"),
                    obj=efficiency+penalty,
                    cost=cost,
                    res_cost=res_cost,
                    fire_cost=fire_cost,
                    penalty=penalty,
                    Start=S,
                    Travel=TR,
                    Rest=R,
                    End=E,
                    Scheduling=U,
                    Work=W,
                    Selection=Z,
                    mu=MU)
  }else{
    results <- list(model="inf_exact_model",
                    sol_result="INFEASIBLE", 
                    solver_result=results,
                    cost = NA,
                    res_cost=NA,
                    fire_cost=NA,
                    time = difftime(Sys.time(), start.time, units="secs"))
  }
  
  return(results)
}
# --------------------------------------------------------------------------- #


# scheduling_model ------------------------------------------------------------
#' Model
#'
#' @param data data information.
#'
#' @return
#' @export
#'
#' @examples 
#' data <- WildfireResources::example_data()
#' m <- WildfireResources::scheduling_model(data)
#' 
#' resources_file <- 'example/example1/Aeronaves1.csv'
#' fire_file <- 'example/example1/Incendio4.csv'
#' csvs <- WildfireResources::load_data(resources_file, fire_file)
#' data1 <- WildfireResources::get_data(csvs$data.resources, csvs$data.fire, 10)
#' m1 <- WildfireResources::scheduling_model(data1)
scheduling_model <- function(data){
  import::from(romo, "%inset%")
  m <- romo::Model()
  
  # ===========================================================================
  # Sets
  # ===========================================================================
  
  m$I  <- romo::Set(name="I", elements = data$I)
  m$G  <- romo::Set(name="G", elements = data$G)
  m$T  <- romo::Set(name="T", elements = data$T)
  m$np <- length(m$T@elements)
  m$T0 <- romo::Set(name="T0", elements = as.character(seq(0, m$np)))
  
  # Corregir para que los sets puedan definirse sobre otros conjunto: 
  #   SetExpression.
  m$G_I <- function(g){
    return(romo::Set(name="group", elements=data$G_I[[g]]))
  }
  
  # Define empty set
  m$T_int <- function(t1, t2){
    t1 <- as.numeric(t1)
    t2 <- as.numeric(t2)
    if(t1<=t2){
      return(romo::Set(name="T_int", 
                       elements=as.character(
                         max(1, as.numeric(t1)):min(m$np, as.numeric(t2)))
      )
      )
    }else{
      return(romo::Set(name="T_int", 
                       elements=c()
      )
      )
    }
    
  }
  
  # =============================================================================
  # Parameters
  # =============================================================================
  
  # Resources
  # =========
  m$C    <- data$C
  m$P    <- data$P
  m$BPR  <- data$BPR
  m$A    <- data$A
  m$CWP  <- data$CWP
  m$CRP  <- data$CRP
  m$CUP  <- data$CUP
  m$TRP  <- data$TRP
  m$WP   <- data$WP
  m$RP   <- data$RP
  m$UP   <- data$UP
  m$ITW  <- data$ITW
  m$IOW  <- data$IOW
  
  # Groups
  # ======
  m$nMax <- data$nMax
  m$nMin <- data$nMin
  
  # Wildfire
  # ========
  m$PER <- data$PER
  m$NVC <- data$NVC
  m$EF  <- data$EF
  
  # ===========================================================================
  # Model information 
  # ===========================================================================
  
  # Auxiliar
  # ========
  m$PR <- m$BPR * m$EF
  m$M_prime <- sum(m$PR)
  
  
  # ===========================================================================
  # Variables
  # ===========================================================================
  
  # Resources
  # =========
  m$s <- romo::Var(name = "s", sets = romo::ListSets(m$I, m$T), type = "binary")
  m$tr <- romo::Var(name = "tr", sets = romo::ListSets(m$I, m$T), type = "binary")
  m$r <- romo::Var(name = "r", sets = romo::ListSets(m$I, m$T), type = "binary")
  m$er <- romo::Var(name = "er", sets = romo::ListSets(m$I, m$T), type = "binary")
  m$e <- romo::Var(name = "e", sets = romo::ListSets(m$I, m$T), type = "binary")
  
  # Wildfire
  # ========
  m$mu <- romo::Var(name = "mu", sets = romo::ListSets(m$G, m$T), type = "continuous", lb=0)
  
  # Auxiliary
  # =========
  m$u <- romo::AuxVar(
    name="u", 
    iterator=romo::Iter(i %inset% m$I, t %inset% m$T), 
    expr = (
      romo::Sum(
        iterator = romo::Iter(t1 %inset% m$T_int(1,t)), 
        expr = m$s[i, t1]
      ) 
      - romo::Sum(
        iterator = romo::Iter(t2 %inset% m$T_int(1, as.numeric(t)-1)),
        expr = m$e[i,t2]
      )
    )
  )
  
  m$w <- romo::AuxVar(
    name="w", 
    iterator=romo::Iter(i %inset% m$I, t %inset% m$T), 
    expr = m$u[i, t] - m$r[i, t] - m$tr[i, t]
  )
  
  m$z <- romo::AuxVar(
    name="z", 
    iterator=romo::Iter(i %inset% m$I), 
    expr = romo::Sum(iterator = romo::Iter(t %inset% m$T), expr = m$e[i, t])
  )
  
  
  # =============================================================================
  # Model
  # =============================================================================
  
  # Objective function
  # ==================
  
  # Auxiliary variables
  # -------------------
  m$Res_Cost <- romo::AuxVar(
    name="Res_Cost",
    expr = (
      romo::Sum(
        iterator = romo::Iter(i1 %inset% m$I, t1 %inset% m$T), 
        expr = m$C[i1]*m$u[i1, t1]) 
      + romo::Sum(
        iterator = romo::Iter(i2 %inset% m$I), 
        expr = m$P[i2]*m$z[i2])
    )
  )
  
  m$Efficiency <- romo::AuxVar(
    name="Efficiency",
    expr = romo::Sum(
      iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
      expr = m$PR[i,t]*m$w[i,t]
    )
  )
  
  m$Penalty <- romo::AuxVar(
    name="Penalty",
    expr = romo::Sum(
      iterator = romo::Iter(g %inset% m$G, t %inset% m$T),
      expr = m$M_prime*m$mu[g, t]
    )
  )
  
  
  # Total Cost
  # ----------
  m$Total_Cost <- romo::Objective(
    name = "Total_Efficiency",
    sense = "minimize",
    expr = - m$Efficiency + m$Penalty
  )
  
  
  # Constraints
  # ===========
  
  # Start of activity
  # -----------------
  m$start_act_1 <- romo::Constraint(
    name = "start_act_1",
    iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      m$A[i]*m$w[i,t] <= 
        romo::Sum(
          iterator=romo::Iter(t1 %inset% m$T_int(1, t)), 
          expr= m$tr[i, t1]
        )
    )
  )
  
  m$start_act_2 <- romo::Constraint(
    name = "start_act_2",
    iterator = romo::Iter(i %inset% m$I),
    expr = (if(m$ITW[i] == TRUE){
      m$s[i,1] + romo::Sum(iterator=romo::Iter(t %inset% m$T_int(2, m$np)), expr=(m$np+1)*m$s[i,t]) - m$np*m$z[i] <= 0
    }else{
      romo::Sum(iterator=romo::Iter(t %inset% m$T), expr=m$s[i,t]) - m$z[i] <= 0
    } 
    )
  )
  
  
  # End of activity
  # ---------------
  m$end_act <- romo::Constraint(
    name = "end_act",
    iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      romo::Sum(
        iterator=romo::Iter(t1 %inset% m$T_int(as.numeric(t)-m$TRP[i]+1, t)),
        expr=m$tr[i,t1]  
      ) 
      >= 
        m$TRP[i]*m$e[i, t]
    )
  )
  
  # Breaks
  # ------
  
  # Auxiliary variables
  # ···················
  m$cr <- romo::AuxVar(
    name="cr", 
    iterator=romo::Iter(i %inset% m$I, t %inset% m$T), 
    expr = (
      if(m$ITW[i] == T | m$IOW[i] == T){
        
        ((as.numeric(t)+m$CWP[i]-m$CRP[i])*m$s[i,1]) + romo::Sum(
          iterator = romo::Iter(t1 %inset% m$T_int(2, t)),
          expr = ((as.numeric(t)+1-as.numeric(t1)+m$WP[i])*m$s[i,t1])
        ) + romo::Sum(
          iterator = romo::Iter(t2 %inset% m$T_int(1, t)),
          expr = -(as.numeric(t)-as.numeric(t2))*m$e[i,t2] - m$r[i,t2] - m$WP[i]*m$er[i,t2]
        )
        
      }else{

        romo::Sum(
          iterator = romo::Iter(t1 %inset% m$T_int(1, t)),
          expr = ((as.numeric(t)+1-as.numeric(t1))*m$s[i, t1] 
                  - (as.numeric(t)-as.numeric(t1))*m$e[i,t1] 
                  - m$r[i,t1]
                  - m$WP[i]*m$er[i,t1]
          )
        )
        
      }
    )
  )
  
  
  # Constraints
  # ···········
  m$breaks_1_lb <- romo::Constraint(
    name = "breaks_1_lb",
    iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
    expr = 0 <= m$cr[i,t]
  )
  
  m$breaks_1_ub <- romo::Constraint(
    name = "breaks_1_ub",
    iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
    expr = m$cr[i,t] <= m$WP[i]
  )
  
  m$break_2 <- romo::Constraint(
    name = "break_2",
    iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      if(as.numeric(t)-m$RP[i] < 0){

        m$CRP[i]*m$s[i,1] + romo::Sum(
          iterator = romo::Iter(t1 %inset% m$T_int(1,t)), 
          expr = m$r[i,t1]
        ) >= m$er[i,t]*min(as.numeric(t), m$RP[i])
        
      }else{
        
        romo::Sum(
          iterator = romo::Iter(t1 %inset% m$T_int(as.numeric(t)-m$RP[i]+1,t)),
          expr = m$r[i,t1]
        ) >= m$RP[i]*m$er[i,t]
        
      } 
    )
  )
  
  m$break_3 <- romo::Constraint(
    name = "break_3",
    iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      romo::Sum(
        iterator = romo::Iter(t1 %inset% m$T_int(as.numeric(t), as.numeric(t)+m$RP[i]-1)),
        expr = m$er[i,t1]
      ) >= m$r[i,t] 
    )
  )
  
  m$break_4 <- romo::Constraint(
    name = "break_4",
    iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      romo::Sum(
        iterator=romo::Iter(t1 %inset% m$T_int(as.numeric(t)-m$TRP[i], as.numeric(t)+m$TRP[i])),
        expr = m$r[i,t1]+m$tr[i,t1]
      )
      >= 
        romo::Sum(
          iterator=romo::Iter(t1 %inset% m$T_int(as.numeric(t)-m$TRP[i], as.numeric(t)+m$TRP[i])),
          expr = m$r[i,t]
        )
    )
  )
  
  # Maximum number of usage periods in a day
  # ----------------------------------------
  m$max_num_usage <- romo::Constraint(
    name = "max_num_usage",
    iterator = romo::Iter(i %inset% m$I),
    expr = (
      romo::Sum(
        iterator = romo::Iter(t %inset% m$T),
        expr = m$u[i,t]
      )
      <= m$UP[i] - m$CUP[i]
    )
  )
  
  
  # Maximum and minimum number of resources of a group
  # --------------------------------------------------
  m$min_group <- romo::Constraint(
    name = "min_group",
    iterator = romo::Iter(g %inset% m$G, t %inset% m$T),
    expr = (
      m$nMin[g,t] <= romo::Sum(
        iterator = romo::Iter(i %inset% m$G_I(g)),
        expr = m$w[i,t]
      ) + m$mu[g,t]
    )
  )
  
  m$max_group <- romo::Constraint(
    name = "max_group",
    iterator = romo::Iter(g %inset% m$G, t %inset% m$T),
    expr = (
      romo::Sum(
        iterator = romo::Iter(i %inset% m$G_I(g)),
        expr = m$w[i,t]
      )
      <= m$nMax[g,t]
    )
  )
  
  
  # Logical
  # -------
  m$logical_1 <- romo::Constraint(
    name = "logical_1",
    iterator = romo::Iter(i %inset% m$I),
    expr = (
      romo::Sum(
        iterator = romo::Iter(t %inset% m$T),
        expr = as.numeric(t)*m$e[i,t]
      )
      >= 
        romo::Sum(
          iterator = romo::Iter(t %inset% m$T),
          as.numeric(t)*m$s[i,t]
        )
    )
  )
  
  m$logical_2 <- romo::Constraint(
    name = "logical_2",
    iterator = romo::Iter(i %inset% m$I),
    expr = (
      romo::Sum(
        iterator = romo::Iter(t %inset% m$T),
        expr = m$e[i,t]
      )
      <= 1
    )
  )
  
  m$logical_3 <- romo::Constraint(
    name = "logical_3",
    iterator = romo::Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      m$r[i,t] + m$tr[i,t] <= m$u[i,t]
    )
  )
  
  m$logical_4 <- romo::Constraint(
    name = "logical_4",
    iterator = romo::Iter(i %inset% m$I),
    expr = (
      romo::Sum(
        iterator = romo::Iter(t %inset% m$T),
        expr = m$w[i,t]
      ) >= m$z[i]
    )
  )
  
  return(m)
}
# ---------------------------------------------------------------------------- #
