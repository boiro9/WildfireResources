
# ---------------------------------------------------------------------------- #
# Exact model
# ---------------------------------------------------------------------------- #

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
#' sol <- WildfireResources::exact_model(data, solver="gurobi")
#' 
#' resources_file <- 'example/feasible/Resources.csv'
#' fire_file <- 'example/feasible/Fire.csv'
#' csvs <- WildfireResources::load_data(resources_file, fire_file)
#' data1 <- WildfireResources::get_data(csvs$data.resources, csvs$data.fire, 10)
#' sol <- WildfireResources::exact_model(data1)
#' sol
exact_model <- function(
  data, solver="gurobi", solver_options=list(TimeLimit=600, OutputFlag=0)){
  # ---------------------------------------------------------------------------
  # Start time
  # ---------------------------------------------------------------------------

  start.time <- Sys.time()
  
  # ---------------------------------------------------------------------------
  # Load model
  # ---------------------------------------------------------------------------
  exactmod <- model(data)
  
  results <- romo::Solve(exactmod, solver, solver_options=solver_options)

  if(results$status=="OPTIMAL"){
    objects <- romo::get_objects(exactmod)
    x <- objects$variables$value
    
    # S[i,t]
    S <- matrix(nrow = length(exactmod$I@elements), 
               ncol = length(exactmod$T@elements))
    row.names(S) <- exactmod$I@elements
    colnames(S) <- exactmod$T@elements
    for(i in exactmod$I@elements){
      for(t in exactmod$T@elements){
        S[i, t] <- exactmod$s[i,t]@value
      }
    }


    # TR[i,t]
    TR <- matrix(nrow = length(exactmod$I@elements), 
               ncol = length(exactmod$T@elements))
    row.names(TR) <- exactmod$I@elements
    colnames(TR) <- exactmod$T@elements
    for(i in exactmod$I@elements){
      for(t in exactmod$T@elements){
        TR[i, t] <- exactmod$tr[i,t]@value
      }
    }

    # R[i,t]
    R <- matrix(nrow = length(exactmod$I@elements), 
                ncol = length(exactmod$T@elements))
    row.names(R) <- exactmod$I@elements
    colnames(R) <- exactmod$T@elements
    for(i in exactmod$I@elements){
      for(t in exactmod$T@elements){
        R[i, t] <- exactmod$r[i,t]@value
      }
    }
    
    # ER[i,t]
    ER <- matrix(nrow = length(exactmod$I@elements), 
               ncol = length(exactmod$T@elements))
    row.names(ER) <- exactmod$I@elements
    colnames(ER) <- exactmod$T@elements
    for(i in exactmod$I@elements){
      for(t in exactmod$T@elements){
        ER[i, t] <- exactmod$er[i,t]@value
      }
    }

    # E[i,t]
    E <- matrix(nrow = length(exactmod$I@elements), 
                ncol = length(exactmod$T@elements))
    row.names(E) <- exactmod$I@elements
    colnames(E) <- exactmod$T@elements
    for(i in exactmod$I@elements){
      for(t in exactmod$T@elements){
        E[i, t] <- exactmod$e[i,t]@value
      }
    }

    # U[i,t]
    U <- matrix(nrow = length(exactmod$I@elements), 
               ncol = length(exactmod$T@elements))
    row.names(U) <- exactmod$I@elements
    colnames(U) <- exactmod$T@elements
    for(i in exactmod$I@elements){
      for(t in exactmod$T@elements){
        uexpr <- exactmod$u[i,t]@expr
        lenuexpr <- length(uexpr@variables)
        U[i, t] <- uexpr@variables%*%x[1:lenuexpr] + uexpr@independent
      }
    }

    # W[i,t]
    W <- matrix(nrow = length(exactmod$I@elements), 
               ncol = length(exactmod$T@elements))
    row.names(W) <- exactmod$I@elements
    colnames(W) <- exactmod$T@elements
    for(i in exactmod$I@elements){
      for(t in exactmod$T@elements){
        wexpr <- exactmod$w[i,t]@expr
        lenwexpr <- length(wexpr@variables)
        W[i, t] <- wexpr@variables%*%x[1:lenwexpr] + wexpr@independent
      }
    }

    # Z[i]
    Z <- numeric(length(exactmod$I@elements))
    names(Z) <- exactmod$I@elements
    for(i in exactmod$I@elements){
      zexpr <- exactmod$z[i]@expr
      lenzexpr <- length(zexpr@variables)
      Z[i] <- zexpr@variables%*%x[1:lenzexpr] + zexpr@independent
    }

    # MU[g,t]
    MU <- matrix(nrow = length(exactmod$G@elements), 
               ncol = length(exactmod$T@elements))
    row.names(MU) <- exactmod$G@elements
    colnames(MU) <- exactmod$T@elements
    for(g in exactmod$G@elements){
      for(t in exactmod$T@elements){
        MU[g, t] <- exactmod$mu[g,t]@value
      }
    }

    # Y:
    Y <- numeric(length(exactmod$T@elements))
    names(Y) <- exactmod$T@elements
    for(t in exactmod$T@elements){
      Y[t] <- exactmod$y[t]@value
    }
    
    # Res_Cost:
    res_cost_expr <- exactmod$Res_Cost@expr
    len_res_cost_expr <- length(res_cost_expr@variables)
    res_cost <- (res_cost_expr@variables%*%x[1:len_res_cost_expr] + 
                   res_cost_expr@independent)[1]
    
    # Fire_Cost:
    fire_cost_expr <- exactmod$Fire_Cost@expr
    len_fire_cost_expr <- length(fire_cost_expr@variables)
    fire_cost <- (fire_cost_expr@variables%*%x[1:len_fire_cost_expr] + 
                    fire_cost_expr@independent)[1]
    
    # Cost:
    cost_expr <- exactmod$Cost@expr
    len_cost_expr <- length(cost_expr@variables)
    cost <- (cost_expr@variables%*%x[1:len_cost_expr] + 
               cost_expr@independent)[1]
    
    # Penalty:
    pen_expr <- exactmod$Penalty@expr
    len_pen_expr <- length(pen_expr@variables)
    penalty <- pen_expr@variables%*%x[1:len_pen_expr] + pen_expr@independent
    
    results <- list(model="exact",
                    sol_result="OPTIMAL",
                    solver_result=results,
                    time = difftime(Sys.time(), start.time, units="secs"),
                    obj=cost+penalty,
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
                    mu=MU,
                    Y=Y)
  }else{
    results <- list(model="exact",
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


# model -----------------------------------------------------------------------
#' Model
#'
#' @param data data information.
#'
#' @return
#' @export
#'
#' @examples 
#' data <- WildfireResources::example_data()
#' m <- WildfireResources::model(data)
#' 
#' resources_file <- 'example/example1/Aeronaves1.csv'
#' fire_file <- 'example/example1/Incendio4.csv'
#' csvs <- WildfireResources::load_data(resources_file, fire_file)
#' data1 <- WildfireResources::get_data(csvs$data.resources, csvs$data.fire, 10)
#' sol <- WildfireResources::model(data1)
#' sol
model <- function(data){
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
  m$M_prime <- 100*(sum(m$C) + sum(m$NVC))
  m$M <- sum(m$PER) + sum(m$PR)
  
  
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
  m$y <- romo::Var(name = "y", sets = romo::ListSets(m$T0), type = "binary")
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
  
  m$Fire_Cost <- romo::AuxVar(
    name="Fire_Cost",
    expr = (
      romo::Sum(
        iterator = romo::Iter(t2 %inset% m$T), 
        expr = m$NVC[t2]*m$y[as.numeric(t2)-1])
    )
  )
  
  m$Cost <- romo::AuxVar(
    name="Cost",
    expr = (m$Res_Cost + m$Fire_Cost)
  )
  
  m$Penalty <- romo::AuxVar(
    name="Penalty",
    expr = romo::Sum(
      iterator = romo::Iter(g %inset% m$G, t %inset% m$T),
      expr = m$M_prime*m$mu[g, t]
    ) + m$y[m$np]
  )
  
  
  # Total Cost
  # ----------
  m$Total_Cost <- romo::Objective(
    name = "Total_Cost",
    sense = "minimize",
    expr = m$Cost + m$Penalty
  )
  
  
  # Constraints
  # ===========
  
  # Wildfire containment
  # --------------------
  m$cont_1 <- romo::Constraint(
    name = "cont_1",
    expr = (
      romo::Sum(
        iterator = romo::Iter(t1 %inset% m$T), 
        expr = m$PER[t1]*m$y[as.numeric(t1)-1]) 
      <= 
        romo::Sum(
          iterator = romo::Iter(i %inset% m$I, t2 %inset% m$T),
          expr = m$PR[i,t2]*m$w[i,t2]
        )
    )
  )
  
  m$cont_2 <- romo::Constraint(
    name = "cont_2",
    iterator = romo::Iter(t %inset% m$T),
    expr = (
      romo::Sum(
        iterator = romo::Iter(t1 %inset% m$T_int(1, t)), 
        expr = m$PER[t1]*m$y[as.numeric(t)-1]
      ) <= romo::Sum(
        iterator = romo::Iter(i %inset% m$I, t2 %inset% m$T_int(1, t)),
        expr = m$PR[i,t2]*m$w[i,t2]
      ) 
      + m$M*m$y[t]
    )
  )
  
  
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
      if(m$ITW[i] == FALSE && m$IOW[i] == FALSE){
        romo::Sum(
          iterator = romo::Iter(t1 %inset% m$T_int(1, t)),
          expr = ((as.numeric(t)+1-as.numeric(t1))*m$s[i, t1] 
                  - (as.numeric(t)-as.numeric(t1))*m$e[i,t1] 
                  - m$r[i,t1]
                  - m$WP[i]*m$er[i,t1]
          )
        )
      }else{
        ((as.numeric(t)+m$CWP[i]-m$CRP[i])*m$s[i,1]) + romo::Sum(
          iterator = romo::Iter(t1 %inset% m$T_int(2, t)),
          expr = ((as.numeric(t)+1-as.numeric(t1)+m$WP[i])*m$s[i,t1])
        ) + romo::Sum(
          iterator = romo::Iter(t2 %inset% m$T_int(1, t)),
          expr = -(as.numeric(t)-as.numeric(t2))*m$e[i,t2] - m$r[i,t2] - m$WP[i]*m$er[i,t2]
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
      if(as.numeric(t)-m$RP[i] >= 0){
        romo::Sum(
          iterator = romo::Iter(t1 %inset% m$T_int(as.numeric(t)-m$RP[i]+1,t)),
          expr = m$r[i,t1]
        ) >= m$RP[i]*m$er[i,t]
      }else{
        m$CRP[i]*m$s[i,1] + romo::Sum(
          iterator = romo::Iter(t1 %inset% m$T_int(1,t)), 
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
      m$nMin[g,t]*m$y[as.numeric(t)-1] <= romo::Sum(
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
      <= m$nMax[g,t]*m$y[as.numeric(t)-1] 
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
  
  m$logical_5 <- romo::Constraint(
    name = "logical_5",
    expr = (
      m$y[0] == 1
    )
  )
  
  return(m)
}
# ---------------------------------------------------------------------------- #
