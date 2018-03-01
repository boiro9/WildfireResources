
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
#' @import lpSolveAPI
#' @import Rsymphony
#'
#' @export
#'
#' @examples
#' data <- WildfireResources::example_data()
#' WildfireResources::exact_model(data, solver="gurobi")
exact_model <- function(
  data, solver="gurobi", solver_params=list(TimeLimit=600, OutputFlag=0)){
  # ---------------------------------------------------------------------------
  # Start time
  # ---------------------------------------------------------------------------

  start.time <- Sys.time()
  
  # ---------------------------------------------------------------------------
  # Load model
  # ---------------------------------------------------------------------------
  exactmod <- model(data)
  
  results <- Solve(exactmod, solver)

  if(results$status=="OPTIMAL"){
    objects <- get_objects(exactmod)
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


    # FL[i,t]
    FL <- matrix(nrow = length(exactmod$I@elements), 
               ncol = length(exactmod$T@elements))
    row.names(FL) <- exactmod$I@elements
    colnames(FL) <- exactmod$T@elements
    for(i in exactmod$I@elements){
      for(t in exactmod$T@elements){
        FL[i, t] <- exactmod$fl[i,t]@value
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
    
    # Cost:
    cost_expr <- exactmod$Cost@expr
    len_cost_expr <- length(cost_expr@variables)
    cost <- cost_expr@variables%*%x[1:len_cost_expr] + cost_expr@independent
    
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
                    penalty=penalty,
                    Start=S,
                    Fly=FL,
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
#' @import romo
#' @return
#' @export
#'
#' @examples 
#' data <- WildfireResources::example_data()
#' m <- WildfireResources::model(data)
model <- function(data){
  m <- romo::Model()
  
  # ===========================================================================
  # Sets
  # ===========================================================================
  
  m$I  <- Set(name="I", elements = data$I)
  m$G  <- Set(name="G", elements = data$G)
  m$T  <- Set(name="T", elements = data$T)
  m$np <- length(m$T@elements)
  m$T0 <- Set(name="T0", elements = as.character(seq(0, m$np)))
  
  # Corregir para que los sets puedan definirse sobre otros conjunto: 
  #   SetExpression.
  m$G_I <- function(g){
    return(Set(name="group", elements=data$G_I[[g]]))
  }
  
  # Define empty set
  m$T_int <- function(t1, t2){
    t1 <- as.numeric(t1)
    t2 <- as.numeric(t2)
    if(t1<=t2){
      return(Set(name="T_int", 
                 elements=as.character(
                   max(1, as.numeric(t1)):min(m$np, as.numeric(t2)))
      )
      )
    }else{
      return(Set(name="T_int", 
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
  m$CFP  <- data$CFP
  m$CRP  <- data$CRP
  m$CTFP <- data$CTFP
  m$FBRP <- data$FBRP
  m$FP   <- data$FP
  m$RP   <- data$RP
  m$DFP  <- data$DFP
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
  m$s <- Var(name = "s", sets = ListSets(m$I, m$T), type = "binary")
  m$fl <- Var(name = "fl", sets = ListSets(m$I, m$T), type = "binary")
  m$r <- Var(name = "r", sets = ListSets(m$I, m$T), type = "binary")
  m$er <- Var(name = "er", sets = ListSets(m$I, m$T), type = "binary")
  m$e <- Var(name = "e", sets = ListSets(m$I, m$T), type = "binary")
  
  # Wildfire
  # ========
  m$y <- Var(name = "y", sets = ListSets(m$T0), type = "binary")
  m$mu <- Var(name = "mu", sets = ListSets(m$G, m$T), type = "continuous", lb=0)
  
  # Auxiliary
  # =========
  m$u <- AuxVar(
    name="u", 
    iterator=Iter(i %inset% m$I, t %inset% m$T), 
    expr = (
      Sum(
        iterator = Iter(t1 %inset% m$T_int(1,t)), 
        expr = m$s[i, t1]
      ) 
      - Sum(
        iterator = Iter(t2 %inset% m$T_int(1, as.numeric(t)-1)),
        expr = m$e[i,t2]
      )
    )
  )
  
  m$w <- AuxVar(
    name="w", 
    iterator=Iter(i %inset% m$I, t %inset% m$T), 
    expr = m$u[i, t] - m$r[i, t] - m$fl[i, t]
  )
  
  m$z <- AuxVar(
    name="z", 
    iterator=Iter(i %inset% m$I), 
    expr = Sum(iterator = Iter(t %inset% m$T), expr = m$e[i, t])
  )
  
  
  # =============================================================================
  # Model
  # =============================================================================
  
  # Objective function
  # ==================
  
  # Auxiliary variables
  # -------------------
  m$Cost <- AuxVar(
    name="Cost",
    expr = (
      Sum(
        iterator = Iter(i1 %inset% m$I, t1 %inset% m$T), 
        expr = m$C[i1]*m$u[i1, t1]) 
      + Sum(
        iterator = Iter(i2 %inset% m$I), 
        expr = m$P[i2]*m$z[i2]) 
      + Sum(
        iterator = Iter(t2 %inset% m$T), 
        expr = m$NVC[t2]*m$y[as.numeric(t2)-1])
    )
  )
  
  m$Penalty <- AuxVar(
    name="Penalty",
    expr = Sum(
      iterator = Iter(g %inset% m$G, t %inset% m$T),
      expr = m$M_prime*m$mu[g, t]
    ) + m$y[m$np]
  )
  
  
  # Total Cost
  # ----------
  m$Total_Cost <- Objective(
    name = "Total_Cost",
    sense = "minimize",
    expr = m$Cost + m$Penalty
  )
  
  
  # Constraints
  # ===========
  
  # Wildfire containment
  # --------------------
  m$cont_1 <- Constraint(
    name = "cont_1",
    expr = (
      Sum(
        iterator = Iter(t1 %inset% m$T), 
        expr = m$PER[t1]*m$y[as.numeric(t1)-1]) 
      <= 
        Sum(
          iterator = Iter(i %inset% m$I, t2 %inset% m$T),
          expr = m$PR[i,t2]*m$w[i,t2]
        )
    )
  )
  
  m$cont_2 <- Constraint(
    name = "cont_2",
    iterator = Iter(t %inset% m$T),
    expr = (
      Sum(
        iterator = Iter(t1 %inset% m$T_int(1, t)), 
        expr = m$PER[t1]*m$y[as.numeric(t)-1]
      ) <= Sum(
        iterator = Iter(i %inset% m$I, t2 %inset% m$T_int(1, t)),
        expr = m$PR[i,t2]*m$w[i,t2]
      ) 
      + m$M*m$y[t]
    )
  )
  
  
  # Start of activity
  # -----------------
  m$start_act_1 <- Constraint(
    name = "start_act_1",
    iterator = Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      m$A[i]*m$w[i,t] <= 
        Sum(
          iterator=Iter(t1 %inset% m$T_int(1, t)), 
          expr= m$fl[i, t1]
        )
    )
  )
  
  m$start_act_2 <- Constraint(
    name = "start_act_2",
    iterator = Iter(i %inset% m$I),
    expr = (if(m$ITW[i] == 1){
      m$s[i,1] + Sum(iterator=Iter(t %inset% m$T_int(2, m$np)), expr=(m$np+1)*m$s[i,t]) - m$np*m$z[i] <= 0
    }else{
      Sum(iterator=Iter(t %inset% m$T), expr=m$s[i,t]) - m$z[i] <= 0
    } 
    )
  )
  
  
  # End of activity
  # ---------------
  m$end_act <- Constraint(
    name = "end_act",
    iterator = Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      Sum(
        iterator=Iter(t1 %inset% m$T_int(as.numeric(t)-m$FBRP[i]+1, t)),
        expr=m$fl[i,t1]  
      ) 
      >= 
        m$FBRP[i]*m$e[i, t]
    )
  )
  
  # Breaks
  # ------
  
  # Auxiliary variables
  # ···················
  m$cr <- AuxVar(
    name="cr", 
    iterator=Iter(i %inset% m$I, t %inset% m$T), 
    expr = (
      if(m$ITW[i] == 0 && m$IOW[i] == 0){
        Sum(
          iterator = Iter(t1 %inset% m$T_int(1, t)),
          expr = ((as.numeric(t)+1-as.numeric(t1))*m$s[i, t1] 
                  - (as.numeric(t)-as.numeric(t1))*m$e[i,t1] 
                  - m$r[i,t1]
                  - m$FP[i]*m$er[i,t1]
          )
        )
      }else{
        ((as.numeric(t)+m$CFP[i]-m$CRP[i])*m$s[i,1]) + Sum(
          iterator = Iter(t1 %inset% m$T_int(2, t)),
          expr = ((as.numeric(t)+1-as.numeric(t1)+m$FP[i])*m$s[i,t1])
        ) + Sum(
          iterator = Iter(t2 %inset% m$T_int(1, t)),
          expr = -(as.numeric(t)-as.numeric(t2))*m$e[i,t2] - m$r[i,t2] - m$FP[i]*m$er[i,t2]
        )
      }
    )
  )
  
  
  # Constraints
  # ···········
  m$breaks_1_lb <- Constraint(
    name = "breaks_1_lb",
    iterator = Iter(i %inset% m$I, t %inset% m$T),
    expr = 0 <= m$cr[i,t]
  )
  
  m$breaks_1_ub <- Constraint(
    name = "breaks_1_ub",
    iterator = Iter(i %inset% m$I, t %inset% m$T),
    expr = m$cr[i,t] <= m$FP[i]
  )
  
  m$break_2 <- Constraint(
    name = "break_2",
    iterator = Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      if(as.numeric(t)-m$RP[i] >= 0){
        Sum(
          iterator = Iter(t1 %inset% m$T_int(as.numeric(t)-m$RP[i]+1,t)),
          expr = m$r[i,t1]
        ) >= m$RP[i]*m$er[i,t]
      }else{
        m$CRP[i]*m$s[i,1] + Sum(
          iterator = Iter(t1 %inset% m$T_int(1,t)), 
          expr = m$r[i,t1]
        ) >= m$RP[i]*m$er[i,t]
      } 
    )
  )
  
  m$break_3 <- Constraint(
    name = "break_3",
    iterator = Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      Sum(
        iterator=Iter(t1 %inset% m$T_int(as.numeric(t)-m$FBRP[i], as.numeric(t)+m$FBRP[i])),
        expr = m$r[i,t1]+m$fl[i,t1]
      )
      >= 
        Sum(
          iterator=Iter(t1 %inset% m$T_int(as.numeric(t)-m$FBRP[i], as.numeric(t)+m$FBRP[i])),
          expr = m$r[i,t]
        )
    )
  )
  
  # Maximum number of usage periods in a day
  # ----------------------------------------
  m$max_num_usage <- Constraint(
    name = "max_num_usage",
    iterator = Iter(i %inset% m$I),
    expr = (
      Sum(
        iterator = Iter(t %inset% m$T),
        expr = m$u[i,t]
      )
      <= m$DFP[i] - m$CTFP[i]
    )
  )
  
  
  # Maximum and minimum number of resources of a group
  # --------------------------------------------------
  m$min_group <- Constraint(
    name = "min_group",
    iterator = Iter(g %inset% m$G, t %inset% m$T),
    expr = (
      m$nMin[g,t]*m$y[as.numeric(t)-1] <= Sum(
        iterator = Iter(i %inset% m$G_I(g)),
        expr = m$w[i,t] + m$mu[g,t]
      )
    )
  )
  
  m$max_group <- Constraint(
    name = "max_group",
    iterator = Iter(g %inset% m$G, t %inset% m$T),
    expr = (
      Sum(
        iterator = Iter(i %inset% m$G_I(g)),
        expr = m$w[i,t]
      )
      <= m$nMax[g,t]*m$y[as.numeric(t)-1] 
    )
  )
  
  
  # Logical
  # -------
  m$logical_1 <- Constraint(
    name = "logical_1",
    iterator = Iter(i %inset% m$I),
    expr = (
      Sum(
        iterator = Iter(t %inset% m$T),
        expr = as.numeric(t)*m$e[i,t]
      )
      >= 
        Sum(
          iterator = Iter(t %inset% m$T),
          as.numeric(t)*m$s[i,t]
        )
    )
  )
  
  m$logical_2 <- Constraint(
    name = "logical_2",
    iterator = Iter(i %inset% m$I),
    expr = (
      Sum(
        iterator = Iter(t %inset% m$T),
        expr = m$e[i,t]
      )
      <= 1
    )
  )
  
  m$logical_3 <- Constraint(
    name = "logical_3",
    iterator = Iter(i %inset% m$I, t %inset% m$T),
    expr = (
      m$r[i,t] + m$fl[i,t] <= m$u[i,t]
    )
  )
  
  m$logical_4 <- Constraint(
    name = "logical_4",
    expr = (
      m$y[0] == 1
    )
  )
  
  return(m)
}
# ---------------------------------------------------------------------------- #
