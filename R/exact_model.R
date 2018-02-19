
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
#' @import romo
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


  require_gurobi=require("gurobi")
  if(solver=="gurobi" &
     require_gurobi &
     requireNamespace("slam", quietly = TRUE)){
    
    rem_elemnts = c()#-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
    S_rec<-list()
    S_rec$A<-constr#[rem_elemnts,]
    S_rec$obj<-obj
    S_rec$sense<-sense#[rem_elemnts]
    S_rec$rhs<-rhs#[rem_elemnts]
    S_rec$vtypes<-type
    S_rec$lb<-numeric(n_var)
    S_rec$modelsense<-"min"

    sol<-gurobi(S_rec, solver_params)
    x<-sol$x
    obj_value <- sol$objval
    resolver <- sol$status
    
    if(resolver == "OPTIMAL"){
      sol_result <-"OPTIMAL"
    }else{
      sol_result <- "INFEASIBLE"
    }
    
  }else if(solver=="lpSolve" &
           requireNamespace("lpSolveAPI", quietly = TRUE)){
    S_rec<-make.lp(dim(constr)[1], dim(constr)[2])

    set.objfn(S_rec, obj)

    for(j in 1:dim(constr)[1]) set.row(S_rec, j, constr[j,])
    set.rhs(S_rec, rhs)
    set.constr.type(S_rec, sense)

    type_C <- which(type=="C")
    type_B <- which(type=="S")

    set.type(S_rec, type_C, "real")
    set.type(S_rec, type_B, "binary")

    resolver<-solve(S_rec)
    if(resolver==0){
      sol_result <-"OPTIMAL"
    }else{
      sol_result <- "INFEASIBLE"
    }
    obj_value <- get.objective(S_rec)
    x<-get.variables(S_rec)
  }else if(requireNamespace("Rsymphony", quietly = TRUE)){
    sense[sense=="="] <- "=="

    sol <- Rsymphony_solve_LP(obj, constr, sense, rhs, types = type, max = F)

    obj_value <- sol$objval
    x <- sol$solution
    resolver <- sol$status

    if(resolver==0){
      sol_result <-"OPTIMAL"
    }else{
      sol_result <- "INFEASIBLE"
    }
  }

  if(sol_result=="OPTIMAL"){
    #   S[i,t] : t+(i-1)*m
    S = matrix(x[1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(S) <- I
    colnames(S) <- TP

    #  FL[i,t] : 1*(n*m)+t+(i-1)*m
    FL = matrix(x[1*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(FL) <- I
    colnames(FL) <- TP

    # R[i,t] : 2*(n*m)+t+(i-1)*m
    R = matrix(x[2*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(R) <- I
    colnames(R) <- TP
    
    #  ER[i,t] : 3*(n*m)+t+(i-1)*m
    ER = matrix(x[3*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(ER) <- I
    colnames(ER) <- TP

    #   E[i,t] : 4*(n*m)+t+(i-1)*m
    E = matrix(x[4*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(E) <- I
    colnames(E) <- TP

    #   U[i,t] : 5*(n*m)+t+(i-1)*m
    U = matrix(x[5*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(U) <- I
    colnames(U) <- TP

    #   W[i,t] : 6*(n*m)+t+(i-1)*m
    W = matrix(x[6*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(W) <- I
    colnames(W) <- TP

    #   Z[i]   : 7*(n*m)+i
    Z = matrix(x[7*(n*m)+1:(n)],nrow = n, byrow = T)
    row.names(Z) <- I

    #  MU[g,t]   : 7*(n*m)+n+t+(g-1)*m
    MU = matrix(x[7*(n*m)+n+1:(ng*m)], ncol = m, byrow = T)
    colnames(MU) <- TP

    #   Y[t-1] : 7*(n*m)+n+ng*m+t
    #   Y[m]   : 7*(n*m)+n+ng*m+m+1
    Y = matrix(x[7*(n*m)+n+ng*m+1:(m+1)], ncol = m+1, byrow = T)
    colnames(Y) <- c("0",TP)

    results <- list(model="exact",
                    sol_result=sol_result,
                    solver_result=resolver,
                    time = difftime(Sys.time(), start.time, units="secs"),
                    obj=obj_value,
                    cost=t(cost)%*%x,
                    penalty=t(penalty)%*%x,
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
                    solver_result=resolver,
                    cost = NA,
                    time = difftime(Sys.time(), start.time, units="secs"))
  }

  return(results)
}


model <- function(data){
  m <- Model()
  
  # =============================================================================
  # Sets
  # =============================================================================
  
  m$I  <- Set(name="I", elements = data$I)
  m$G  <- Set(name="G", elements = data$G)
  m$T  <- Set(name="T", elements = data$T)
  m$np <- length(m$T@elements)
  m$T0 <- Set(name="T0", elements = as.character(seq(0, m$np)))
  
  # Corregir para que los sets puedan definirse sobre otros conjunto: SetExpression.
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
  
  # =============================================================================
  # Model information 
  # =============================================================================
  
  # Auxiliar
  # ========
  m$PR <- m$BPR * matrix(unlist(m$EF), ncol=m$np, byrow=T)
  rownames(m$PR) <- m$I@elements
  colnames(m$PR) <- m$T@elements
  m$M_prime <- 100*(sum(m$C) + sum(m$NVC))
  m$M <- sum(m$PER) + sum(m$PR)
  
  
  # =============================================================================
  # Variables
  # =============================================================================
  
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
        expr = m$PER[t1]*m$y[as.numeric(t)-1]) 
      <= 
        Sum(
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
      m$s[i,1] + Sum(iterator=Iter(t %inset% m$T), expr=(m$np+1)*m$s[i,t]) - m$np*m$z[i] <= 0
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
        iterator=Iter(t1 %inset% m$T_int(1, as.numeric(t)-m$FBRP[i]+1)),
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
          expr = (
            (as.numeric(t)+1-as.numeric(t1))*m$s[i, t1] 
             - (as.numeric(t)-as.numeric(t1))*m$e[i,t1] 
             - m$r[i,t1]
             - m$FP[i]*m$er[i,t1]
          )
        )
      }else{
        (as.numeric(t)+m$CFP[i]-m$CRP[i])*m$s[i,1] + Sum(
          iterator = Iter(t1 %inset% m$T_int(2, t)),
          expr = (
            (as.numeric(t)+1-as.numeric(t1)+m$FP[i])*m$s[i,t1]
            )
        ) + Sum(
          iterator = Iter(t2 %inset% m$T_int(1, t)),
          expr = (
            - (as.numeric(t)-as.numeric(t2))*m$e[i,t2] 
            - m$r[i,t2]
            - m$FP[i]*m$er[i,t2]
          )
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

