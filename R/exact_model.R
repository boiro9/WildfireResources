
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
exact_model <- function(data, M_prime=0, solver="gurobi", solver_params=list(TimeLimit=600, OutputFlag=0)){
  #-----------------------------------------------------------------------------
  # Start time
  #-----------------------------------------------------------------------------

  start.time <- Sys.time()

  #-----------------------------------------------------------------------------
  # Load data
  #-----------------------------------------------------------------------------
  I=data$I               # Set of aircraft to select.
  G=data$G               # Set of groups.
  G_I=list()             # Members of each group.
  for(g in seq(length(data$G_I))){
    G_I[[g]] = which(I %in% data$G_I[[g]])         
  }
  TP=data$TP             # Set of time Periods.
  FP=data$FP             # Maximum number of time periods with no rests.
  RP=data$RP             # Number of time periods of rest.
  DFP=data$DFP           # Maximum number time periods working.
  FBRP=data$FBRP         # Number of time periods flying from fire to rest place and vice versa.
  A=data$A               # Number of time periods to arrive to the wildfire.
  CFP=data$CFP           # Number of time periods worked currently with no rests.
  CRP=data$CRP           # Number of time periods rested currently.
  CTFP=data$CTFP         # Number of time periods worked currently.
  C=data$C               # Cost per period of aircraft.
  P=data$P               # Cost of select aircraft.
  BPR=data$BPR           # Base yield of aircraft.
  PER=data$PER           # Perimeter of the wildfire in each time period.
  NVC=data$NVC           # Incremental cost of the wildfire in each time period.
  EF=data$EF             # Efficience of the aircrafts in each time period.
  nMax=data$nMax         # Maximum number of aircrafts working in the wildfire in each time period.
  nMin=data$nMin         # Minimum number of aircrafts working in the wildfire in each time period.
  ITW=data$ITW           # If resource is currently on this wildfire
  IOW=data$IOW           # If resource is currently on other wildfire
  
  
  #-----------------------------------------------------------------------------
  # Number of aircraft and periods
  #-----------------------------------------------------------------------------

  n <-length(I)     # number of aircraft
  m <-length(TP)    # number of periods
  ng<-length(G)     # number of groups
  
  #-----------------------------------------------------------------------------
  # Other information: is computed taken the above information
  #-----------------------------------------------------------------------------
  PR = matrix(ncol = length(TP), nrow = length(I))
  for(i in seq(n)){
    for(t in seq(m)){
      PR[i,t] = BPR[i]*EF[[i]][t]
    }
  }
  M_prime <- max(100*(sum(C)+sum(NVC)),M_prime) # Penalization term
  M <- sum(PER)+sum(PR)                         # Contention term

  #-----------------------------------------------------------------------------
  # Mathematical modelling
  #-----------------------------------------------------------------------------

  # Order of variables:
  #   s[i,t] : 0*(n*m)+t+(i-1)*m
  #  fl[i,t] : 1*(n*m)+t+(i-1)*m
  #   r[i,t] : 2*(n*m)+t+(i-1)*m
  #  er[i,t] : 3*(n*m)+t+(i-1)*m
  #   e[i,t] : 4*(n*m)+t+(i-1)*m
  #   u[i,t] : 5*(n*m)+t+(i-1)*m
  #   w[i,t] : 6*(n*m)+t+(i-1)*m
  #   z[i]   : 7*(n*m)+i
  #  mu[g,t] : 7*(n*m)+n+t+(g-1)*m
  #   y[0]   : 7*(n*m)+n+m*ng+1
  #   y[t]   : 7*(n*m)+n+m*ng+1+t
  #  cr[i,t] : 7*(n*m)+n+m*ng+1+m+t+(i-1)*m

  n_var<-8*n*m+n+ng*m+m+1                               # number of variables
  n_cons<-(n*m)+(n*m)+1+1+(n*m)+n+n+(n*m)+(n*m)+(n*m)+  #
    (n*m)+(n*m)+(n*m)+m+n+n+n+(n*m)+m+m                 # number of constraints

  # Type
  type = c(rep("B", n*m),  #   s[i,t]
           rep("B", n*m),  #  fl[i,t]
           rep("B", n*m),  #   r[i,t]
           rep("B", n*m),  #  er[i,t]
           rep("B", n*m),  #   e[i,t]
           rep("B", n*m),  #   u[i,t]
           rep("B", n*m),  #   w[i,t]
           rep("B", n),    #   z[i]
           rep("C", ng*m), #  mu[g,t]
           rep("B", 1)  ,  #   y[0]
           rep("B", m)  ,  #   y[t]
           rep("C", n*m))  #  cr[i,t]
  
  # Lower bound
  lb = c(rep(0, n*m),  #   s[i,t]
         rep(0, n*m),  #  fl[i,t]
         rep(0, n*m),  #   r[i,t]
         rep(0, n*m),  #  er[i,t]
         rep(0, n*m),  #   e[i,t]
         rep(0, n*m),  #   u[i,t]
         rep(0, n*m),  #   w[i,t]
         rep(0, n),    #   z[i]
         rep(0, ng*m), #  mu[g,t]
         rep(0, 1)  ,  #   y[0]
         rep(0, m)  ,  #   y[t]
         rep(0, n*m))  #  cr[i,t]
  
  # Upper bound
  ub = c(rep(1, n*m),             #   s[i,t]
         rep(1, n*m),             #  fl[i,t]
         rep(1, n*m),             #   r[i,t]
         rep(1, n*m),             #  er[i,t]
         rep(1, n*m),             #   e[i,t]
         rep(1, n*m),             #   u[i,t]
         rep(1, n*m),             #   w[i,t]
         rep(1, n),               #   z[i]
         rep(unlist(nMax), ng*m), #  mu[g,t]
         rep(1, 1)  ,             #   y[0]
         rep(1, m)  ,             #   y[t]
         rep(FP, each=n*m))       #  cr[i,t]
  
  # Objective function
  cost <- numeric(n_var)
  penalty <- numeric(n_var)

  # Constraints
  constr<- matrix(0, nrow = n_cons, ncol = n_var)
  sense <- rep("=", n_cons)
  rhs   <- numeric(n_cons)
  
  constr_info <- list()
  
  #============================================================================
  # var u {i in I, t in T} = + sum{t1 in T_int[1, t]}   s[i,t1] 
  #                          - sum{t2 in T_int[1, t-1]} e[i,t2];
  #----------------------------------------------------------------------------
  j_con <- 0
  for(i in seq(n)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='u', i=i, t=t)
      
      # u[i,t]
      constr[j_con, var_i("u",c(i,t),n,m,ng)] <- 1
      
      # - sum{t1 in T_int[1, t]} s[i,t1] 
      for(t1 in T_(1,t,1,m)){
        constr[j_con, var_i("s",c(i,t1),n,m,ng)] <- -1
      }
      
      # + sum{t2 in T_int[1, t-1]} e[i,t2]
      for(t2 in T_(1,t-1,1,m)){
        constr[j_con, var_i("e",c(i,t2),n,m,ng)] <- +1
      }
      
      sense[j_con] = '=' 
      rhs[j_con]   = 0  
    }
  }
  #============================================================================
  

  #============================================================================
  # var w {i in I, t in T} = u[i, t] - r[i, t] - fl[i, t];
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='w', i=i, t=t)
      
      # + w[i,t]
      constr[j_con, var_i("w", c(i,t),n,m,ng)] <-  1

      # - u[i,t]
      constr[j_con, var_i("u", c(i,t),n,m,ng)] <- -1
      
      # + r[i,t]
      constr[j_con, var_i("r", c(i,t),n,m,ng)] <-  1
      
      # + fl[i,t]
      constr[j_con, var_i("fl",c(i,t),n,m,ng)] <-  1
      
      sense[j_con] = '=' 
      rhs[j_con]   = 0 
    }
  }
  #============================================================================
  

  #============================================================================
  # minimize Total_Cost: Cost + Penalty =
  #   + sum{i in I, t in T} C[i]*u[i,t] 
  #   + sum{i in I} P[i]*z[i] 
  #   + sum{t in T} NVC[t]*y[t-1]
  #   + sum{g in G, t in T} M_prime*mu[g,t] + y[m]
  #----------------------------------------------------------------------------
  # + sum{i in I, t in T} C[i]*u[i,t]
  for(i in seq(n)){
    for(t in seq(m)){
      cost[var_i("u",c(i,t),n,m,ng)] <- C[i]  
    }
  }
  
  # + sum{i in I} P[i]*z[i]
  for(i in seq(n)){
    cost[var_i("z",c(i),n,m,ng)]   <- P[i]
  }
  
  # + sum{t in T} NVC[t]*y[t-1]
  for(t in seq(m)){
    cost[var_i("y",c(t-1),n,m,ng)] <- NVC[t]
  }
  
  # + sum{g in G, t in T} M_prime*mu[g,t]
  for(g in seq(ng)){
    for(t in seq(m)){
        penalty[var_i("mu",c(g,t),n,m,ng)] <- M_prime
    }
  }
  
  # + Y[m]
  penalty[var_i("y",c(m),n,m,ng)] <- 1
  
  obj = cost+penalty
  #============================================================================
  
  
  #=========================================================================
  # subject to cont_1:
  #   sum{t in T} PER[t]*y[t-1] <= sum{i in I, t in T} PR[i,t]*w[i,t]
  # ;
  #-------------------------------------------------------------------------
  j_con <- j_con+1
  constr_info[[j_con]] <- list(name='cont_1')
  
  # + sum{t in T} PER[t]*y[t-1]
  constr[j_con, var_i("y",c(t-1),n,m,ng)] <- + PER[t] 
  
  # - sum{i in I, t in T} PR[i,t]*w[i,t]
  constr[j_con, var_i("w",c(i,t),n,m,ng)] <- - PR[i,t] 
  
  sense[j_con] = '<=' 
  rhs[j_con]   = 0 
  #============================================================================

  #============================================================================
  # subject to cont_2 {t in T}:
  #   sum{t1 in T_int[1,t]} PER[t1]*y[t-1] 
  #   <=
  #  sum{i in I, t1 in T_int[1,t]} PR[i,t1]*w[i,t1] 
  #  + M*y[t]
  # ;
  #----------------------------------------------------------------------------
  for(t in seq(m)){
    j_con <- j_con + 1
    constr_info[[j_con]] <- list(name='cont_2', t=t)
    
    # sum{t1 in T_int[1,t]} PER[t1]*y[t-1] 
    for(t1 in T_(1,t,1,m)){
      constr[j_con, var_i("y",c(t-1),n,m,ng)] <- PER[t1]
    }
    
    # - sum{i in I, t1 in T_int[1,t]} PR[i,t1]*w[i,t1]
    for(i in seq(n)){
      for(t1 in T_(1,t,1,m)){
        constr[j_con, var_i("w",c(i,t1),n,m,ng)] <- - PR[i,t1]
      }
    }
    
    # - M*y[t]
    constr[j_con, var_i("y",c(t),n,m,ng)] <- - M
    
    sense[j_con] = '<=' 
    rhs[j_con]   = 0 
  }
  #============================================================================
  
  
  #============================================================================
  # subject to start_act_1 {i in I, t in T}:
  #   A[i]*w[i,t] <= sum{t1 in T_int[1,t]} fl[i,t1]
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='start_act_1', i=i, t=t)
      
      # A[i]*W[i,t]
      constr[j_con, var_i("w",c(i, t),n,m,ng)] <- A[i]
      
      # - sum{t1 in T_int[1,t]} fl[i,t1]
      for(t1 in T_(1,t,1,m)){
        constr[j_con, var_i("fl",c(i,t1),n,m,ng)] <- -1
      }
      
      sense[j_con] = '<=' 
      rhs[j_con]   = 0 
    }
  }
  #============================================================================

  
  #============================================================================
  # subject to start_act_2 {i in I}:
  # if ITW[i] == 1 then
  #   s[i,1] + sum{t in T_int[2,m]} (m+1)*s[i,t] - m*z[i]
  # else
  #   sum{t in T} s[i,t] - z[i]
  # <= 0
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    j_con <- j_con + 1
    constr_info[[j_con]] <- list(name='start_act_2', i=i)
    
    if(ITW[1]==1){
      # s[i,1]
      constr[j_con, var_i("s",c(i,1),n,m,ng)] <- 1
      
      # sum{t in T_int[2,m]} (m+1)*s[i,t]
      for(t in T_(2,m,1,m)){
        constr[j_con, var_i("s",c(i,t),n,m,ng)] <- (m+1)
      }
      
      # - m*z[i]
      constr[j_con, var_i("z",c(i),n,m,ng)] <- - m
      

    }else{
      # sum{t in T} s[i,t]
      for(t in seq(m)){
        constr[j_con, var_i("s",c(i,t),n,m,ng)] <- 1
      }
      
      # - z[i]
      constr[j_con, var_i("z",c(i),n,m,ng)] <- - 1
    }
    
    sense[j_con] = '<=' 
    rhs[j_con]   = 0 
  }
  #============================================================================
  
  
  #============================================================================
  # subject to end_act {i in I, t in T}:
  #   sum{t1 in T_int[max(1, min(m, t-FBRP[i]+1)),t]} fl[i,t1] >= FBRP[i]*e[i,t]
  #;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='end_act', i=i, t=t)
      
      # sum{t1 in T_int[max(1, min(m, t-FBRP[i]+1)),t]} fl[i,t1]
      for(t1 in T_(t-FBRP[i]+1,t,1,m)){
        constr[j_con, var_i("fl",c(i,t1),n,m,ng)] <- 1
      }
      
      # - FBRP[i]*e[i,t]
      constr[j_con, var_i("e",c(i,t),n,m,ng)] <- -FBRP[i]
      
      sense[j_con] = '>=' 
      rhs[j_con]   = 0 
    }
  }
  #============================================================================
  
  
  #============================================================================
  # var cr {i in I, t in T} = 
  # if (ITW[i] == 0) and (IOW[i] == 0) then
  #   + sum{t1 in T_int[1,t]} (t+1-t1)*s[i,t1]
  #   - sum{t2 in T_int[1,t]} (t-t2)*e[i,t2]
  #   - sum{t2 in T_int[1,t]} r[i,t3]
  #   - sum{t3 in T_int[1,t]} FP[i]*er[i,t4]
  # else
  #   + (t+CFP[i]-CRP[i])*s[i,1]
  #   + sum{t1 in T_int[2,t]} (t+1-t1+FP[i])*s[i,t1]
  #   - sum{t1 in T_int[1,t]} (t-t2)*e[i,t2]
  #   - sum{t2 in T_int[1,t]} r[i,t3]
  #   - sum{t3 in T_int[1,t]} FP[i]*er[i,t4]
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='cr', i=i, t=t)
      
      # cr[i,t]
      constr[j_con, var_i("cr",c(i,t),n,m,ng)] <- 1
      
      if(ITW[i]==0 | IOW[i]==0){
        # + sum{t1 in T_int[1,t]} (t+1-t1)*s[i,t1]
        for(t1 in T_(1,t,1,m)){
          constr[j_con, var_i("s",c(i,t1),n,m,ng)] <- - (t+1-t1)
        }
      }else{
        # + (t+CFP[i]-CRP[i])*s[i,1]
        constr[j_con, var_i("s",c(i,1),n,m,ng)] <- - (t+CFP[i]-CRP[i])
        
        # + sum{t1 in T_int[2,t]} (t+1-t1+FP[i])*s[i,t1]
        for(t1 in T_(2,t,1,m)){
          constr[j_con, var_i("s",c(i,t1),n,m,ng)] <- - (t+1-t1+FP[i])
        }
      }
      
      # - sum{t2 in T_int[1,t]} (t-t2)*e[i,t2]
      for(t2 in T_(1,t,1,m)){
        constr[j_con, var_i("e",c(i,t2),n,m,ng)] <- (t-t2)
      }
      
      # - sum{t3 in T_int[1,t]} r[i,t3]
      for(t3 in T_(1,t,1,m)){
        constr[j_con, var_i("r",c(i,t3),n,m,ng)] <- 1
      }
      
      # - sum{t4 in T_int[1,t]} FP[i]*er[i,t4]
      for(t4 in T_(1,t,1,m)){
        constr[j_con, var_i("er",c(i,t4),n,m,ng)] <- FP[i]
      }
      
      sense[j_con] = '=' 
      rhs[j_con]   = 0 
    }
  }
  #============================================================================
  
  
  #============================================================================
  # subject to break_2 {i in I, t in T}:
  #   if t >= RP[i] then
  #     sum{t1 in T_int[max(1, t-RP[i]+1),t]} r[i,t1] 
  #   else
  #     CRP[i]*s[i,1] + sum{t1 in T_int[1,t]} r[i,t1]
  #
  #   >= RP[i]*er[i,t]
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='break_2', i=i, t=t)
      
      # if t >= RP[i] then
      if(t >= RP[i]){
        # sum{t1 in T_int[max(1, t-RP[i]+1),t]} r[i,t1]
        for(t1 in T_(t-RP[i]+1,t,1,m)){
          constr[j_con, var_i("r",c(i,t1),n,m,ng)] <- 1
        }
      # else
      }else{
        # CRP[i]*s[i,1]
        constr[j_con, var_i("s",c(i,1),n,m,ng)] <- CRP[i]
        
        # + sum{t1 in T_int[1,t]} r[i,t1]
        for(t1 in T_(1,t,1,m)){
          constr[j_con, var_i("r",c(i,t1),n,m,ng)] <- 1
        }
      }
      
      # RP[i]*er[i,t]
      constr[j_con, var_i("er",c(i,t),n,m,ng)] <- -RP[i]
      
      sense[j_con] = '>=' 
      rhs[j_con]   = 0
    }
  }
  #============================================================================


  #============================================================================
  # subject to break_3 {i in I, t in T}:
  #   sum{t1 in T_int[max(1,t-FBRP[i]),min(m,t+FBRP[i])]} (r[i,t1]+fl[i,t1])
  #   >= sum{t1 in T_int[max(1,t-FBRP[i]),min(m,t+FBRP[i])]} r[i,t]
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='break_3', i=i, t=t)
      
      # sum{t1 in T_int[max(1,t-FBRP[i]),min(m,t+FBRP[i])]} (r[i,t1]+fl[i,t1])
      for(t1 in T_(t-FBRP[i],t+FBRP[i],1,m)){
        constr[j_con, var_i("r",c(i,t1),n,m,ng)] <- 1
        constr[j_con, var_i("r",c(i,t1),n,m,ng)] <- 1
      }
      
      # sum{t1 in T_int[max(1,t-FBRP[i]),min(m,t+FBRP[i])]} r[i,t]
      constr[j_con, var_i("r",c(i,t),n,m,ng)] <- -length(T_(t-FBRP[i],
                                                            t+FBRP[i],1,m))
      
      sense[j_con] = '>=' 
      rhs[j_con]   = 0
    }
  }
  #============================================================================
  

  #============================================================================
  # subject to max_num_usage {i in I}:
  #   sum{t in T} u[i,t] <= DFP[i] - CTFP[i]
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    j_con <- j_con + 1
    constr_info[[j_con]] <- list(name='max_num_usage', i=i)
    
    # sum{t in T} u[i,t]
    for(t in seq(m)){
      constr[j_con, var_i("u",c(i,t),n,m,ng)] <- 1
    }
    
    sense[j_con] = '<=' 
    rhs[j_con]   = DFP[i] - CTFP[i]
  }
  #============================================================================


  #============================================================================
  # subject to min_group {g in G, t in T}:
  #   nMin[g,t]*y[t-1] <= sum{i in G_I[g]} w[i,t] + mu[g,t]
  # ;
  #----------------------------------------------------------------------------
  for(g in seq(ng)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='min_group', g=g, t=t)
      
      # nMin[g,t]*y[t-1]
      constr[j_con, var_i("y",c(t-1),n,m,ng)] <- nMin[[g]][t]
      
      # sum{i in G_I[[g]]} w[i,t]
      for(i in G_I[[g]]){
        constr[j_con, var_i("w",c(i,t),n,m,ng)] <- -1
      }
      
      # + mu[g,t]
      constr[j_con, var_i("mu",c(g,t),n,m,ng)] <- -1
      
      sense[j_con] = '<=' 
      rhs[j_con]   = 0
    }
  }
  #============================================================================

  
  #============================================================================
  # subject to max_group {g in G, t in T}:
  #   sum{i in G_I[g]} w[i,t] <= nMax[g,t]*y[t-1]
  # ;
  #----------------------------------------------------------------------------
  for(g in seq(ng)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='max_group', g=g, t=t)
      
      # sum{i in G_I[[g]]} w[i,t]
      for(i in G_I[[g]]){
        constr[j_con, var_i("w",c(i,t),n,m,ng)] <- 1
      }
      
      # nMax[g,t]*y[t-1]
      constr[j_con, var_i("y",c(t-1),n,m,ng)] <- - nMax[[g]][t]
      
      sense[j_con] = '<=' 
      rhs[j_con]   = 0
    }
  }
  #============================================================================
  
  
  #============================================================================
  # subject to logical_1 {i in I}:
  #   sum{t in T} t*e[i,t] >= sum{t in T} t*s[i,t]
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    j_con <- j_con + 1
    constr_info[[j_con]] <- list(name='logical_1', i=i)
    
    # sum{t in T} t*e[i,t]
    for(t in seq(m)){
      constr[j_con, var_i("e",c(i,t),n,m,ng)] <- t
    }
    
    # sum{t in T} t*s[i,t]
    for(t in seq(m)){
      constr[j_con, var_i("s",c(i,t),n,m,ng)] <- - t
    }
    
    sense[j_con] = '>=' 
    rhs[j_con]   = 0
  }
  #============================================================================
  
  
  #============================================================================
  # subject to logical_2 {i in I}:
  #   sum{t in T} e[i,t] <= 1
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    j_con <- j_con + 1
    constr_info[[j_con]] <- list(name='logical_2', i=i)
    
    # sum{t in T} t*e[i,t]
    for(t in seq(m)){
      constr[j_con, var_i("e",c(i,t),n,m,ng)] <- 1
    }
    
    sense[j_con] = '<=' 
    rhs[j_con]   = 1
  }
  #============================================================================


  #============================================================================
  # subject to logical_3 {i in I, t in T}:
  #   r[i,t] + fl[i,t] <= u[i,t]
  # ;
  #----------------------------------------------------------------------------
  for(i in seq(n)){
    for(t in seq(m)){
      j_con <- j_con + 1
      constr_info[[j_con]] <- list(name='logical_3', i=i, t=t)
      
      # r[i,t]
      constr[j_con, var_i("r",c(i,t),n,m,ng)] <- 1
      
      # fl[i,t]
      constr[j_con, var_i("fl",c(i,t),n,m,ng)] <- 1
      
      # u[i,t]
      constr[j_con, var_i("u",c(i,t),n,m,ng)] <- - 1
      
      sense[j_con] = '<=' 
      rhs[j_con]   = 0
    }
  }
  #============================================================================
  
  
  #============================================================================
  # subject to logical_4:
  #   y[0] = 1
  # ;
  #----------------------------------------------------------------------------
  j_con <- j_con + 1
  constr_info[[j_con]] <- list(name='logical_4')
  
  # y[0]
  constr[j_con, var_i("y",c(0),n,m,ng)] <- 1
      
  sense[j_con] = '=' 
  rhs[j_con]   = 1
  #============================================================================
  
  
  


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
    #   S[i,t] : 0*(n*m)+t+(i-1)*m
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

    #  MU[t]   : 7*(n*m)+n+t
    MU = matrix(x[7*(n*m)+n+1:(ng*m)], ncol = m, byrow = T)
    colnames(MU) <- TP

    #   Y[t-1] : 7*(n*m)+n+m+n*g+t
    #   Y[m]   : 7*(n*m)+n+m+n*g+m+1
    Y = matrix(x[7*(n*m)+n+m+n*g+1:(m+1)], ncol = m+1, byrow = T)
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


T_ <- function(t1, t2, t_min=1, t_max=m){
  t1=max(t1, t_min)
  t2=min(t2, t_max)
  if(t1 <= t2){
    return(seq(t1,t2))
  }else{
    return(c())
  }
}


var_i <- function(var, index, n, m, ng){
  if(var=="s"){
    if(length(index)==2){
      return(index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="fl"){
    if(length(index)==2){
      return(n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="r"){
    if(length(index)==2){
      return(2*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="er"){
    if(length(index)==2){
      return(3*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="e"){
    if(length(index)==2){
      return(4*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="u"){
    if(length(index)==2){
      return(5*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="w"){
    if(length(index)==2){
      return(6*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="z"){
    if(length(index)==1){
      return(7*n*m+index[1])
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="mu"){
    if(length(index)==2){
      return(7*n*m+n+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="y"){
    if(length(index)==1){
      return(7*n*m+n+ng*m+index[1]+1)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="cr"){
    if(length(index)==2){
      return(7*n*m+n+ng*m+m+1+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }
}


cons <- function(con, index=c(), n, m, ng){
  j_cons = 0
  
  if(con=="u"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  }
  
  if(con=="w"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  }
  
  if(con=="z"){
    if(length(index)==1){
      j_cons = j_cons+index[1]
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n
  }
  
  if(con=="cont_1"){
    if(length(index)==0){
      j_cons = j_cons+1
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+1
  }
  
  if(con=="cont_2"){
    if(length(index)==1){
      j_cons = j_cons+index[1]
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+m
  }
  
  if(con=="start_act_1"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  }
  
  if(con=="start_act_2"){
    if(length(index)==1){
      j_cons = j_cons+index[1]
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+m
  }
  
  if(con=="end_act"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  }
  
  if(con=="cr"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  } 
  
  if(con=="break_1"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  } 
  
  if(con=="break_2"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  } 
  
  if(con=="break_3"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  }
  
  if(con=="max_num_usage"){
    if(length(index)==1){
      j_cons = j_cons+index[1]
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n
  } 
  
  if(con=="min_group"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+ng*m
  } 
  
  if(con=="max_group"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+ng*m
  }
  
  if(con=="logical_1"){
    if(length(index)==1){
      j_cons = j_cons+index[1]
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n
  } 
  
  if(con=="logical_2"){
    if(length(index)==1){
      j_cons = j_cons+index[1]
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n
  }
  
  if(con=="logical_3"){
    if(length(index)==1){
      j_cons = j_cons+index[1]
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n
  } 
  
  if(con=="logical_4"){
    if(length(index)==2){
      j_cons = j_cons+index[2]+(index[1]-1)*m
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+n*m
  } 
  
  if(con=="logical_5"){
    if(length(index)==0){
      j_cons = j_cons+1
      return(j_cons)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    j_cons = j_cons+1
  }
}

