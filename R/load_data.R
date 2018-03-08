
#' Load Wildfire Resources data.
#'
#' @param resources_file 
#' @param fire_file 
#'
#' @return
#' @export
#'
#' @examples
load_data <- function(resources_file, fire_file){
  # Resources
  resources <- load_resources_data(resources_file)
  
  # Fire
  fire <- load_fire_data(fire_file)
  
  return(list(data.aero=aero, data.fire=fire))
}
# --------------------------------------------------------------------------- #


#' Load resources data
#'
#' @param resources_file 
#'
#' @return
#' @export
#'
#' @examples
load_resources_data  <- function(resources_file){
  return(read.csv(resources_file, header = T, sep = ";", dec = ",", 
                  stringsAsFactors =FALSE))
}
# --------------------------------------------------------------------------- #


#' Load fire data
#'
#' @param fire_file 
#'
#' @return
#' @export
#'
#' @examples
load_fire_data  <- function(fire_file){
  return(read.csv2(fire_file, header = T, sep = ";", dec = ",", 
                   stringsAsFactors =FALSE))
}
# --------------------------------------------------------------------------- #


# get_data --------------------------------------------------------------------
#' Get data
#'
#' @param resources 
#' @param fire 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
get_data <- function(resources, fire, input){
  # Law
  # ---
  PeriodTime <- input$PeriodTime
  
  # Aircraft
  # --------
  I_col <- which(names(resources) == 'Name')
  I <- resources[,I_col]
  
  C_col <- which(names(resources) == 'C')
  C_min <- as.double(resources[,C_col])
  names(C_min) <- I
  
  P_col <- which(names(resources) == 'P')
  P <- as.double(resources[,P_col])
  names(P) <- I
  
  A_col <- which(names(resources) == 'A')
  A_min <- as.integer(resources[,A_col])
  names(A_min) <- I
  
  CFP_col <- which(names(resources) == 'CFP')
  CFP_min <- as.integer(resources[,CFP_col])
  names(CFP_min) <- I
  
  CRP_col <- which(names(resources) == 'CRP')
  CRP_min <- as.integer(resources[,CRP_col])
  names(CRP_min) <- I
  
  CTFP_col <- which(names(resources) == 'CTFP')
  CTFP_min <- as.integer(resources[,CTFP_col])
  names(CTFP_min) <- I
  
  BPR_col <- which(names(resources) == 'BPR')
  BPR_hour <- as.double(resources[,BPR_col])
  names(BPR_hour) <- I
  
  FBRP_col <- which(names(resources) == 'FBRP')
  FBRP_min <- as.double(resources[,FBRP_col])
  names(FBRP_min) <- I
  
  FP_col <- which(names(resources) == 'FP')
  FP_min <- as.double(resources[,FP_col])
  names(FP_min) <- I
  
  RP_col <- which(names(resources) == 'RP')
  RP_min <- as.double(resources[,RP_col])
  names(RP_min) <- I
  
  DFP_col <- which(names(resources) == 'DFP')
  DFP_min <- as.double(resources[,DFP_col])
  names(DFP_min) <- I
  
  ITW_col <- which(names(resources) == 'ITW')
  ITW <- as.double(resources[,ITW_col])
  names(ITW) <- I
  
  IOW_col <- which(names(resources) == 'IOW')
  IOW <- as.double(resources[,IOW_col])
  names(IOW) <- I
  
  I_G_col <- which(names(resources) == 'G')
  I_G <- resources[,I_G_col]
  names(I_G) <- I
  
  G <- unique(I_G)
  
  G_I <- list()
  for(g in G){
    G_I[[g]] <- I[which(I_G == g)]
  } 
  
  # Fire
  # ----
  info_rows <- seq(2,dim(fire)[1])
  
  TP_col <- which(names(fire) == 'Period')
  PER_col <- which(names(fire) == 'PER')
  NVC_col <- which(names(fire) == 'NVC')
  EF_ini_col <- which(names(fire) == 'EF')
  nMin_ini_col <- which(names(fire) == 'nMin')
  nMax_ini_col <- which(names(fire) == 'nMax')
  
  EF_col <- seq(EF_ini_col, nMin_ini_col-1)
  nMin_col <- seq(nMin_ini_col, nMax_ini_col-1)
  nMax_col <- seq(nMax_ini_col, dim(fire)[2])
  
  TP <- fire[info_rows,TP_col]
  
  PER <- as.double(fire[info_rows,PER_col])
  names(PER) <- TP
  
  NVC <- as.double(fire[info_rows,NVC_col])
  names(NVC) <- TP
  
  EF <- matrix(0, nrow = length(I), ncol = length(TP))
  row.names(EF) <- I
  colnames(EF) <- TP
  j <- 0
  for(i in I){
    j <- j+1
    EF[i, TP] <- as.double(sub(",", ".", fire[info_rows,EF_col[j]], 
                               fixed = TRUE))
  }
  
  nMin <- matrix(0, nrow = length(G), ncol = length(TP))
  row.names(nMin) <- G
  colnames(nMin) <- TP
  nMax <- matrix(0, nrow = length(G), ncol = length(TP))
  row.names(nMax) <- G
  colnames(nMax) <- TP
  i <- 0
  for(g in G){
    i <- i+1
    nMin[g, TP] <- as.integer(fire[info_rows,nMin_col[i]])
    nMax[g, TP] <- as.integer(fire[info_rows,nMax_col[i]])
  }
  
  # From time to periods
  # --------------------
  FP <- FP_min/PeriodTime
  RP <- RP_min/PeriodTime
  DFP <- DFP_min/PeriodTime
  FBRP <- FBRP_min/PeriodTime
  
  C <- C_min*PeriodTime
  A <- A_min/PeriodTime
  CFP <- CFP_min/PeriodTime
  CRP <- CRP_min/PeriodTime
  CTFP <- CTFP_min/PeriodTime
  BPR <- BPR_hour*PeriodTime/60
  
  return(
    list(
      I=I,        # Set of aircraft to select.
      G=G,        # Group of resources.
      G_I=G_I,    # Groups with resources information.
      T=TP,       # Set of time Periods.
      FP=FP,      # Maximum number of time periods with no rests.
      RP=RP,      # Number of time periods of rest.
      DFP=DFP,    # Maximum number time periods working.
      FBRP=FBRP,  # Number of time periods flying from fire to rest
                  #   place and vice versa.
      A=A,        # Number of time periods to arrive to the wildfire.
      CFP=CFP,    # Number of time periods worked currently with no rests.  
      CRP=CRP,    # Number of time periods rested currently.
      CTFP=CTFP,  # Number of time periods worked currently.
      C=C,        # Cost per period of the aircraft.
      P=P,        # Cost of select the aircraft.
      BPR=BPR,    # Base performance of the aircraft in each time period.
      PER=PER,    # Increment of the perimeter of the wildfire in each 
                  #   time period.
      NVC=NVC,    # Incremental cost of the wildfire in each time period.
      EF=EF,      # Efficience of the aircraft in each time period.
      nMax=nMax,  # Maximum number of aircraft working in the wildfire
                  #   in each time period.
      nMin=nMin,  # Minimum number of aircraft working in the wildfire
                  #   in each time period.
      ITW=ITW,    # 1 if the resource is working in this wildfire.
      IOW=IOW     # 1 if the resource is working in other wildfire. 
    )
  )
}
# --------------------------------------------------------------------------- #