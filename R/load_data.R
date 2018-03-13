
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
  
  return(list(data.resources=resources, data.fire=fire))
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
  resources <- read.csv(resources_file, header = T, sep = ";", dec = ",", 
           stringsAsFactors=FALSE)
  resources[,'ITW'] <- as.logical(resources[,'ITW'])
  resources[,'IOW'] <- as.logical(resources[,'IOW'])
  return(resources)
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
  return(read.csv2(fire_file, header=T, sep=";", dec=",", stringsAsFactors=F))
}
# --------------------------------------------------------------------------- #


# get_data --------------------------------------------------------------------
#' Get data
#'
#' @param resources 
#' @param fire 
#' @param period_time 
#'
#' @return
#' @export
#'
#' @examples
get_data <- function(resources, fire, period_time){
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
  
  CWP_col <- which(names(resources) == 'CWP')
  CWP_min <- as.integer(resources[,CWP_col])
  names(CWP_min) <- I
  
  CRP_col <- which(names(resources) == 'CRP')
  CRP_min <- as.integer(resources[,CRP_col])
  names(CRP_min) <- I
  
  CUP_col <- which(names(resources) == 'CUP')
  CUP_min <- as.integer(resources[,CUP_col])
  names(CUP_min) <- I
  
  BPR_col <- which(names(resources) == 'BPR')
  BPR_hour <- as.double(resources[,BPR_col])
  names(BPR_hour) <- I
  
  TRP_col <- which(names(resources) == 'TRP')
  TRP_min <- as.double(resources[,TRP_col])
  names(TRP_min) <- I
  
  WP_col <- which(names(resources) == 'WP')
  WP_min <- as.double(resources[,WP_col])
  names(WP_min) <- I
  
  RP_col <- which(names(resources) == 'RP')
  RP_min <- as.double(resources[,RP_col])
  names(RP_min) <- I
  
  UP_col <- which(names(resources) == 'UP')
  UP_min <- as.double(resources[,UP_col])
  names(UP_min) <- I
  
  ITW_col <- which(names(resources) == 'ITW')
  ITW <- as.logical(resources[,ITW_col])
  names(ITW) <- I
  
  IOW_col <- which(names(resources) == 'IOW')
  IOW <- as.logical(resources[,IOW_col])
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
  TP_col <- which(names(fire) == 'Period')
  PER_col <- which(names(fire) == 'PER')
  NVC_col <- which(names(fire) == 'NVC')
  EF_cols <- grep("^EF\\..*", names(fire), perl = TRUE)
  nMin_cols <- grep("^nMin\\..*", names(fire), perl = TRUE)
  nMax_cols <- grep("^nMax\\..*", names(fire), perl = TRUE)
  
  TP <- fire[,TP_col]
  
  PER <- as.double(fire[,PER_col])
  names(PER) <- TP
  
  NVC <- as.double(fire[,NVC_col])
  names(NVC) <- TP
  
  EF <- t(subset(fire, select=EF_cols))
  row.names(EF) <- gsub("EF\\.(.*)", "\\1", row.names(EF), perl = T)
  colnames(EF) <- TP
  
  nMin <- t(subset(fire, select=nMin_cols))
  row.names(nMin) <- gsub("nMin\\.(.*)", "\\1", row.names(nMin), perl = T)
  colnames(nMin) <- TP
  
  nMax <- t(subset(fire, select=nMax_cols))
  row.names(nMax) <- gsub("nMax\\.(.*)", "\\1", row.names(nMax), perl = T)
  colnames(nMax) <- TP
  
  # From time to periods
  # --------------------
  WP <- WP_min/period_time
  RP <- RP_min/period_time
  UP <- UP_min/period_time
  TRP <- TRP_min/period_time
  
  C <- C_min*period_time
  A <- A_min/period_time
  CWP <- CWP_min/period_time
  CRP <- CRP_min/period_time
  CUP <- CUP_min/period_time
  BPR <- BPR_hour*period_time/60
  
  return(
    list(
      I=I,        # Set of aircraft to select.
      G=G,        # Group of resources.
      G_I=G_I,    # Groups with resources information.
      T=TP,       # Set of time Periods.
      WP=WP,      # Maximum number of time periods with no rests.
      RP=RP,      # Number of time periods of rest.
      UP=UP,      # Maximum number time periods working.
      TRP=TRP,    # Number of time periods flying from fire to rest
                  #   place and vice versa.
      A=A,        # Number of time periods to arrive to the wildfire.
      CWP=CWP,    # Number of time periods worked currently with no rests.  
      CRP=CRP,    # Number of time periods rested currently.
      CUP=CUP,    # Number of time periods worked currently.
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