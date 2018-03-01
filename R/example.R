#-------------------------------------------------------------------------------
# Example
#-------------------------------------------------------------------------------

#' Data example for the Selection and Allocation problem.
#'
#' @return list with the following information:
#' \tabular{ll}{
#' \code{I}        \tab Set of aircrafts to select.\cr
#' \code{Periods}  \tab Set of time Periods.\cr
#' \code{FP}       \tab Maximum number of time periods with no rests.\cr
#' \code{RP}       \tab Number of time periods of rest.\cr
#' \code{DFP}      \tab Maximum number time periods working.\cr
#' \code{FBRP}     \tab Number of time periods flying from fire to rest place and vice versa.\cr
#' \code{A}        \tab Number of time periods to arrive to the wildfire.\cr
#' \code{CFP}      \tab Number of time periods worked currently with no rests.\cr
#' \code{CRP}      \tab Number of time periods rested currently.\cr
#' \code{CTFP}     \tab Number of time periods worked currently.\cr
#' \code{C}        \tab Cost per period of the aircrafts.\cr
#' \code{P}        \tab Cost of select the aircrafts.\cr
#' \code{BPR}      \tab Base yield of the aircrafts in each time period.\cr
#' \code{SP}       \tab Perimeter of the wildfire in each time period.\cr
#' \code{NVC}      \tab Incremental cost of the wildfire in each time period.\cr
#' \code{EF}       \tab Efficience of the aircrafts in each time period.\cr
#' \code{nMax}     \tab Maximum number of aircrafts working in the wildfire in each time period.\cr
#' \code{nMin}     \tab Minimum number of aircrafts working in the wildfire in each time period.\cr
#' }
#' @export
#'
#' @examples
#' example_data()
#'
example_data <- function(){
  # Resources
  
  I <- c("BellB412_1", "BellB412_2")
  # Variable Cost
  C <- c(10, 10)
  names(C) <- I
  
  # Fix Cost
  P <- c(100, 100)
  names(P) <- I
  
  # Base Performance
  BPR <- c(4.34, 4.34)
  names(BPR) <- I
  
  # Flight time to the wildfire 
  A <- c(0, 0)
  names(A) <- I
  
  # Current flight period
  CFP <- c(11, 0)
  names(CFP) <- I
  
  # Current rest period
  CRP <- c(0, 0)
  names(CRP) <- I
  
  CTFP <- c(11, 0)
  names(CTFP) <- I
  
  FBRP <- c(1, 1)
  names(FBRP) <- I
  
  FP <- c(12, 12)
  names(FP) <- I
  
  RP <- c(4, 4)
  names(RP) <- I
  
  DFP <- c(48, 48)
  names(DFP) <- I
  
  ITW <- c(1, 0)
  names(ITW) <- I
  
  IOW <- c(0, 0)
  names(IOW) <- I
  
  TP  <- c(1, 2, 3, 4)
  EF  <- array(1, 
              dim = c(length(I), length(TP)), 
              dimnames = list(I, TP))
  PER <- c(5.6, 0.1, 0.1, 0.2)
  NVC <- c(70, 140, 200, 270)
  
  G <- c("air")
  G_I <- list("air"=c("BellB412_1", "BellB412_2"))

  nMax <- array(2,
                dim = c(1, length(TP)), 
                dimnames = list(c('air'), TP))
  nMin <- array(0, 
                dim = c(1, length(TP)), 
                dimnames = list(c('air'), TP))

  data = list(
    I    = I,
    G    = G,
    T    = TP,
    G_I  = G_I,
    C    = C,
    P    = P,
    BPR  = BPR,
    A    = A,
    CFP  = CFP,
    CRP  = CRP,
    CTFP = CTFP,
    FBRP = FBRP,
    FP   = FP,
    RP   = RP,
    DFP  = DFP,
    ITW  = ITW,
    IOW  = IOW,
    nMax = nMax,
    nMin = nMin,
    PER  = PER,
    NVC  = NVC,
    EF   = EF
    )
  
  return(data)
}
