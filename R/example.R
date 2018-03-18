#-------------------------------------------------------------------------------
# Example
#-------------------------------------------------------------------------------

#' Data example for the Selection and Allocation problem.
#'
#' @return list with the following information:
#' \tabular{ll}{
#' \code{I}        \tab Set of aircrafts to select.\cr
#' \code{Periods}  \tab Set of time Periods.\cr
#' \code{WP}       \tab Maximum number of time periods with no rests.\cr
#' \code{RP}       \tab Number of time periods of rest.\cr
#' \code{UP}       \tab Maximum number time periods working.\cr
#' \code{TRP}      \tab Number of time periods flying from fire to rest place and vice versa.\cr
#' \code{A}        \tab Number of time periods to arrive to the wildfire.\cr
#' \code{CWP}      \tab Number of time periods worked currently with no rests.\cr
#' \code{CRP}      \tab Number of time periods rested currently.\cr
#' \code{CUP}      \tab Number of time periods worked currently.\cr
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
  
  # Current work periods
  CWP <- c(11, 0)
  names(CWP) <- I
  
  # Current rest period
  CRP <- c(0, 0)
  names(CRP) <- I
  
  CUP <- c(11, 0)
  names(CUP) <- I
  
  TRP <- c(1, 1)
  names(TRP) <- I
  
  WP <- c(12, 12)
  names(WP) <- I
  
  RP <- c(4, 4)
  names(RP) <- I
  
  UP <- c(48, 48)
  names(UP) <- I
  
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
  
  G <- c("air", "air1")
  G_I <- list("air"=c("BellB412_1"), "air1"=c("BellB412_2"))

  nMax <- array(2,
                dim = c(2, length(TP)), 
                dimnames = list(c('air', 'air1'), TP))
  nMin <- array(0, 
                dim = c(2, length(TP)), 
                dimnames = list(c('air', 'air1'), TP))

  data = list(
    I    = I,
    G    = G,
    T    = TP,
    G_I  = G_I,
    C    = C,
    P    = P,
    BPR  = BPR,
    A    = A,
    CWP  = CWP,
    CRP  = CRP,
    CUP  = CUP,
    TRP  = TRP,
    WP   = WP,
    RP   = RP,
    UP   = UP,
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
