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
  I = c("BellB412_1", "BellB412_2")
  C = c(10, 10)
  P = c(100, 100)
  BPR = c(4.34, 4.34)
  A = c(0, 0)
  CFP = c(11, 0)
  CRP = c(0, 0)
  CTFP = c(11, 0)
  FBRP = c(1, 1)
  FP = c(12, 12)
  RP = c(4, 4)
  DFP = c(48, 48)
  ITW = c(1, 0)
  IOW = c(0, 0)
  
  #resources = list(
  #  BellB412_1 = list(
  #    C    = 10,
  #    P    = 100,
  #    BPR  = 4.34,
  #    A    = 0,
  #    CFP  = 11,
  #    CRP  = 0,
  #    CTFP = 11,
  #    FBRP = 1,
  #    FP   = 12,
  #    RP   = 4,
  #    DFP  = 48,
  #    ITW  = 1,
  #    IOW  = 0
  #  ),
  #  BellB412_2 = list(
  #    C    = 10,
  #    P    = 100,
  #    BPR  = 4.34,
  #    A    = 1,
  #    CFP  = 0,
  #    CRP  = 0,
  #    CTFP = 0,
  #    FBRP = 1,
  #    FP   = 12,
  #    RP   = 4,
  #    DFP  = 48,
  #    ITW  = 0,
  #    IOW  = 0
  #  )
  #)
  
  G = c("Aircrafts")
  G_I = list(c('BellB412_1', 'BellB412_2'))
  nMax = list(c(2, 2, 2, 2))
  nMin = list(c(1, 1, 1, 1))
  
  #groups = list(
  #  Aircrafts = list(
  #    members = c('BellB412_1', 'BellB412_2'),
  #    nMax    = 2,
  #    nMin    = 1
  #  )
  #)
  
  TP  = c('1', '2', '3', '4')
  EF  = list(c(1, 1, 1, 1), c(1, 1, 1, 1))
  PER = c(5.6, 0.1, 0.1, 0.2)
  NVC = c(70, 140, 200, 270)
  
  #fire = list(
  #  '1' = list(
  #    EF  = list(
  #      BellB412_1 = 1,
  #      BellB412_2 = 1
  #    ),
  #    PER = 5.6,
  #    NVC = 70
  #  ),
  #  '2' = list(
  #    EF  = list(
  #      BellB412_1 = 1,
  #      BellB412_2 = 1
  #    ),
  #    PER = 0.1,
  #    NVC = 140
  #  ),
  #  '3' = list(
  #    EF  = list(
  #      BellB412_1 = 1,
  #      BellB412_2 = 1
  #    ),
  #    PER = 0.1,
  #    NVC = 200
  #  ),
  #  '4' = list(
  #    EF  = list(
  #      BellB412_1 = 1,
  #      BellB412_2 = 1
  #    ),
  #    PER = 0.2,
  #    NVC = 270
  #  )
  #)
  
  data = list(
    I    = I,
    G    = G,
    TP   = TP,
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
  
  #data = list(resources = resources, groups = groups, fire = fire)
  return(data)
}
