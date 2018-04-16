# -----------------------------------------------------------------------------
# Scheduling
# -----------------------------------------------------------------------------


#' data.scheduling
#'
#' @param sol
#'
#' @return data
#' @export
data.scheduling <- function(sol){
  
  WRF<-(sol$Work+2*sol$Rest+3*sol$Travel)
  
  indices = sol$Selection==1
  
  columns = 1:max(which(WRF!=0,arr.ind = T)[,2])
  rows = names(indices)[indices]
  
  WRF <- matrix(WRF[indices, columns], 
                ncol=length(columns), 
                nrow=length(rows),
                dimnames=list(rows,columns))

  return(WRF)
}
# --------------------------------------------------------------------------- #


#' Scheduling plot
#'
#' @param WRF work, rest and fly matrix.
#'
#' @return scheduling plot.
#'
#' @export
#' 
#' @example 
#' WRF <- matrix(c(3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 0, 0,
#'                 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 0, 0,
#'                 0, 0, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3,
#'                 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 0, 0,
#'                 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 0, 0,
#'                 1, 1, 1, 3, 2, 2, 2, 2, 3, 1, 1, 1, 1, 3,
#'                 1, 1, 1, 3, 2, 2, 2, 2, 3, 1, 1, 1, 1, 3,
#'                 1, 1, 1, 1, 1, 1, 3, 2, 2, 2, 2, 3, 1, 3,
#'                 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 0, 0),
#'                 byrow=T, ncol=14)
#' colnames(WRF) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
#' rownames(WRF) <- c('BellB412_1', 'BellB412_2', 'BellB212_1', 'BellB212_2', 
#'                    'Ka32_1', 'Ka32_2', 'BellB407_1', 'BellB407_2' ,
#'                    'BellB407_3')
#' WildfireResources::plotscheduling(WRF)
plotscheduling <- function(WRF){
  import::from(plotly, "%>%")
  df <- data.frame(aero=numeric(), start=numeric(), end=numeric(), 
                   do=numeric())
  for(i in 1:dim(WRF)[1]){
    cont=T
    for(j in 1:dim(WRF)[2]){
      if(WRF[i,j]==1){
        df[dim(df)[1]+1,] = c(row.names(WRF)[i], j-1, j, "Work")
      }else if(WRF[i,j]==2){
        df[dim(df)[1]+1,] = c(row.names(WRF)[i], j-1, j, "Rest")
      }else if(WRF[i,j]==3){
        df[dim(df)[1]+1,] = c(row.names(WRF)[i], j-1, j, "Travel")
      }
    }
  }
  df$start<-as.numeric(df$start)
  df$end<-as.numeric(df$end)
  df$do <- factor(df$do, c('Work', 'Rest', 'Travel'))

  colors=c("firebrick1", "green3", "dodgerblue3")

  plotly::plot_ly(df, x =~start, xend=~end,
          y=~aero, yend=~aero,
          colors=colors) %>%
    plotly::add_segments(color=~do, type="scatter",
                         mode = 'lines', line = list(width = 3))%>%
    plotly::layout(xaxis = list(title = '<b>Periods</b>',
                                zeroline=F,
                                autotick=F),
           yaxis = list (title = '',
                         zeroline=F,
                         #showline=F,
                         showticklabels=T),
           margin=list(
             autoexpand=F,
             l=100,
             r=100,
             t=40)
    )
}
# --------------------------------------------------------------------------- #


# -----------------------------------------------------------------------------
# Contention
# -----------------------------------------------------------------------------

#' data.contention
#'
#' @param data data of asa problem
#' @param sol solution of asa problem
#'
#' @return data
#' @export
data.contention <- function(data, sol){
  if("Y" %in% names(sol)){
    periods <- 1:(max(which(sol$Y==1),0)+1)
  }else{
    periods <- 1:dim(sol$Work)[2]
  }
  
  contention <- numeric(length(periods))
  for(t in periods){
    contention[t] <- max(0,
      sum(data$PER[1:t])
      - sum(sol$Work[,1:t]*data$BPR*data$EF[,1:t])
      )
  }
  
  df.contention <- data.frame(periods=periods,
                              contention = contention)

  return(df.contention)
}
# --------------------------------------------------------------------------- #


#' Contention plot
#'
#' @param df
#'
#' @return contention plot
#'
#' @export
plotcontention <- function(df){
  import::from(plotly, "%>%")
  plotly::plot_ly(df, x=~periods, y=~contention, type="bar") %>%
    plotly::layout(xaxis = list(title = '<b>Periods</b>',
                        zeroline=F,
                        autotick=F),
           yaxis = list (title = '<b>No contention (proportion)</b>'
           ),
           margin=list(
             autoexpand=F,
             l=100,
             r=100,
             t=40)
    )
}
# --------------------------------------------------------------------------- #


#-------------------------------------------------------------------------------
#Number of resources per period
#-------------------------------------------------------------------------------

#' Get the number of resources per period.
#'
#' @param sol solution of WildfireResources problem.
#'
#' @return the number of resources in each period.
#' @export
data_num_resources <- function(data, sol){
  if("Y" %in% names(sol)){
    periods <- 1:(max(which(sol$Y==1),0)+1)
  }else{
    periods <- 1:dim(sol$Work)[2]
  }
  
  df.n_resources_period <- data.frame(periods=periods)
  for(g in data$G){
    data_group <- sol$Work[data$G_I[[g]], periods]
    if(!is.null(dim(data_group))){
      df.n_resources_period[g] <- slam::col_sums(data_group)
    }else{
      df.n_resources_period[g] <- data_group
    }
  }

  if(sol$model=="rest_model"){
    df.n_resources_period <- 
      df.n_resources_period[-dim(df.n_resources_period)[1],]
  }
  return(df.n_resources_period)
}
# --------------------------------------------------------------------------- #


#' Plot the number of resources per period.
#'
#' @param df
#'
#' @return resources plot
#'
#' @export
plot_num_resources <- function(df){
  import::from(plotly, "%>%")
  p <- plotly::plot_ly(df, type="bar") %>%
    plotly::layout(xaxis = list(title = '<b>Periods</b>',
                                zeroline=F,
                                autotick=F),
           yaxis = list (title = '<b>Number of resources</b>'
           ),
           margin=list(
             autoexpand=F,
             l=100,
             r=100,
             t=40)
    )
  for(g in colnames(df)[-1]){
     p <- plotly::add_trace(p, y = df[[g]], name = g)
  }
  p <- plotly::layout(p, yaxis = list(title = 'Count'), barmode = 'stack')
  p
}
# --------------------------------------------------------------------------- #


#-------------------------------------------------------------------------------
# Performance per period
#-------------------------------------------------------------------------------

#' Performance in each period.
#'
#' @param data data of asa problem.
#' @param sol solution of asa problem.
#'
#' @export
data.performance <- function(data, sol){
  if("Y" %in% names(sol)){
    periods <- 1:(max(which(sol$Y==1),0)+1)
  }else{
    periods <- 1:dim(sol$Work)[2]
  }
  performance <- numeric(length(periods))
  for(t in periods){
    performance[t] <- sum(sol$Work[,t]*data$BPR*data$EF[, t])
  }
  
  df.performance <- data.frame(periods=periods, performance = performance)

  if(sol$model=="rest_model"){
    df.performance <- df.performance[-dim(df.performance)[1],]
  }

  return(df.performance)
}
# --------------------------------------------------------------------------- #


#' Performance plot
#'
#' @param df data frame
#'
#' @export
plotperformance <- function(df){
  import::from(plotly, "%>%")
  if(requireNamespace("plotly", quietly = TRUE)){
    plotly::plot_ly(df, x=~periods, y=~performance, type="bar") %>%
      plotly::layout(xaxis = list(title = '<b>Periods</b>',
                                  zeroline=F,
                                  autotick=F),
             yaxis = list(title = '<b>Performance</b>'
             ),
             margin=list(
               autoexpand=F,
               l=100,
               r=100,
               t=40)
      )
  }
}
# --------------------------------------------------------------------------- #

