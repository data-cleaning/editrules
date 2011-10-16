#' plot errorLocation object
#' 
#' The errorLocation object can be plotted. The plot shows the error frequency for variables and observations.
#' @method plot errorLocation
#' @param x errorLocation object
#' @param digits number of digits shown in percentage of x-axis.
#' @param ... other arguments that will be transferred to \code{barplot}
plot.errorLocation <- function(x, digits=1, ...){
  N <- nrow(x$adapt)
  varfreq <- apply(x$adapt, 2, sum) / N
  #names(varfreq) <- paste( names(varfreq), " (", round(100*varfreq/N, digits=digits), "%)", sep="")
  oldpar <- par(mfrow=c(2,1))
  barplot( sort(varfreq, decreasing=TRUE),  
         , main="Variable errors frequency"
         , xlab = "Frequency"
         , ylab= "Variable"
         , horiz = TRUE
         , las = 1
         , ...
         )
  errfreq <- tabulate(1+apply(x$adapt, 1, sum)) / N 
  names(errfreq) <- paste(seq_along(errfreq) - 1) #, " (",round(100*errfreq/N, digits=digits),"%)", sep="")
  barplot( errfreq
         , main="Observation errors frequency"
         , xlab = "Frequency"
         , ylab = "Number of errors"
         , horiz = TRUE
         , las= 1
         , ...
         )
  par(oldpar)
}

#plot(err)