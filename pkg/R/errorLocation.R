
#' Generate new errorlocation object
#' @param adapt Logical array of same dimensions of dataset, indicating which variable have to be changed per record
#' @param status data.frame 
#' @call A \code{call} object
#' @user \code{character} username
#' @timestamp \code{character} returned by \code{date()}.
#' @keywords internal
newerrorlocation <- function(adapt, status, call=sys.call(-1), user=Sys.info()["user"], timestamp=date()){
    structure(
        list(
            adapt  = adapt,
            status = status,
            call = call,
            user = user,
            timestamp = timestamp
        ),
        class=c('errorLocation')
    ) 
}

#' Print object of class errorLocation 
#'
#' @param x object of class errorLocation
#' @param ... arguments to be passed to other methods
#' @method print errorLocation
#' @export
print.errorLocation <- function(x,...){
    cat("Object of class 'errorLocation' generated at",x$timestamp,'\n')
    cat("call :", as.character(as.expression(x$call)),'\n')
    cat("slots:",paste("$",names(x),sep=''),'\n\n')
    
    if ( nrow(x$adapt) <= 10 ){ 
        cat('Values to adapt:\n')
        print(x$adapt)
        cat('\n','Status:\n')
        print(x$status)
    } else {
        cat('Values to adapt:')
        print(x$adapt[1:10,,drop=FALSE])
        cat("...print truncated",'\n\n')
        cat('\n','Status:\n')
        print(x$status[1:10,,drop=FALSE])
        cat("...print truncated",'\n')
    }

}




#' Combine two objects
#'
#' @param x object 1
#' @param y object 2
#' @param ... extra parameters
#' 
`%+%` <- function(x,y,...){
    UseMethod("%+%")
}

#' Combine two objects of class errorLocation
#' @method `%+%` errorLocation
#' 
`%+%.errorLocation` <- function(x,y,...){
   stopifnot( class(y) == 'errorLocation' )
    
    newerrorlocation(    
        adapt=x$adapt | y$adapt,
        status=data.frame(
            weight     = x$status$weight + y$status$weight,
            degeneracy = x$status$degeneracy * y$status$degeneracy,
            user       = x$status$user + y$status$user,
            system     = x$status$system + y$status$system,
            elapsed    = x$status$elapsed + y$status$elapsed,
            maxDurationExceeded = x$status$maxDurationExceeded | y$status$maxDurationExceeded
            ),
        call = unique(c(x$call,y$call)),
        user = unique(c(x$user,y$user)),
        timestamp=date()
    )

}


`%+%.NULL` <- function(x,y,...){
    y
}


