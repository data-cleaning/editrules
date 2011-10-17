
#'
#'
#'
newerrorLocation <- function(adapt, status, call=sys.call(-1), user=Sys.info()["user"], timestamp=date()){
    structure(
        list(
            adapt  = adapt,
            status = status,
            call = call,
            user = user,
            timestamp = timestamp
        ),
        class=c('errorLocation','list')
    ) 
}


#' Combine two objects
#'
#' @param x object 1
#' @param y object 2
#' @param ... extra parameters
#' 
`%+` <- function(x,y,...){
    UseMethod("`%+%`")
}

#' Combine two objects of class errorLocation
#' @method `%+%` errorLocation
#' 
`%+%.errorLocation` <- function(x,y,copycall=TRUE,...){
    stopifnot(is.errorLocation(y))
    if(copycall){ 
        call <- x$call
    } else {
        call <- sys.call()
    }
    
    newerrorlocation(    
        adapt=x$adapt | y$adapt,
        call = call,
        user = x$user,
        timestamp=date()
    )

}



