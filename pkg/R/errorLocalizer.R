#' Localize errors in a record based on Fellegi and Holt's paradigm
#' 
#' Returns a \code{\link{backtracker}} object for error localization in numerical data.
#' The returned backtracker containts methods to search depth-first to the least weighted
#' number of variables that need to be adapted so that all restrictions in E can be 
#' satisfied. (Generalized principle of Fellegi and Holt (1976)).
#'
#' The search is excecuted with a branch-and-bound algorithm, where in the left branche,
#' a variable is assumed correct and its value subsituted in \code{E}, while in the right
#' branche a variable is assumed incorrect and eliminated from \code{E} with Fourier-Motzkin
#' elimination. See De Waal (2003), chapter 8 for a consice description.
#'
#' Every call to \code{<backtracker>$searchNext()} returns one solution \code{list}, consisting of
#' \itemize{
#' \item{w: The solution weight.} 
#' \item{adapt: \code{logical} indicating whether a variable should be adapted (\code{TRUE}) or not}}
#'
#' Every subsequent call leads either to \code{NULL}, in which case either all solutions have been found,
#' or \code{maxduration} was exceeded. The property \code{<backtracker>$maxdurationExceeded} indicates if this is
#' the case. Otherwise, a new solution with a weight \code{w} not higher than the weight of the last found solution
#' is returned.
#' 
#' Alternatively \code{<backtracker>$searchBest()} will return the best solution found within \code{maxduration} seconds.
#' If multiple equivalent solutions are found, a random one is returned.
#'
#' The backtracker is prepared such that missing data in the input record \code{x} is already
#' set to adapt, and missing variables have been eliminated already.
#'
#' @title Localize errors in numerical data based on the paradigm of Fellegi and Holt.
#'
#' @param E an \code{\link{editmatrix}} or an \code{\link{editarray}}
#' @param x a named numerical vecor (if E is an editmatrix) or a named character vector (if E is an editarray). 
#'    This is the record for which errors will be localized.
#' @param ... Arguments to be passed to other methods (e.g. reliability weights)
#'
#' @return an object of class \code{\link{backtracker}}. Each execution of \code{$searchNext()} yields a solution
#'      in the form of a \code{list} (see details). Executing \code{$searchBest()} returns the lowest-weight solution.
#'      When multiple solotions with the same weight are found, \code{$searchBest()} picks one at random.
#'
#' @example ../examples/errorLocalizer.R
#' @seealso \code{\link{localizeErrors}}
#' @references 
#' I.P. Fellegi and D. Holt (1976). A systematic approach to automatic edit and imputation. 
#' Journal of the American Statistical Association 71, pp 17-25
#'
#' T. De Waal (2003) Processing of unsave and erroneous data.  PhD thesis, Erasmus Research institute 
#' of management, Erasmus university Rotterdam. 
#' http://www.cbs.nl/nl-NL/menu/methoden/onderzoek-methoden/onderzoeksrapporten/proefschriften/2008-proefschrift-de-waal.htm
#' 
#' @export
errorLocalizer <- function(E, x, ...){
    UseMethod("errorLocalizer")
}


#' Localize errors in numerical data
#'
#' @method errorLocalizer editmatrix
#' @param weight a \code{lengt(x)} positive weight vector
#' @param maxadapt maximum number of variables to adapt
#' @param maxweight maximum weight of solution, if weights are not given, this is equal to the 
#' maximum number of variables to adapt. 
#' @param maxduration maximum time (in seconds), for \code{$searchNext()}, \code{$searchAll()} and \code{$searchBest()} 
#'
#' @rdname errorLocalizer
#' @export
errorLocalizer.editmatrix <- function(
            E, 
            x, 
            weight=rep(1,length(x)), 
            maxadapt=length(x), 
            maxweight=sum(weight),
            maxduration=600,
            ...){
    stopifnot(is.numeric(weight), all(!is.na(weight)), all(weight>=0))

    if ( !isNormalized(E) ) E <- normalize(E)
    # missings must be adapted, others still have to be treated.
    adapt <- is.na(x)   
    vars <- getVars(E)

    o <- order(weight[!adapt], decreasing=TRUE)
    totreat <- names(x)[!adapt][o]

    # only treat variables in occuring in editmatrix.
    totreat <- totreat[totreat %in% vars]
    # if variables do not occur in editmatrix, do not adapt.
    adapt[!(names(adapt) %in% vars)] <- FALSE
    # Eliminate missing variables.
    for (v in names(x)[is.na(x)]) E <- eliminate.editmatrix(E,v)

    wsol <- min(sum(weight[vars %in% totreat]), maxweight)
    cp <- backtracker(
        maxduration=maxduration,
        isSolution = {

            w <- sum(weight[adapt])
            if ( w > min(wsol,maxweight) 
              || sum(adapt) > maxadapt
              || isObviouslyInfeasible.editmatrix(.E)
               ) return(FALSE)

            if ( w == wsol && isObviouslyInfeasible.editmatrix(substValue(.E,totreat,x[totreat])) ) 
                    return(FALSE)
            # TODO report status
            if (length(totreat) == 0){
                wsol <<- w
                adapt <- adapt 
                rm(totreat)
                return(TRUE)
            }
        },
        choiceLeft = {
            .var <- totreat[1]
            .E <- substValue.editmatrix(.E, .var , x[.var])
            adapt[.var] <- FALSE
            totreat <- totreat[-1]
        },
        choiceRight = {
            .var <- totreat[1]
            .E <- eliminate.editmatrix(.E, .var)
            adapt[.var] <- TRUE
            totreat <- totreat[-1]
        },
        .E = E,
        x = x,
        maxadapt=maxadapt,
        maxweight=maxweight,
        totreat = totreat,
        adapt = adapt,
        weight = weight,
        wsol = wsol
    )
    
    # add a searchBest function, currently returns last solution (which has the lowest weight)
    with(cp,{
        degeneracy <- NA
        searchBest <- function(maxduration=600, VERBOSE=FALSE){
            l <- searchAll(maxduration=maxduration,VERBOSE=VERBOSE)
            if (length(l)>0){ 
                ws <- sapply(l,function(s) s$w)
                iwmin <- which(ws==min(ws))
                degeneracy <<- length(iwmin)
                if (length(iwmin) == 1) return(l[[iwmin]])
                return(l[[sample(iwmin,1)]])
            }
        }
    })
    cp
}


#' Localize errors in categorical data
#'
#' @method errorLocalizer editarray
#' @rdname errorLocalizer
#' @export
errorLocalizer.editarray <- function(
    E, 
    x, 
    weight=rep(1,length(x)), 
    maxadapt=length(x),
    maxweight=sum(weight),
    maxduration=600,
    ...){
    stopifnot(is.numeric(weight), all(!is.na(weight)), all(weight>=0))
    adapt <- is.na(x)


    o <- order(weight[!adapt], decreasing=TRUE)
    totreat <- names(x)[!adapt][o]
#    o <- order(weight, decreasing=TRUE)
#    totreat <- names(x)[o[!adapt]]
#    weight <- weight[o[!adapt]]

    vars <- getVars.editarray(E)
    for (v in vars[adapt & names(x) %in% vars]) E <- eliminate.editarray(E,v)
    wsol <- min(sum(weight[vars %in% totreat]),maxweight)
    ind <- getInd(E)
    bt <- backtracker(
        isSolution = {
            w <- sum(weight[adapt])

            if ( w > min(wsol,maxweight) || sum(adapt) > maxadapt )  return(FALSE) 

            # check feasibility 
            I <- unique(do.call(c, c(ind[totreat],ind[adapt])))
            if ( length(totreat) > 0 &&  any(rowSums(E[,I,drop=FALSE]) == length(I)) ) return(FALSE)
            
            if ( length(totreat) == 0 ){
                # can eliminated variables be filled in?
                I <- do.call(c,ind[adapt])
                if ( length(I) > 0 && any(rowSums(E[,I,drop=FALSE]) == length(I)) ) return(FALSE)
                # Take care of corner case: check that the record is invalid
                
                if ( length(I) == 0 && nrow(E) > 0 && any(apply(E[,,drop=FALSE],1,all)) )  return(FALSE)
                # prepare output
                wsol <<- w
                adapt <- adapt 
                rm(totreat)
                return(TRUE)
            } 
        },
        choiceLeft = {
            .var <- totreat[1]
            E <- substValue.editarray(E, .var , x[.var], remove=FALSE)
            adapt[.var] <- FALSE
            totreat <- totreat[-1]
        },
        choiceRight = {
            .var <- totreat[1]
            E <- eliminate.editarray(E, .var)
            adapt[.var] <- TRUE
            totreat <- totreat[-1]
        },
        E       = E,
        x       = x,
        maxadapt= maxadapt,
        maxweight=maxweight,
        totreat = totreat,
        adapt   = adapt,
        weight  = weight,
        wsol    = wsol,
        ind     = ind
    )
    # add a searchBest function, currently returns last solution (which has the lowest weight)
    with(bt,{
        degeneracy <- NA
        searchBest <- function(maxduration=600, VERBOSE=FALSE){
            l <- searchAll(maxduration=maxduration,VERBOSE=VERBOSE)
            if (length(l)>0){ 
                ws <- sapply(l,function(s) s$w)
                iwmin <- which(ws==min(ws))
                degeneracy <<- length(iwmin)
                if (length(iwmin) == 1) return(l[[iwmin]])
                return(l[[sample(iwmin,1)]])
            }
        }
    })
    bt
}





