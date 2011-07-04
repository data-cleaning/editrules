rm(list=ls())
require(editrules)
# simple edit manipulation with categorical edits.
# m, 04.05.2011


isSolvable <- function(E, x, adapt){
    A <- getArr(E)
    vars <- getVars.editarray(E)
    n <- length(vars)
    for ( i in which(!adapt) ){ 
        n <- n - A[,x[vars[i]]]
    }
    if ( any(n <=0 ) ) return(FALSE)
    ind <- getInd(E)[adapt]
    m <- apply(E,1,function(e){
        sum(sapply(ind,function(I) if ( any(!e[I]) ) 1 else 0 ))
    })
    cat(paste("n:",paste(n,collapse=","),"\n"))
    cat(paste("m:",paste(m,collapse=","),"\n"))
    return(all(m <= (n-1)))
}


violatedEdits.editarray <- function(E,x){
    if ( is.data.frame(x)) x <- sapply(x,as.character)
    apply(x,1, function(r) 
        apply(E[,r],1,all)
    )
}


# choicepoint object for error localization in categorical data.
# E: editarray, x: character vector.
# This function will become obsolete if the workhorse functions are overloaded.
# TODO: deceide at which level the overloading / specialization limit lies.
#
errorLocalizer.editarray <- function(E, x, weight=rep(1,length(x)), ...){
    adapt <- is.na(x)
    o <- order(weight, decreasing=TRUE)
    totreat <- names(x)[o[!adapt]]
    # x in direct sum representation
    y <- rep(FALSE,ncol(E))
    names(y) <- colnames(E)
    y[x] <- TRUE # assumption: category names are unique over all cat. variables.

    vars <- getVars.editarray(E)
    for (v in vars[adapt & names(x) %in% vars]) E <- eliminateFM.editarray(E,v)
    wsol <- sum(weight)
    cp <- choicepoint(
        isSolution = {
            w <- sum(weight[adapt])
            if ( w > wsol || isObviouslyInfeasible.editarray(E) ) return(FALSE)
   
            if (length(totreat) == 0){
                if (!isSolvable(E1,x,adapt)) return(FALSE) 
                wsol <<- w
                adapt <- adapt 
                rm(totreat)
                return(TRUE)
            }
        },
        choiceLeft = {
            .var <- totreat[1]
            E <- substValue.editarray(E, .var , x[.var])
            adapt[.var] <- FALSE
            totreat <- totreat[-1]
        },
        choiceRight = {
            .var <- totreat[1]
            E <- eliminateFM.editarray(E, .var)
            adapt[.var] <- TRUE
            totreat <- totreat[-1]
        },
        E = E,
        E1=E,
        x = x,
        totreat = totreat,
        adapt = adapt,
        weight = weight,
        wsol = wsol,
        ind = getInd(E)
    )
    cp
}


# example from method series
D <- data.model(
    civilStatus = c("married","unmarried","widowed","divorced"),
    age = c("under16","over15"),
    positionInHousehold = c("marriage partner","child","other")
)

# edits:
#   1. you cannot be married under 16 yrs
#   2. you cannot be a marriage partner in the household if you're not married
E <- editarray(
    forbid(D,  civilStatus = "married", age = "under16"),
    forbid(D, civilStatus = c("unmarried", "widowed","divorced"), 
        positionInHousehold = "marriage partner")
)




# derived edit, by eliminating civilStatus: you cannot be a marriage
# partner in a household whe you're under 16:
print(eliminateFM.editarray(E,"civilStatus"))
substValue.editarray(E,"civilStatus","married")

x <- c(civilStatus="married",age="under16",positionInHousehold = "marriage partner")

#cp <- errorLocalizer.editarray(E,x)
#print(cp$searchNext())
#print(cp$searchNext())

















