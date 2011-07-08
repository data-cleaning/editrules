# alternative implementation of errorlocalizer
# differences:
#  *) returns all solutions, in stead of one by one
#  *) slightly faster then normal implementation (but not very)
#  *) uses a recursive function in stead of nested stack, inner workings are more understandable, but original 
#    is better debugable using VERBOSE=TRUE and way more flexible.
require(editrules)

eloc <- function(E){

    if ( !isNormalized(E) ) 
        E <- normalize(E)
        
    btx <- function(x, weight=rep(1,length(x))){
        adapt <- is.na(x)   
        names(adapt) <- names(x)

        #order decreasing by weight
        o <- order(weight, decreasing=TRUE)
        totreat <- names(x)[o[!adapt]]

        # Eliminate missing variables.
        for (v in names(x)[is.na(x)]) E <- eliminateFM(E,v)
        
        wsol <- sum(weight)
       
        bt <- function(E, totreat, adapt, sol){
           w <- sum(weight[adapt])
           
           #reject
           if ( w > wsol || isObviouslyInfeasible(E) ){
              return(sol)
           }
           
           #accept
           if (length(totreat) == 0){
              wsol <<- w
              sol[[length(sol)+1]] <- list(w=w, adapt=adapt)
              return(sol)          
           }
           
           var <- totreat[1]
           
           sol <- bt( E = substValue(E, var , x[var])
                    , totreat = totreat[-1]
                    , adapt = adapt
                    , sol = sol
                    )
                    
           sol <- bt( E = eliminateFM(E, var , x[var])
                    , totreat = totreat[-1]
                    , adapt = {a <- adapt; a[var] <- TRUE;a}
                    , sol = sol
                    )
           sol
        }
        
        bt(E=E, totreat=totreat, adapt=adapt, sol=list())
    }    
    btx
}

E <- editmatrix(c(
    "p + c1 + c2 == t",
    "c1 - 0.3*t >= 0",
    "p > 0",
    "c1 > 0",
    "c2 > 0",
    "t > 0"))
    
    
cp <- errorLocalizer(E,x=c(p=755, c1=50, c2=NA,t=200))
el <- eloc(E)

sol <- cp$searchAll()
sol

sol <- el(x=c(p=755, c1=50, c2=NA,t=200))
sol

cat("backtracker:\n")
system.time(replicate(100, {
    cp <- errorLocalizer(E,x=c(p=755, c1=50, c2=NA,t=200))
    sol<-cp$searchAll()
}))

cat("hardcoded:\n")
system.time(replicate(100, {
   sol<-el(x=c(p=755, c1=50, c2=NA,t=200))}
))
