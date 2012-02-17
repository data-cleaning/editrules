
rfiles <- dir('pkg/R',full.names=TRUE)
for ( f in rfiles) dmp <- source(f)

DA <- letters[1:5]
DB <- letters[1:5]
F <- letters[1:2]
G <- letters[2:3]
H <- letters[4:5]

E <- editset(expression(
    x + y == z,
    if ( x > 0 ) y > 0,
    x >= 0,
    y >= 0,
    z >= 0,
    A %in% DA,
    B %in% DB,
    if ( A %in% F ) y > 0,
    if ( A %in% G ) B %in% H 
))



dnf <- function(E, name, env){
    vars <- getVars(E,type="dummy")
    if (length(vars) == 0){ 
        print("joepie!")
        assign(name,E,envir=env)
    } else {
        nm <- paste(vars[1],c("T","F"),sep="_")
        nm <- paste(name,nm,sep="_and_")
        E1 <- substValue(E,vars[1],TRUE)
        if ( isFeasible(E1$num) && isFeasible(E1$mixcat)) dnf(E1,nm[1],env)
        E1 <- substValue(E,vars[1],FALSE)
        if (isFeasible(E1$num) && !isFeasible(E1$mixcat)) dnf(E1,nm[2],env)
    }
}


disjunct <- function(E){
    e <- new.env()
    dnf(E,"E", e)
    as.list(e)
}

disjunct(E)








