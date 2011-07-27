require(editrules)

map <- "/home/mark/R/editrules/examples/categorical"
fil <- c("categorical.R", "editarray.R", "reduceCat.R")
dmp <- sapply(file.path(map,fil),source)

source("../../pkg/R/parse.R")

civilStatusLevels <- c("married","unmarried","widowed","divorced")
hhLevels <- c("marriage partner", "child","other")

x <- c(
    "civilStatus %in% civilStatusLevels", 
    "positionInHousehold %in% hhLevels",
    "age %in% c('<16','16-60','>16')",
    "if ( age == '<16') civilStatus != 'married'",
    "if (civilStatus != 'married') positionInHousehold != 'marriage partner'"
)
(E <- editarray(x))

datamodel(E)

F <- eliminateFM.editarray(E,"civilStatus")
contains(F,"civilStatus")
t(substValue.editarray(E,"positionInHousehold","child"))

r <- data.frame(age="<16",civilStatus="married",positionInHousehold="marriage partner")

violatedEdits(E,r)

u <- sapply(r,as.character)
bt <- errorLocalizer.editarray(E,u)
bt$searchNext(VERBOSE=FALSE)
bt$searchNext(VERBOSE=FALSE)
bt$searchNext(VERBOSE=FALSE)


a <- editarray(
        c("gender %in% c('m','f')",
          "pregnant %in% c('y','n')",
          "if(gender == 'm') pregnant == 'n'"
        ))
b <- neweditarray(
    E = array(c(FALSE,TRUE,FALSE,TRUE),dim=c(1,4)),
    ind = list(
        gender  = as.integer(c('f'=1,'m'=2)),
        pregnant= as.integer(c('n'=3,'y'=4))),
    sep=":",
    names="e1",
    levels=c("gender:f","gender:m","pregnant:n","pregnant:y")
    )




