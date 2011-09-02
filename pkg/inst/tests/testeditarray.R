

library(testthat)

context("Editarray")


test_that("2x2 categorical datamodel",{
    dm <- c(
        "g %in% c('m','f')",
        "p %in% c('y','n')")
     is_equivalent_to(getArr(editarray(c(dm,"if( p == 'y' )  g != 'm'"))),array(c(F,T,F,T),dim=c(1,4)))
     is_equivalent_to(getArr(editarray(c(dm,"if( p %in% c('y') )  g != 'm'"))),array(c(F,T,F,T),dim=c(1,4)))
     is_equivalent_to(getArr(editarray(c(dm,"if( p %in% c('y') )  g == 'f'"))),array(c(F,T,F,T),dim=c(1,4)))
})


test_that("2x{TRUE,FALSE} datamodel",{
    dm <- c(
        "g %in% c('m','f')",
        "p %in% c(FALSE,TRUE)")
     is_equivalent_to(getArr(editarray(c(dm,"if( p )  g != 'm'"))),array(c(F,T,F,T),dim=c(1,4)))
     is_equivalent_to(getArr(editarray(c(dm,"if( g == 'm' ) !p"))),array(c(F,T,F,T),dim=c(1,4)))
     is_equivalent_to(getArr(editarray(c(dm,"!p || g=='f'"))),array(c(F,T,F,T),dim=c(1,4)))
})

context("Editarray parsing")
test_that("parse editarray to character and back",{
    edts <- c(
        "g %in% c('m','f')",
        "p %in% c(FALSE,TRUE)",
        "if (p) !g=='m'")
    is_equivalent_to(editarray(edts), editarray(as.character(editarray(edts))))
})




