require(testthat)

test_that("Obviously unfeasable systems are detected",{
    expect_that(editrules:::isObviouslyUnfeasable(
        matrix(c(0,1),nrow=1),"==") , is_true())    
    expect_that(editrules:::isObviouslyUnfeasable(
        matrix(c(0,-1),nrow=1),"<") , is_true())    
    expect_that(editrules:::isObviouslyUnfeasable(
        matrix(c(1e-12,-1),nrow=1),"<=") , is_true())    
})


source("fourierMotzkin.R")
test_that("Obviously redundant rows are detected",{
    expect_that(isObviouslyRedundant(
        matrix(c(0,1e-12),nrow=1), "=="), is_true())
    expect_that(isObviouslyRedundant(
        matrix(c(0,1),nrow=1), "<="), is_true())
    expect_that(isObviouslyRedundant(
        matrix(c(1e-12,1),nrow=1), "<"), is_true())
})




