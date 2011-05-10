require(testthat)

test_that("Obviously unfeasable systems are detected",{
    expect_that(isObviouslyInfeasible(
        editmatrix("0*x == 1")) , is_true())    
    expect_that(isObviouslyInfeasible(
        editmatrix("0*x < -1")) , is_true())
    expect_that(isObviouslyInfeasible(
        editmatrix("1e-12*x <= -1")) , is_true())    
})

test_that("Obviously redundant rows are detected",{
    expect_that(isObviouslyRedundant(
        matrix(c(0,1e-12),nrow=1), "=="), is_true())
    expect_that(isObviouslyRedundant(
        matrix(c(0,1),nrow=1), "<="), is_true())
    expect_that(isObviouslyRedundant(
        matrix(c(1e-12,1),nrow=1), "<"), is_true())
})




