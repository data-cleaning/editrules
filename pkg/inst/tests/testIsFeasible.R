require(testthat)

context("Feasibility checks")

test_that("isFeasible",{
    expect_false(isFeasible(editmatrix(c("x +y < 0","x+y>0")), warn=FALSE))
})

test_that("isFeasible, warning",{
    expect_warning(isFeasible(editmatrix(c("x +y < 0","x+y>0")), warn=TRUE))
})

test_that("isFeasible with 0==1",{
    expect_false(isFeasible(editmatrix("0==1"), warn=FALSE))
})
