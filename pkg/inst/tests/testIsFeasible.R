require(testthat)
test_that("isFeasible",{
    expect_false(isFeasible(editmatrix(c("x +y < 0","x+y>0"))))
})



