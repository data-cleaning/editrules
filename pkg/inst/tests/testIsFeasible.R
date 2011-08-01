require(testthat)
test_that("isFeasible",{
    expect_false(isFeasible(editmatrix(c("x +y < 0","x+y>0"))))
})

test_that("isFeasible with 0==1",{
    expect_false(isFeasible(editmatrix("0==1")))
})



