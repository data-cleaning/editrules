require(testthat)
require(lpSolveAPI)

context("Localize errors using MIP")

test_that("localizeError_mip",{
    Et <- editmatrix(c(
        "p + c == t",
        "c - 0.6*t >= 0",
        "c>=0",
        "p >=0"
        )
               )

    x <- c(p=755,c=125,t=200)

    sol <- localizeError_mip(Et, x)
    expect_equal(sol$w, 1)
    expect_equivalent(sol$adapt, c(FALSE, TRUE, FALSE))
    expect_equivalent(sol$x_feasible, c(125, 75, 200))
})