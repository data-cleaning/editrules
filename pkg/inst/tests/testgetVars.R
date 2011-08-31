
require(testthat)

context("getVars")

test_that("getVars.editmatrix works",{
        expect_identical(
                getVars(editmatrix(c( "x+3*y == 2*z", "x > 2"))),
                c("x","y","z")
        )
})
E <- editarray(c(
    "gender %in% c('male','female')",
    "pregnant %in% c(TRUE, FALSE)",
    "if( gender == 'male' ) pregnant == FALSE"
    )
)

getVars(E)




