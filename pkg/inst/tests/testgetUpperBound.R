context("Upperbound/softedits")

test_that("get upperbound",{
    
    E  <- editmatrix(expression(
                       x + y > 1
                     , x + -y > 1
                     , x + y < 1
                    ))    
    xlim <- matrix(c(-20,-40,5,10), ncol=2)
    ub <- getUpperBounds(E, xlim=xlim)
    
    expect_equivalent(ub[,1], c(60, 30, 15))
    expect_equivalent(ub[,2], c(-61, -31, -14))
})
