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
    expect_equivalent(ub[,2], c(-1, -1, 1))
    expect_equivalent(ub[,3], c(-61, -31, -14))
})


test_that("get upperbound with NA",{
  
  E  <- editmatrix(expression(
    x  <= NA
   ,-x <= NA
    ))
  
  xlim <- generateXlims(x=c(x=NA_real_), list(x=c(-50,100)))
  #matrix(c(-100,100), ncol=2)
  ub <- getUpperBounds(E, xlim=xlim)
  expect_equivalent(ub[1,], c(100, 0, -100))
  expect_equivalent(ub[2,], c(50, 0, -50))  
})
