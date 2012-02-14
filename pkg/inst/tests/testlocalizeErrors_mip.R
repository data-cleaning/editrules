require(testthat)
library(lpSolveAPI)

context("Localize errors using MIP")

test_that("localizeError_mip",{
  Et <- editmatrix(expression( p + c == t
                             , c - 0.6*t >= 0
                             , c>=0
                             , p >=0
                             )
                  )
  
  x <- c(p=755,c=125,t=200)
  
  sol <- localize_mip_rec(Et, x)
  expect_equal(sol$w, 1)
  expect_equivalent(sol$adapt, c(TRUE, FALSE, FALSE))
  expect_equivalent(sol$x_feasible, c(75, 125, 200))
})

test_that('localizeErrors works without specified weight',{
  E <- editmatrix(c('x+y==z','x < 1'))
  dat <- data.frame(
    x = c(1,0,2),
    y = c(1,1,1),
    z = c(1,1,1)
  )
  
  loc <- localizeErrors( E = E
                       , dat = dat
                       , method="mip"
                       )
  
  #print(loc)
  expect_equivalent( loc$adapt
                   , matrix(c(
                      TRUE , FALSE, FALSE,
                      FALSE , FALSE, FALSE,
                      TRUE , FALSE, FALSE),
                           nrow=3,
                           byrow=TRUE
                           )
                  )
  
  
})



test_that('localizeErrors works with single specified weight',{
  
  expect_equivalent(localizeErrors(
    E       = editmatrix(c('x+y==z','x<1')),
    dat     = data.frame(
      x = c(1,1,1),
      y = c(1,1,1),
      z = c(1,1,1)
      ),
    weight  = c(1,2,2),
    method="mip"
    )$adapt,
                    matrix(c(
                      TRUE , FALSE, FALSE,
                      TRUE , FALSE, FALSE,
                      TRUE , FALSE, FALSE),
                           nrow=3,
                           byrow=TRUE
                           )
                    )
  
  
})


test_that('localizeErrors works with different weights per record',{
  expect_equivalent(localizeErrors(
    E       = editmatrix('x+y==z'),
    dat     = data.frame(
      x = c(1,1,1),
      y = c(1,1,1),
      z = c(1,1,1)
      ),
    weight  = matrix(c(
      1,2,2,
      2,1,2,
      2,2,1),
                     nrow=3,
                     byrow=TRUE
                     ),
    method="mip"
    )$adapt,
                    matrix(c(
                      TRUE , FALSE, FALSE,
                      FALSE, TRUE , FALSE ,
                      FALSE, FALSE, TRUE),
                           nrow=3,
                           byrow=TRUE
                           )
                    )
  expect_that(localizeErrors(
    E = editmatrix('x +y==z'),
    dat     = data.frame(
      x = c(1,1,1),
      y = c(1,1,1),
      z = c(1,1,1)
      ),
    weight  = matrix(c(
      1,2,2,
      2,2,1),
                     nrow=3,
                     byrow=TRUE
                     ),
    method="mip"
    ),
              throws_error()
              )
})



test_that('localizeErrors handles single edits with mip method',{
    expect_true(localizeErrors(editmatrix("x>0"),data.frame(x=-1),method='mip')$adapt[1,1])
    loc <- localizeErrors(editmatrix("x>0"),data.frame(x=NA),method='mip')
    #print(loc)
    expect_true(loc$adapt[1,1])
})

test_that('localizeErrors handles trivial single edits with mip method',{
  expect_false( localizeErrors( editmatrix("z == 100")
                              , data.frame(z=100)
                              , method='mip'
                              )$adapt[1,1])
})

test_that('localizeErrors handles a ">" edits correctly.',{
  loc <- localizeErrors( editmatrix("x > 1")
                       , data.frame(x=1)
                       , method='mip'
                       )
  #print(loc)
  expect_true( localizeErrors( editmatrix("x < 1")
                             , data.frame(x=1)
                             , method='mip'
                             )$adapt[1,1]
             )
})


