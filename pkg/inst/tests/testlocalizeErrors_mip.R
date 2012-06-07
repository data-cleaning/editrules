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
  
  sol <- errorLocalizer.mip(Et, x)
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
    expect_true(
        localizeErrors(editmatrix("x>0"),data.frame(x=-1),method='mip')$adapt[1,1]
    )
    loc <- localizeErrors(editmatrix("x>0"),data.frame(x=NA),method='mip')
    expect_true(loc$adapt[1,1])
})

test_that('localizeErrors handles trivial single edits with mip method',{
  expect_false( 
    localizeErrors( editmatrix("z == 100")
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


## editset
test_that("localizeError_mip editset",{
  Et <- editset(expression( p + c == t
                               , c - 0.6*t >= 0
                               , c>=0
                               , p >=0
                               )
                   )
  
  x <- c(p=755,c=125,t=200)
  
  sol <- errorLocalizer.mip(Et, x)
  expect_equal(sol$w, 1)
  expect_equivalent(sol$adapt, c(TRUE, FALSE, FALSE))
  expect_equivalent(sol$x_feasible, c(75, 125, 200))
})

test_that('localizeErrors works without specified weight, editset',{
  E <- editset(c('x+y==z','x < 1'))
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



test_that('localizeErrors works with single specified weight, editset',{
  
  expect_equivalent(localizeErrors(
    E       = editset(c('x+y==z','x<1')),
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


test_that('localizeErrors works with different weights per record, editset',{
  expect_equivalent(localizeErrors(
    E       = editset('x+y==z'),
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
    E = editset('x +y==z'),
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

test_that('localizeErrors handles single edits containing NA with mip method, editset',{
  expect_true(
    localizeErrors(editset("x>0"),data.frame(x=-1),method='mip')$adapt[1,1]
    )
  loc <- localizeErrors(editset("x>0"),data.frame(x=NA),method='mip')
  expect_true(loc$adapt[1,1])
})

test_that('localizeErrors handles trivial single edits with mip method, editset',{
  expect_false( 
    localizeErrors( editset("z == 100")
                    , data.frame(z=100)
                    , method='mip'
                    )$adapt[1,1])
})

test_that('localizeErrors handles a "<" edits correctly., editset',{
  loc <- localizeErrors( editset("x > 1")
                         , data.frame(x=1)
                         , method='mip'
                         )
  #print(loc)
  expect_true( localizeErrors( editset("x < 1")
                               , data.frame(x=1)
                               , method='mip'
                               )$adapt[1,1]
               )
})

test_that('localizeErrors',{
  E <- editset(c(
    "age %in% c('under aged','adult')",
    "maritalStatus %in% c('unmarried','married','widowed','divorced')",
    "positionInHousehold %in% c('marriage partner', 'child', 'other')",
    "if( age == 'under aged' ) maritalStatus == 'unmarried'",
    "if( maritalStatus %in% c('married','widowed','divorced')) !positionInHousehold %in% c('marriage partner','child')"
    ))
  record <- data.frame(age='under aged', maritalStatus='married', positionInHousehold='child')
  expect_equivalent(
    localizeErrors(E,record, method="mip")$adapt
    ,
    array(c(FALSE,TRUE,FALSE),dim=c(1,3))
    )
})

test_that('localizeErrors with outofrange error',{
  E <- editset(c(
    "age %in% c('under aged','adult')",
    "maritalStatus %in% c('unmarried','married','widowed','divorced')",
    "positionInHousehold %in% c('marriage partner', 'child', 'other')",
    "if( age == 'under aged' ) maritalStatus == 'unmarried'",
    "if( maritalStatus %in% c('married','widowed','divorced')) !positionInHousehold %in% c('marriage partner','child')"
    ))
  record <- data.frame(age='under aged', maritalStatus='unmarried', positionInHousehold='out-of-range')
  expect_equivalent(
    localizeErrors(E,record, method="mip")$adapt
    ,
    array(c(FALSE,FALSE,TRUE),dim=c(1,3))
    )
})

test_that("localizeErrors works with TRUE/FALSE",{
  E <- editset(expression(
    A %in% c(TRUE,FALSE),
    B %in% letters[1:4],
    if ( !A ) B %in% letters[1:2]
    ))
  
  # should run without errors...
  localizeErrors(E,data.frame(A=c(TRUE,FALSE),B=c('c',"d")), method="mip")
})



