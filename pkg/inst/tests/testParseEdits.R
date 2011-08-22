require(testthat)

context("Parsing")

test_that("parseEdits all works",{
  x <- c( "2*x < 1"
        , "if (A=='a') B == 'b'"
        , "if (A=='a') B == FALSE"
        , "if (A=='a') B > 1"
        , "if (c==1) B || C == FALSE"
        )
  e <- parseEdits(x)  
  expect_equal(length(e), 5)
})

test_that("parseEdits num works",{
  x <- c( "2*x < 1"
        , "if (A=='a') B == 'b'"
        , "if (A=='a') B == FALSE"
        , "if (A=='a') B > 1"
        , "if (c==1) B || C == FALSE"
        )
  e <- parseEdits(x, "num")  
  expect_equal(length(e), 1)
  expect_equal(e, expression(2 * x < 1))
})

test_that("parseEdits cat works",{
  x <- c( "2*x < 1"
        , "if (A=='a') B == 'b'"
        , "if (A=='a') B == FALSE"
        , "if (A=='a') B > 1"
        , "if (c==1) B || C == FALSE"
        )
  e <- parseEdits(x, "cat")  
  expect_equal(length(e), 2)
  expect_equal(e, expression( if (A == "a") B == "b"
                            , if (A == "a") B == FALSE
                            )
              )
})

test_that("parseEdits mix works",{
  x <- c( "2*x < 1"
        , "if (A=='a') B == 'b'"
        , "if (A=='a') B == FALSE"
        , "if (A=='a') B > 1"
        , "if (c==1) B || C == FALSE"
        )
  e <- parseEdits(x, "mix")  
  expect_equal(length(e), 2)
  expect_equal(e, expression( if (A == "a") B > 1
                            , if (c == 1) B || C == FALSE
                            )
               , label=deparse(e)
               )
})
