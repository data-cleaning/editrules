
context("Error localization: numerical data.frames")


test_that('localizeErrors works without specified weight',{
    
    expect_equivalent(localizeErrors(
        E       = editmatrix(c('x+y==z','x<1')),
        dat     = data.frame(
                  x = c(1,1,1),
                  y = c(1,1,1),
                  z = c(1,1,1)
                )
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



test_that('localizeErrors works with single specified weight',{
    
    expect_equivalent(localizeErrors(
        E       = editmatrix('x+y==z'),
        dat     = data.frame(
                  x = c(1,1,1),
                  y = c(1,1,1),
                  z = c(1,1,1)
                ),
        weight  = c(1,2,2),
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
                 )
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
                 )
        ),
        throws_error()
    )
})


