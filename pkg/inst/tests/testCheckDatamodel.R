
context('Check datamodel')

test_that('checkDatamodel.editmatrix returns NULL',{
    expect_identical(
        checkDatamodel(editmatrix('x+y==z'),dat=data.frame(x=1,y=2,z=3)),
        NULL
    )
})

test_that('checkDatamodel.editarray works',{
    # dat has column also in E
    expect_equivalent(    
        checkDatamodel(
            E = editarray('x %in% 1:2'),
            dat = data.frame(x=1:3)
        )$adapt[,1],
        c(FALSE,FALSE,TRUE)
    )
    # dat has column not specified by E
    expect_equivalent(    
        checkDatamodel(
            E = editarray('x %in% 1:2'),
            dat = data.frame(y=1:3,x=1:3)
        )$adapt[,1],
        c(FALSE,FALSE,FALSE)
    )
    # dat misses a variable, specified in E
    expect_error(    
        checkDatamodel(
            E = editarray('x %in% 1:2'),
            dat = data.frame(y=1:3)
        )
    )
    # dat computes correct weights
    expect_equivalent(
        checkDatamodel(
            E = editarray(c('x %in% 1:2','y %in% c("a","b")')),
            dat = data.frame(x=1:4,y=c('a','c','b','c'))
        )$status$weight,
        c(0,1,1,2)
    )
})




