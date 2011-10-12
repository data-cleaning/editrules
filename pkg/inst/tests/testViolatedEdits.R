
require(testthat)

context("Detect violated edits")

test_that("Numerical edit violations are detected",{
    expect_equivalent(
        violatedEdits(
            editmatrix(c( "x+3*y==2*z", "x==z")),
                data.frame( 
                   x = c(0,2,1),
                   y = c(0,0,1),
                   z = c(0,1,1))
        ),
        matrix(c(FALSE,FALSE,TRUE,FALSE,TRUE,FALSE),nrow=3)
    )
    # with a tolerance
    expect_equivalent(
        violatedEdits(
            editmatrix(c( "x+3*y==2*z", "x==z")),
            data.frame( 
                x = c(0,2,1),
                y = c(0,0,1),
                z = c(0,1,1)),
            tol=100
        ),
        matrix(c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),nrow=3)
    )
})
				 
test_that("An empty editmatrix is always valid",{
    expect_equivalent(
        violatedEdits(
            editmatrix("x==1")[0],
                data.frame( 
                   x = c(0,2,1),
                   y = c(0,0,1),
                   z = c(0,1,1))
        ),
        matrix(nrow=3, ncol=0)
    )
})

test_that("violation of inequalities are detected",{
    expect_true(violatedEdits(editmatrix("x<0"),c(x=1))[1])
    expect_true(violatedEdits(editmatrix("x<0"),c(x=0))[1])
    expect_true(violatedEdits(editmatrix("x<0"),c(x=0,tol=1e-8))[1])
    expect_false(violatedEdits(editmatrix("x<0"),c(x=-1))[1])
    expect_true(all(
        violatedEdits(
        editmatrix("x < y"),
        data.frame(x=c(1,2),y=c(-1,3))==c(TRUE,FALSE))
    ))
})


test_that("categorical edit violations are detected",{
    E <-  editarray(c(
        "gender %in% c('male','female')",
        "pregnant %in% c(TRUE, FALSE)",
        "if( gender == 'male' ) !pregnant"))    
    dat <- data.frame(
        gender=c('male','male','female','cylon'), 
        pregnant=c(TRUE,FALSE,TRUE,TRUE)
    )
    expect_equivalent(
        violatedEdits(E,dat), 
        matrix(c(
            FALSE, FALSE,  TRUE,
            FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE,
            TRUE,  FALSE, FALSE),byrow=TRUE,nrow=4)
    )
    expect_equivalent(
        violatedEdits(E,dat,datamodel=FALSE), 
        matrix(c(
             TRUE,
            FALSE,
            FALSE,
            FALSE),byrow=TRUE,nrow=4)
    )
})
