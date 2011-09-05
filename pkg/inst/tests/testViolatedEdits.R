
require(testthat)

context("Detect violated edits")

test_that("Numerical edit violations are detected",{
    is_equivalent_to(
        violatedEdits(
            editmatrix(c( "x+3*y==2*z", "x==z")),
                data.frame( 
                   x = c(0,2,1),
                   y = c(0,0,1),
                   z = c(0,1,1))
        ),
        matrix(c(FALSE,FALSE,TRUE,FALSE,TRUE,FALSE),nrow=3)
    )
})
				 

test_that("categirical edit violations are detected",{
    E <-  editarray(c(
        "gender %in% c('male','female')",
        "pregnant %in% c(TRUE, FALSE)",
        "if( gender == 'male' ) !pregnant"))    
    dat <- data.frame(
        gender=c('male','male','female','cylon'), 
        pregnant=c(TRUE,FALSE,TRUE,TRUE)
    )
    is_equivalent_to(
        violatedEdits(E,dat), 
        matrix(c(
            FALSE, FALSE,  TRUE,
            FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE,
            TRUE,  FALSE, FALSE),byrow=TRUE,nrow=4)
    )
    is_equivalent_to(
        violatedEdits(E,dat,datamodel=FALSE), 
        matrix(c(
             TRUE,
            FALSE,
            FALSE,
            FALSE),byrow=TRUE,nrow=4)
    )
})






