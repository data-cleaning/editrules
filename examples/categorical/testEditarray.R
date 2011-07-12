require(testthat)

test_that("editarray works accoring to plan",{
    expect_identical(editarray(
        c("gender %in% c('m','f')",
          "pregnant %in% c('y','n')",
          "if(gender == 'm') pregnant == 'n'"
          )),
        neweditarray(
            E = array(c(TRUE,FALSE,TRUE,FALSE),
            ind = list(gender=c(gender:f=1,genger:m=2),pregnant=c(pregnant:n=3,pregnant:y=4)),
            sep=":",
            names="e1",
            levels=c("gender:f=1","gender:m=2","pregnant:n=3","pregnant:y=4")
            )
        )
    )
})


