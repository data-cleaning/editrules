
context("Editset")

test_that("editset parses categorical edits",{
    
    v <- expression(
        A %in% c('a','b'),
        B %in% c('c','d'),
        if ( A == 'a') B == 'c'
    )
    E <- editset(v)
    expect_equal(E$num,editmatrix(expression()))
    expect_equal(E$mixnum,editmatrix(expression()))
    expect_equal(E$cat,editarray(v))
    expect_equal(
        E$mixcat,
        editarray(expression(A %in% letters[1:2],B %in% letters[3:4])))
})

test_that("editset parses numerical edits",{
    v <- expression(x + y == z, 2*x -u == v)
    E <- editset(v)
    expect_equal(E$num,editmatrix(v))
    expect_equal(E$cat,editarray(expression()))
    expect_equal(E$mixnum,editmatrix(expression()))
    expect_equal(E$mixcat,editarray(expression()))
})

test_that("editset parses conditional numeric edits",{
    # test 1: inequalities
    v <- expression( if ( x > 0 ) y > 0 )
    E <- editset(v)
    expect_equal(E$num, editmatrix(expression()))
    expect_equal(E$cat, editarray(expression()))
    expect_equivalent(E$mixnum, editmatrix(expression(x>0,y<=0)))
    expect_equivalent(getArr(E$mixcat),array(c(F,T,F,T),dim=c(1,4)))

    # test 2: with equalities in if-statement
#    v <- expression( if ( x == 0 ) y == 0)

})







