
# The following is an example by Williams (1986). Eliminating all variables
# except z maximizes 4x1 - 5x2 -3x3:
E <- editmatrix(c(
     "4*x1 - 5*x2 - 3*x3 + z <= 0",
     "-x1 + x2 -x3 <= 2",
     "x1 + x2 + 2*x3 <= 3",
     "-x1 <= 0",
     "-x2 <= 0",
     "-x3 <= 0"))
# get augmented matrix from editmatrix:
A <- as.matrix(E)

# setting renormalize=FALSE to reproduce the numbers from Williams.
# eliminate 1st variable
(P1 <- fourierMotzkin(A, 1, renormalize=FALSE))
# eliminate 2 variables. Note that redundant rows have been eliminated
(P2 <- fourierMotzkin(A, c("x1","x2"), renormalize=FALSE))
# finally, the answer:
(P3 <- fourierMotzkin(A, 1:3, renormalize=FALSE))


