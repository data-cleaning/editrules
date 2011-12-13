
expandEdits("x_i > 0", i = 1:10)

expandEdits("#var > 0", prefix="#", var = c("turnover", "profit", "employees"))

expandEdits("x_i < y_j", i=1:3,j=2:3)

expandEdits("sum_i(x_i) == y", i=1:3)

expandEdits("sum_month(x_year__month) == y_year", month=1:12, year=2010:2011)
