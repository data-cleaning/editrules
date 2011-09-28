Et <- editmatrix(c(
        "p + c == t",
        "c - 0.6*t >= 0",
        "c>=0",
        "p >=0"
        )
               )

x <- c(p=755,c=125,t=200)

editrules:::localizeError_mip(Et, x)
