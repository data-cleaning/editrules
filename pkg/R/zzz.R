
.onAttach <- function(libname,pkgname){
    msg <- "Please note that function 'editrules' is deprecated and will be removed in the next version. Use as.data.frame'"
    nfc <- "Use suppressPackageStartupMessages(library(editrules)) to suppress this message on loading editrules."
    packageStartupMessage(msg)
    packageStartupMessage(nfc)
}


