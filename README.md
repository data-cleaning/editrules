[![Build Status](https://travis-ci.org/data-cleaning/editrules.svg)](https://travis-ci.org/data-cleaning/editrules)
[![CRAN](http://www.r-pkg.org/badges/version/editrules)](http://cran.r-project.org/package=editrules/)
[![Downloads](http://cranlogs.r-pkg.org/badges/editrules)](http://www.r-pkg.org/pkg/editrules) 

**`editrules` has been succeeded by R packages: [validate](http://cran.r-project.org/package=validate) and [errorlocate](http://cran.r-project.org/package=errorlocate)**

editrules
=========

R package for parsing edit rules
The editrules package aims to provide an environment to conveniently define, read and check recordwise data constraints including
* Linear (in)equality constraints for numerical data
* Constraints on value combinations of categorical data
* Conditional constraints on numerical and/or mixed data

In literature these constraints, or restrictions are refered to as _edits_. 
editrules can perform common rule set manipulations like variable elimination and value substitution, 
and offers error localization functionality based on the (generalized) paradigm of Fellegi and Holt. 
Under this paradigm, one determines the smallest (weighted) number of variables to adapt such that no
(additional or derived) rules are violated. The paradigm is based on the assumption that errors are distributed 
randomly over the variables and there is no detectable cause of error. 
It also decouples the detection of corrupt variables from their correction. 
For some types of error, such as sign flips, typing errors or rounding errors, this assumption does not hold. 
These errors can be detected and are closely related to their resolution. 
The reader is referred to the deducorrect package for treating such errors.

To install the latest version in R:
```R
install.packages("editrules")
```

To get started, see the [editrules vignette](https://cran.r-project.org/web/packages/editrules/vignettes/editrules-vignette.pdf).

