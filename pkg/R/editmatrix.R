# retourneert een vector met een fac van de variabelen 
retrieveSign <- function(e, fac=1){
   #stopifnot(is.language(e))
   if (length(e) == 1){
      #TODO check for numerics
      l <- fac
	  names(l) <- as.character(e)
	  return(l)
   }
   if (length(e) == 2){
      op <- deparse(e[[1]])
	  rhs <- e[[2]]
      if (op == "("){
	    return(retrieveSign(rhs, fac))
	  } 
   }
   if (length(e) == 3){
      op <- deparse(e[[1]])
	  lhs <- e[[2]]
	  rhs <- e[[3]]
	  lsign <- rsign <- fac
      if ( op == "=="
	    || op == "<"
	    || op == "=<"
	    || op == "-"
		 ){
	    rsign <- -1 * fac
	  } 
	  else if ( op == ">"
	         || op == ">="
		      ){
	    lsign <- -1 * fac
	  }
	  else if (op == "+"){
	  }
	  else if (op == "*"){
	    #TODO check if lhs is a numeric
	    return(retrieveSign(rhs, eval(lhs)*fac))
	  }
	  else { 
		stop(". Operator ", op, " not implemented", "Invalid expression:", e)
	  }
	  return(c( retrieveSign(lhs, lsign)
		      , retrieveSign(rhs, rsign)
		  )
		)
   }
   stop("Invalid expression:", e)
}

makeEditRow <- function(edt){
  if (length(edt) != 3){
     stop(paste("Invalid edit rule:", edt))
  }
  wgt  <- retrieveSign(edt)
  
  # op <- deparse(edt[[1]])
  # if ( op == ">"){
     # op <- "<"
  # } else if (op == ">="){
    # op <- "=<"
  # }
  # wgt[op] <- 1
	
  stopifnot(length(wgt)==length(unique(names(wgt))))
  return(wgt)  
}

#' transforms a list of R equalities into a matrix with +1, -1 and 0 for each variable
#'
#' i.e. x == y   results in  (x=-1, y=1, w=0, z=0)
#' and x == y + w results in (x=-1, y=1, w=1, z=0)
#' @export
#' @param editinfo data.frame containing R (in)equalities
#' @param editrules character vector with R (in)equalities
editmatrix <- function( editsinfo = NULL
					  , editrules = NULL
					  ){
	
	if (is.null(editsinfo)){
	   if (is.null(editrules)){
		 stop("No valid input")
	   }
	   edts <- parse(text=editrules)
	   editsinfo <- data.frame(edit=sapply(edts, deparse))
	}
	else {
	   edts <- parse(text=editsinfo$edit)
	   editsinfo$edit <- sapply(edts, deparse)
	}

	if (is.null(editsinfo$name)){
	   editsinfo$name <- paste("rule", seq(along.with=edts),sep=" ")
	}
	
	if (is.null(editsinfo$description)){
	   editsinfo$description <- rep("", length(edts))
	}
	editsinfo <- editsinfo[c(2,1,3)]

	stopifnot(is.language(edts))
    
    rowedts <- lapply(edts, function(edt){makeEditRow(edt)})
	vars <- unique(names(unlist(rowedts)))

	mat <- matrix( 0
	             , ncol=length(vars)
				 , nrow=length(rowedts)
				 , dimnames = list( edits = editsinfo$name
				                  , var=vars
								  )
				 )
				 
	for (i in 1:length(rowedts)){
	   mat[i,names(rowedts[[i]])] <- rowedts[[i]]
    }
	structure( mat
	         , class="editmatrix"
			 , editsinfo=editsinfo
			 , edits = edts
			 )
	# print(mat)
	# edits <-sapply(edts, deparse)
	# rownames(mat) <- edits
	# ret <- list( matrix = mat
	           # , edits = edits
	           # )
	# if (addChecks){
		# checks <- do.call( cbind
						 # , lapply(edts, function(edt){
										   # eval(edt, dat)
										# }
								 # )
						 # )
	   # colnames(checks) <- edits
	   # ret$checks <- checks
	# }
	# ret
}

#' check if object is an editmatrix
#' @export
is.editmatrix <- function(x){
   return(inherits(x, "editmatrix"))
}


#' retrieve editinfo on an editmatrix
#'
#' the edit info will be derived if x is a matrix
#' @export
editsinfo <- function(x){
   if (is.editmatrix(x)){
	return(attr(x, "editsinfo"))
   }
   
   mat <- as.matrix(x)
   if (is.null(mat)){
      stop("x should be a matrix")
   }
   
   vars <- colnames(mat)
   if (is.null(vars)){
       if ((n <- ncol(mat)) > length(letters)){
		   vars <- character(ncol(mat))
		   vars[1:ncol(mat)] <- letters	      
	   } else{
	      vars <- letters[1:n]
	   }
   }
   vars <- make.names(vars, unique=TRUE)
   colnames(mat) <- vars

   rulenames <- rownames(mat)
   if (is.null(rulenames)){
      rulenames <- paste("rule", seq(to=nrow(mat)))
   }
   
   er <- character(nrow(mat))
   for (i in 1:nrow(mat)){
      r <- mat[i,]
	  lhs <- r > 0
	  rhs <- r < 0
	  
	  r <- abs(r)
	  
	  facs <- paste(r, "*", vars, sep="")
	  facs[r==0] <- "" #remove 0's
	  facs[r==1] <- vars[r==1] #simplify 1's
	  
	  leftterm <- if (any(lhs)) paste(facs[lhs], collapse=' + ')
	              else 0
				  
	  rightterm <- if (any(rhs)) paste(facs[rhs], collapse=' + ')
	               else 0
	  er[i] <- paste(leftterm, "==", rightterm)
   }
   
   data.frame( name=rulenames
             , edit=er
			 , description=""
			 )
}

edits <- function(x){
   stopifnot(is.editmatrix(x))
   return(attr(x, "edits"))
}

#' transform a matrix into an edit matrix
as.editmatrix <- function(x){
   if (is.editmatrix(x)){
      return(x)
   }
   #TODO check for colnames
   mat <- as.matrix(x)
   structure( mat
            , class="editmatrix"
			, editsinfo=editinfo(mat)
			)
}