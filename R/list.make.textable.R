#' Booktabs table from list of matrices
#' Make a latex table to use with the booktaps package from a list of arrays
#'
#' @param value.formatstring 	How should the values of the array be formatted? (Not the row/colnames)
#' @param arrlist
#' list of arrays
#' @param cutfunction
#' function which decides, based on all values of the matrix if a row should be colored
#' @param rowcol
#' the row color
#'
#'
#' @return
#' @export
#'
#' @examples
#' arr <- matrix(rnorm(20), ncol=4, nrow=5)
#' rownames(arr) <- paste0("Row", 1:5)
#' colnames(arr) <- paste0("Col", 1:4)
#' arrlist <- list("ARR1"=arr, "ARR2"=arr+1, "ARR3"=list("ARR3_1"=arr+2, "ARR3_2"=arr+3))
#' list.make.textable(arrlist)
#' list.make.textable(arrlist, value.formatstring = "%5.3f")
#' list.make.textable(arrlist, value.formatstring = "\\textit{%5.3f}")
#' list.make.textable(arrlist, value.formatstring = "\\textit{%5.3f}", sisetup=TRUE)
#' list.make.textable(arrlist, sisetup=TRUE)
#'
#' # to save into a file:
#' sink("testfile.txt")
#' list.make.textable(arrlist, value.formatstring = "%5.3f")
#' sink()


list.make.textable <- function(arrlist
							   ,value.formatstring=NA
							   , cutfunction=function(x){return(FALSE)}
							   , rowcol="red!10"
							   ,sisetup=FALSE){
	recursive.iflist <- function(maybeList, name=NA){
		if(! is.list(maybeList)){
			make.textable(array0 = maybeList
						  , value.formatstring = value.formatstring
						  ,title = name
						  , cutfunction=cutfunction
						  , rowcol=rowcol)	# then we finally got the array
		}
		for(listX in names(maybeList)){
			if(is.na(name)){
				name0 <- listX
			}else{
				name0 <- paste0(name, ":", listX)
			}
			recursive.iflist(maybeList[[listX]], name = name0)
		}
	}
	recursive.iflist(arrlist)
}
