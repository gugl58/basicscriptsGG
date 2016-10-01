#' Booktabs table from list of matrices
#' Make a latex table to use with the booktaps package from a list of arrays
#'
#' @param arraylist			An (nested) list of arrays
#' @param format.string 	How should the values of the array be formatted? (Not the row/colnames)
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
#' list.make.textable(arrlist, format.string = "%5.3f")
#'
#' # to save into a file:
#' sink("testfile.txt")
#' list.make.textable(arrlist, format.string = "%5.3f")
#' sink()


list.make.textable <- function(arrlist
							   ,format.string=NA){
	recursive.iflist <- function(maybeList, name=NA){
		if(! is.list(maybeList)){
			make.textable(maybeList, title = name)	# then we finally got the array
		}
		for(listX in names(maybeList)){
			recursive.iflist(maybeList[[listX]], name = listX)
		}
	}
	recursive.iflist(arrlist)
}
