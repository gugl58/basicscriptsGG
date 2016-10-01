#' Booktabs table from matrix
#' Make a latex table to use with the booktaps package from any array
#'
#' @param array0		An array
#' @param format.string 	How should the values of the array be formatted? (Not the row/colnames)
#'
#' @return
#' @export
#'
#' @examples
#' arr <- matrix(rnorm(20), ncol=4, nrow=5)
#' rownames(arr) <- paste0("Row", 1:5)
#' colnames(arr) <- paste0("Col", 1:4)
#' makeTexTable(arr)
#' makeTexTable(arr, format.string = "%5.3f")
#'
#' # to save into a file:
#' sink("testfile.txt")
#' makeTexTable(arr, format.string = "%5.3f")
#' sink()


make.textable <- function(array0
						 ,format.string=NA){
	nrows <- dim(array0)[1]
	ncols <- dim(array0)[2]
	array0.CN <- colnames(array0)
	array0.RN <- rownames(array0)
	cat("\\begin{tabular}{", rep("l", ncols), "}", sep = "")
	cat("\n\\toprule\n")
	cat(c("", array0.CN), sep=" & ")
	cat("\\\\\n")
	for(i in 1:nrows){
		if(is.character(format.string)){
			cat(array0.RN[i]
				, sprintf(format.string, array0[i, ])
				, sep=" & \t")
		}else{
			cat(array0.RN[i], array0[i, ], sep=" & \t")
		}
		cat("\\\\\n")
	}
	cat("\\bottomrule\n")
	cat("\\end{tabular}")
}
