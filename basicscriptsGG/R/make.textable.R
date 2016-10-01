#' Booktabs table from matrix
#' Make a latex table to use with the booktaps package from any array
#'
#' @param array0		An array
#' @param value.formatstring 	How should a single value of the array be formatted? (Not the row/colnames)
#'
#' @return
#' @export
#'
#' @examples
#' arr <- matrix(rnorm(20), ncol=4, nrow=5)
#' rownames(arr) <- paste0("Row", 1:5)
#' colnames(arr) <- paste0("Col", 1:4)
#' make.textable(arr)
#' make.textable(arr, title = "abc")
#' make.textable(arr, title = "abc", label = "testlabel")
#' make.textable(arr, value.formatstring = "%5.3f")
#'
#' # to save into a file:
#' sink("testfile.txt")
#' make.textable(arr, value.formatstring = "%5.3f")
#' sink()


make.textable <- function(array0
						 , value.formatstring=NA
						 , title=NA
						 , label=NA){
	nrows <- dim(array0)[1]
	ncols <- dim(array0)[2]+1
	array0.CN <- colnames(array0)
	array0.RN <- rownames(array0)
	cat("\\begin{table}\n\\centering\n")
	if(!is.na(title)){
		cat("\\caption{", title, "}")
		if(is.na(label)){
			label <- paste0("tab:", sub(" ", "", title))
		}
	}
	if(!is.na(label)){
		cat("\t\\label{", label, "}\n")
	}
	cat("\\begin{tabular}{", rep("l", ncols), "}", sep = "")
	cat("\n\\toprule\n")
	cat(c("", array0.CN), sep=" & ")
	cat("\\\\ \\midrule\n")
	for(i in 1:nrows){
		if(is.character(value.formatstring)){
			values <- sprintf(value.formatstring, array0[i, ])
			if(grepl(pattern = "%[0-9]*\\.?[0-9]*[eE].*\\$", value.formatstring)){ # I assume that value.formatstring is for a single value
				values <- sub("(-?[0-9]+\\.?[0-9]*[eE])([+-][0-9]*)", "\\1^{\\2}", values)
			}

			cat(array0.RN[i]
				, values
				, sep=" & \t")
		}else{
			cat(array0.RN[i], array0[i, ], sep=" & \t")
		}
		cat("\\\\\n")
	}
	cat("\\bottomrule\n")
	cat("\\end{tabular}")
	cat("\\end{table}\n\n")
}
