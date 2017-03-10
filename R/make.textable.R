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
#' make.textable(arr, value.formatstring = "%5.3e")
#' make.textable(arr, value.formatstring = "$%5.3e$")
#' make.textable(arr, cutfunction=function(x){x>1})
#' make.textable(arr, value.formatstring = "$%5.3e$")
#'
#' # to save into a file:
#' sink("testfile.txt")
#' make.textable(arr, value.formatstring = "%5.3f")
#' sink()


make.textable <- function(array0
						 , value.formatstring=NA
						 , title=NA
						 , label=NA
						 , cutfunction=function(x){return(FALSE)}
						 , rowcol="red!10"
						 ,sisetup=FALSE){
	# first: create a vector which rows should be colored
	colored.row <- apply(array0, 1, cutfunction)

	nrows <- dim(array0)[1]
	ncols <- dim(array0)[2]+1
	array0.CN <- colnames(array0)
	array0.RN <- rownames(array0)

	if(sisetup){
		cat("%% \\usepackage{siunitx} %Paket für Einheiten mitsamt der deutschen Anpassungen\n")
	}

	if(any(colored.row)){
		cat("%\\usepackage{booktabs}\n")
		cat("%\\usepackage{xcolor}\n")
		cat("%\\usepackage{colortbl} % http://ctan.org/pkg/colortbl\n")
		cat("%\\newcommand{\\rowcol}{\\rowcolor{",rowcol,"}} %\n\n", sep = "")
	}
	cat("\\begin{table}\n")
	cat("\\sisetup{round-mode=places\n,round-precision=3\n,scientific-notation=fixed\n,fixed-exponent=0\n}\n")
	cat("\\centering\n")
	if(is.na(value.formatstring)){
		value.formatstring <- "\\num{%12.6e}"
	}

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
		if(colored.row[i]){
			cat("\\rowcol ")
		}
		if(is.character(value.formatstring)){
			values <- sprintf(value.formatstring, array0[i, ])
			if(grepl(pattern = "%[0-9]*\\.?[0-9]*[eE].*\\$", value.formatstring)){ # I assume that value.formatstring is for a single value
				values <- sub("(-?[0-9]+\\.?[0-9]*)[eE]([+-][0-9]*)", "\\1\\\\cdot 10^{\\2}", values)
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
	cat("\\end{tabular}\n")
	cat("\\end{table}\n\n")
}
