#' Source a script function.name from function.dir
#'
#' @param function.name How is the script (without .R) called
#' @param function.dir  Where is the script located
#' @param silent 		If true, no messages are printed
#'
#' @return function.name is in workspace
#' @export
#'
#' @examples
source.function <- function(function.name, function.dir, silent=FALSE){
	if(!silent)
		catt("Sourcing ", function.name, ".R")
	source(file.path(function.dir, paste0(function.name, ".R")))
	if(!silent)
		catt("Sourced. ", function.name, "() is now in workspace.\n")
}
