#' Source a script function.name from function.dir
#'
#' @param function.name How is the script (without .R) called
#' @param function.dir  Where is the script located
#'
#' @return function.name is in workspace
#' @export
#'
#' @examples
source.function <- function(function.name, function.dir){
	catt("Sourcing ", function.name, ".R")
	source(file.path(function.dir, paste0(function.name, ".R")))
	catt("Sourced. ", function.name, "() is now in workspace.\n")
}
