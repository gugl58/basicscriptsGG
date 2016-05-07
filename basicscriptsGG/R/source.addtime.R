#' A personal adaptation of source()
#'
#' This source.addtime() makes use of the RuntimeC-Class and includes making
#' a time-point aswell as sourcing the script.
#' Advantage: one-liner and the called script has access to its name.
#' The script will be sourced automatically in .GobalEnv (according to stackoverflow)
#' I also tested this and it seems to work.
#'
#'
#' @param scriptpath path to a script to execute
#' @param runtimecobject The RuntimeC-object where the timepoint should be added.
#'
#' @export
#'
#' @examples
#' library(basicscriptsGG)
#' A_RUNTIME <- RuntimeC()		# initialize runtime-array
#' A_RUNTIME <- add.timepoint(A_RUNTIME, name = "Start")
#'
#' source.addtime("ANY.Scriptpath", A_RUNTIME)

source.addtime <- function(scriptpath, runtimecobject){
	assign("A_scriptname", basename(scriptpath), envir=globalenv())
	runtimecobject <- add.timepoint(runtimecobject, name = A_scriptname)
	source(scriptpath)
	return(runtimecobject)
}
