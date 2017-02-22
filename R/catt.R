#' A modified cat()
#'
#' A modified cat() (therefore catt()), to have empty space before output generally
#' If defince.catt() is done as wished, catt's usage is as cat().
#'
#' @param ntab    How many tabs indent
#' @param nspace  How many spaces indent
#'
#' @export
#'
#' @examples
#' var <- "testtext\n"
#' catt <- define.catt(nspace=2) # from now on, all catt("anyText") are indented by two spaces
#' catt(var, "\n", "  ", var)
#' catt <- define.catt() # reset to 0 indent
#'
#' # defines overwrite each other
#' catt(var)
#' catt <- define.catt(nspace=2)
#' catt(var)
#' catt <- define.catt(nspace=3)
#' catt(var)
#'
#'# combinations are not possible in a usefull way.
#' catt <- define.catt(nspace=2, ntab=3)
#' catt(var)
#' catt <- define.catt()
define.catt <- function(ntab = NULL, nspace=NULL){
	catt <- function(...){
		if(!is.null(ntab)) cat(paste0(paste(rep("\t", ntab), collapse = ""), ...))
		if(!is.null(nspace)) cat(paste0(paste(rep(" ", nspace), collapse = ""), ...))
		if(is.null(ntab) && is.null(nspace)) cat(..., sep = "")
	}
	return(catt)
}
