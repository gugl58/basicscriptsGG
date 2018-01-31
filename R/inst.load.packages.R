#' Loads a package called "package" and installs it if necessary
#'
#' @param package       The package you want to load
#' @param silent        If "TRUE", no output is generated
#' @param instBiocLite  If it is a package from bioconductor, set it to TRUE
#'
#' @return loads the "package"
#' @export
#'
#' @examples
#' inst.load.packages("ggplot2") # loads (& installs) ggplot2
#' inst.load.packages("ggplot2", silent=TRUE)
#'
#' # if package not already installed it also installs it with inst.bioclite=TRUE
#' inst.load.packages("vioplot", inst.bioclite=TRUE)
#'
#' # if bioconductor package should be installed, inst.bioclite MUST be =TRUE
#' inst.load.packages("NanoStringQCPro", inst.bioclite=TRUE)
inst.load.packages <- function(package, silent=FALSE, inst.bioclite=FALSE){
	if(!package %in% .packages()){
		# Try to install
		if((package %in% installed.packages()[,"Package"]))
		{
			if(!silent)	message(paste("***", package, "already installed"))
		}else{
			if(inst.bioclite == TRUE)
			{
				source("http://bioconductor.org/biocLite.R")
				biocLite(package)
			}else{
				#repos needed for non-interactive mode.
				install.packages(package, repos="http://cran.rstudio.com/")
			}
		}


		# Now installing should be finished, load the library
		if((package %in% installed.packages()[,"Package"]))
		{
			library(package, character.only = TRUE)
			if(!silent)	message(paste("***", package, as.character(packageVersion(package)),
									  "was loaded in the environment"))
		}else{
			if(!silent)	message(paste("***", package, as.character(packageVersion(package)),
									  "was not installed properly"))
		}
	}
}
