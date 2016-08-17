#' Prints a list of plots without printing every plot number
#'
#' @param plotlist      List of plots
#'
#' @return loads the "package"
#' @export
#'
#' @examples
#' library(ggplot2)
#' mat <- matrix(rnorm(300), nrow = 100, ncol = 3)
#' mat[, 3] <- ifelse(mat[, 3]>0.5, TRUE, FALSE)
#' mat <- as.data.frame(mat)
#' p0 <- ggplot(mat, aes(x=V1, y=V2)) + geom_point()
#' p1 <- ggplot(mat, aes(x=V1, y=V2, col=V3)) + geom_point()
#' plist <- list(p0,p1)
#'
#' print.plotlist(plist)
print.plotlist <- function(plotlist){
	for(plotN in 1:length(plotlist)){
		print(plotlist[[plotN]])
	}
	print(paste0(plotN, " plots plotted"))
}
