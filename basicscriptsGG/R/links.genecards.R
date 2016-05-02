#' Gene list to links from genecards.org
#'
#' @param genelist  vector of genes
#' @param urlfile  File where the links should be stored
#'
#' @return urlfile (writes it into workingDIR)
#' @export
#'
#' @examples
#' genes <- c("MYC", "NLRP11")
#' links.genecards(genes)
#' links.genecards(genes, urlfile="links.genecards.txt")
links.genecards <- function(genelist, urlfile="GeneCards.genelist.txt"){

	url.genelist <- sprintf("http://www.genecards.org/cgi-bin/carddisp.pl?gene=%-30s", genelist)
	write(url.genelist, urlfile)
}
