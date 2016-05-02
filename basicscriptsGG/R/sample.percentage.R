#' Divides vector0 into random "length(percentages)" parts
#'
#' @param vector0     Vector of numbers, strings, etc. ("sample" must handle it)
#' @param percentages Determines how many groups and how many (percentagewise)
#' 					  samples should be per group
#' @param seed0       A seed for reproducability, seed will NOT be the same
#' 					  for each group (but reproducible: seed <- seed0 + #group)
#'
#' @return List of sorted, by "percentages" randomly divided vector0
#' @export
#'
#' @examples
#'
#' words  <- c("a","b","c","d")
#' sample.percentage(words)
#'
#'
#' vec <- 0:99
#' sample.percentage(vec)
#'
#' sample.percentage(vec, percentages=c(0.5,0.5))
#'
#' sample.percentage(vec, percentages=c(0.1, 0.1, 0.1, 0.7))
#'
sample.percentage <- function(vector0, percentages=c(0.5,0.2,0.3), seed0=NA)
{
	if(sum(percentages) != 1)
	{
		stop("Added percentages \"percentages\" are not 1")
	}

	residue <- vector0
	divlist <- vector("list", length(percentages))

	warn <- FALSE
	for( i in 1:length(percentages))
	{
		if(length(residue)<=1 || i==length(percentages))
		{
			# divmat[i] <- residue
			divlist[[i]] <- residue
			if(i!=length(percentages) && !warn)
			{
				warning("Insufficient residues, take a bigger vectorsize")
				warn <- TRUE
			}
		}else
		{
			if(!is.na(seed0))
			{
				set.seed(seed0 + i)	#reproducability, without i: always the same sample!
			}
			divlist[[i]] <- sample(residue, ceiling(length(vector0)*percentages[i]), replace = FALSE)
		}
		residue <- setdiff(residue, divlist[[i]])
		divlist[[i]] <- sort(divlist[[i]])
	}
	if(any(duplicated(unlist(divlist))))
	{
		stop("Duplicates in divlist")
	}
	return(divlist)
}
