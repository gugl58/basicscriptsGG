#' Constructor for S3 RuntimeC-Class
#'
#' Invokes a RuntimeC-Object holding a matrix with 5 cols and 0 rows where future
#' time-points are stored
#'
#' @return RuntimeC-object
#'
#' @examples
#' rt_object <- RuntimeC()
#' @export
RuntimeC <- function() {
	value  <- list(mat=matrix(nrow=0, ncol=5))
	colnames(value$mat) <- c("TotalUserCPUtime", "SystemCPUtime",
							 "realTime", "", "")
	# class can be set using class() or attr() function
	attr(value, "class") <- "RuntimeC"
	value
}

#' Adding timepoints
#'
#' Adds a named timepoint to object
#'
#' @param object RuntimeC-class object
#' @param name   Name of time point
#' @param scripttime
#' Time for one script, given by "proc.time() - proc.time()"
#'
#' @examples
#' library(basicscriptsGG)
#' rt_object <- RuntimeC()
#' rt_object <- add.timepoint(rt_object, "useless", proc.time()-proc.time())	# add new timepoint (with zero time)
#' t1 <- proc.time()
#' Sys.sleep(1)		# someFunction
#' t2 <- proc.time()
#' rt_object <- add.timepoint(rt_object, "someFunction", t2-t1)	# add new timepoint (with zero time)
#' rt_object <- add.timepoint(rt_object, "someFunction", t2-t1)	# add new timepoint (with zero time)
#' rt_object <- add.timepoint(rt_object, "someFunction", t2-t1)	# add new timepoint (with zero time)
#'
#' @rdname add.timepoint
#' @export add.timepoint
add.timepoint <- function(object, name, scripttime){ UseMethod("add.timepoint", object)}

#' @rdname add.timepoint
#' @method add.timepoint RuntimeC
#' @S3method add.timepoint RuntimeC
add.timepoint.RuntimeC <- function(object, name, scripttime) {
	object$mat <- rbind(object$mat, scripttime)
	rownames(object$mat)[dim(object$mat)[1]] <- name
	return(object)
}




#' Finalize the RuntimeC-Object
#'
#' Adds a summary of all times in RuntimeC
#'
#'
#' @param object     RuntimeC-object
#' @param forceFinal Forces adding the summary and calculating the relative times
#'
#' @return
#' object with 2 summary rows and all rows hold the scripttime of the rownames()
#'
#' @examples
#' library(basicscriptsGG)
#' rt_object <- RuntimeC()
#' rt_object <- add.timepoint(rt_object, "useless", proc.time()-proc.time())	# add new timepoint (with zero time)
#' t1 <- proc.time()
#' Sys.sleep(1)		# someFunction
#' t2 <- proc.time()
#' rt_object <- add.timepoint(rt_object, "someFunction", t2-t1)	# add new timepoint (with zero time)
#' rt_object <- summarizeRT(rt_object)	# subtract the first for (starting point)
#' @rdname summarizeRT
#' @export summarizeRT
summarizeRT <- function(object, forceFinal){ UseMethod("summarizeRT", object)}


#' @rdname summarizeRT
#' @method summarizeRT RuntimeC
#' @S3method summarizeRT RuntimeC
summarizeRT.RuntimeC <- function(object, forceFinal=FALSE) {
	if(dim(object$mat)[1] <= 1)
	{
		return(object)
	}
	final.rowname <- "Total time - Sum: hours"
	if(rownames(object$mat)[dim(object$mat)[1]] == final.rowname && !forceFinal)
	{
		print("Object already finished")
		return(object)
	}

	total.sum <- apply(object$mat, 2, sum)
	object$mat <- rbind(object$mat, total.sum)
	rownames(object$mat)[dim(object$mat)[1]] <- "Total time - Sum: sec"
	object$mat <- rbind(object$mat, total.sum/60)
	rownames(object$mat)[dim(object$mat)[1]] <- "Total time - Sum: min"
	object$mat <- rbind(object$mat, (total.sum/60)/60)
	rownames(object$mat)[dim(object$mat)[1]] <- final.rowname


	return(object)
}




#' Write function
#'
#' Just the normal write function made generic and RuntimeC-support added
#'
#'
#' @param object     See ?base::write
#' @examples
#' # See ?base::write. For RuntimeC:
#' library(basicscriptsGG)
#' rt_object <- RuntimeC()
#' rt_object <- add.timepoint(rt_object, "useless", proc.time()-proc.time())	# add new timepoint (with zero time)
#' t1 <- proc.time()
#' Sys.sleep(1)		# someFunction
#' t2 <- proc.time()
#' rt_object <- add.timepoint(rt_object, "someFunction", t2-t1)	# add new timepoint (with zero time)
#' rt_object <- summarizeRT(rt_object)	# subtract the first for (starting point)
#' write(rt_object, "test.txt")
#'
#' @rdname write
#' @export write
write <- function(x, ...) UseMethod("write")

#' @rdname write
#' @method write default
#' @S3method write default

write.default <- base::write

#' @rdname write
#' @method write RuntimeC
#' @S3method write RuntimeC
write.RuntimeC <- function(obj, file)
{
	width.save <- options()$width
	options(width=250)

	sink(file = file)
	print(round(obj$mat, digits = 2))
	sink()

	options(width = width.save)
}
