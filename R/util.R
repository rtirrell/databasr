#' Test whether two vectors or lists have exactly the same elements, perhaps
#' in a different order.
#' 
#' @param v1 a vector or list.
#' @param v2 a vector or list.
#' @return \code{TRUE} if the vectors have the same elements in some order,
#'   \code{FALSE} otherwise.
#' @export
have_same_elements <- function(v1, v2) {
	if (length(v1) != length(v2)) FALSE
	else length(intersect(v1, v2)) == length(v1)
}

#' Capitalize a string.
#' 
#' @param value a character vector of length one
#' @return the capitalized
#' @examples 
#' capitalize("hello world") # "Hello world"
#' @export
capitalize <- function(values) {
	str_c(toupper(str_sub(values, 1, 1)), str_sub(values, 2))
}
