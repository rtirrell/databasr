#' Performs much better for character versus the below. 
#' Is marginally slower for numeric types.
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
capitalize <- function(value) {
	str_c(toupper(str_sub(value, 1, 1)), str_sub(value, 2))
}

have_same_elementsTwo <- function(v1, v2) {
	all.equal(sort(v1), sort(v2))
}

doSampleInt <- function() {
	sample.int(seq.int(100 * 1000), 10 * 1000 * 1000, replace = TRUE)
}

doSampleLetters <- function() {
	sample(LETTERS, 1000 * 1000, replace = TRUE)
}
	
timeSames <- function() {
	list(one = system.time(replicate(10, {
			t <- doSampleLetters()
			s <- doSampleLetters()
			have_same_elements(t, s)
		})),
	two = system.time(replicate(10, {
		t <- doSampleLetters()
		s <- doSampleLetters()
		have_same_elementsTwo(t, s)
	})))
}