Formatter <- setRefClass('Formatter',
	fields = c(
		# A list of character vectors (i.e., a list where each element is a character vector of one
		# or more lines).
		'.lines',
		'level'
	),
	contains = c(
		'DatabasrObject'
	),
	
	methods = list(
		initialize = function() {
			callSuper()
			initFields(.lines = list(), level = 0)
		},
		
		getPadding = function() {
			return(str_c(str_c(rep("  ", level), collapse = "")))
		},
		
		begin = function(value, padding = getPadding()) {
			.lines[[length(.lines) + 1]] <<- character(0)
			if (!missing(value)) line(value, padding)
			.self
		},
		
		#' Begin a new line.
		#' 
		#' @param line new line.
		#' @param padding padding to prepend to the line, defaults to \code{\link{getPadding}}.
		#' @return .self
		line = function(line, padding = getPadding()) {
			if (is.list(line)) line <- unlist(line)
			.lines[[length(.lines)]] <<- c(.lines[[length(.lines)]], str_c(padding, line))
			.self
		},
		
		lines = function(other.lines, padding = getPadding()) {
			for (other.line in other.lines) line(other.line, padding)
			.self
		},
		
		#' Add to the last line.
		#' 
		#' @param value value to add, a character vector of length one.
		#' @param sep separator to prepend to the value, defaults to " ".
		#' @return .self
		toLine = function(value, sep = " ") {
			last <- length(.lines[[length(.lines)]])
			.lines[[length(.lines)]][[last]] <<- str_c(.lines[[length(.lines)]][[last]], value, sep = sep)
			.self
		},
		
		up = function() {
			level <<- level - 1
			.self
		},
		
		down = function() {
			level <<- level + 1
			.self
		},
		
		
		end = function() {
			return.lines <- .lines[[length(.lines)]]
			.lines <<- .lines[-length(.lines)]
			return.lines
		},
		
		
		finish = function(other.lines) {
			other.lines <- unlist(other.lines)
			other.lines[[length(other.lines)]] <- str_c(other.lines[[length(other.lines)]], ";")
			str_c(other.lines, collapse = "\n")
		}
	)
)