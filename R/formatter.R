Formatter <- setRefClass('Formatter',
	fields = c(
		# A list of character vectors (i.e., a list where each element is a 
		# character vector of one or more lines).
		'stack',
		# The current stack level.
		'level'
	),
	contains = c(
		'DatabasrObject'
	),
	
	methods = list(
		initialize = function() {
			callSuper()
			initFields(stack = list(), level = 0)
		},
		
		get_padding = function() {
			str_c(str_c(rep('  ', level), collapse = ''))
		},
		
		#' Push a value onto the stack.
		begin = function(value, padding = get_padding()) {
			stack[[length(stack) + 1]] <<- character(0)
			if (!missing(value)) line(value, padding)
			.self
		},
		
		#' Begin a new line.
		#' 
		#' @param line new line.
		#' @param padding padding to prepend to the line, defaults to \code{\link{get_padding}}.
		#' @return .self
		line = function(line, padding = get_padding()) {
			if (is.list(line)) line <- unlist(line)
			stack[[length(stack)]] <<- c(
				stack[[length(stack)]], str_c(padding, line)
			)
			.self
		},
		
		lines = function(other.lines, padding = get_padding()) {
			for (other.line in other.lines) line(other.line, padding)
			.self
		},
		
		#' Add a value to the last line.
		#' 
		#' @param value value to add, a character vector of length one.
		#' @param sep separator to prepend to the value, defaults to ' '.
		#' @return .self
		to_line = function(value, sep = ' ') {
			last <- length(stack[[length(stack)]])
			stack[[length(stack)]][[last]] <<- str_c(
				stack[[length(stack)]][[last]], value, sep = sep
			)
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
		
		#' Pop the stack return the popped value.
		end = function() {
			return.lines <- stack[[length(stack)]]
			stack <<- stack[-length(stack)]
			return.lines
		},
		
		#' Collapse the given lines by a newline and terminate with a semicolon.
		finish = function(other.lines) {
			other.lines <- unlist(other.lines)
			other.lines[[length(other.lines)]] <- str_c(
				other.lines[[length(other.lines)]], ';'
			)
			str_c(other.lines, collapse = '\n')
		}
	)
)