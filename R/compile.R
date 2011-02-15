compileStatement <- function(value, compiler, parent) {
	lines <- c()
	if (!is.null(parent)) lines <- compiler$levelDown()$line(lines, '(', ' ')
	lines <- c(lines, sapply(value$.children, compile, compiler, value))
	if (!is.null(parent)) lines <- compiler$levelUp()$line(lines, ')')
	return(str_c(unlist(lines), collapse = '\n'))
}

compileChildren <- function(compiler, value, include = NULL, exclude = NULL) {
	if (!is.null(include)) {
		indices <- include
	} else {
		indices <- seq_along(value$.children)
		if (!is.null(exclude))
			indices <- indices[-exclude]
	}
	
	return(unlist(sapply(value$.children[indices], compile, compiler, value)))
}



compile <- function(value, compiler, parent) {
	value.class <- class(value)
	function.name <- str_c(
		'compile', toupper(substring(value.class, 1, 1)), substring(value.class, 2)
	)
	#print(str_c('Compile function name: ', function.name))
	get(function.name)(value, compiler, parent)
}

formatTuple <- function(value, quote = FALSE) {
	if (quote)
		value = sprintf('"%s"', value)
	
	return(sprintf('(%s)', str_c(value, collapse = ', ')))
}

countCharacter <- function(character, ...) {
	return(sapply(strsplit(unlist(list(...)), '', fixed = TRUE), function(value) {
		sum(value == character)
	}))
}

Compiler <- setRefClass('Compiler',
	contains = c(
		'DatabasrObject'
	),
	
	methods = list(
		initialize = function() {
			callSuper()
			setOptions(level = 0)
			return(.self)
		},
		
		getCounter = function(counter.name) {
			if (!counter.name %in% names(.options)) 
				return(.options[[counter.name]] <<- 1)
			return(.options[[counter.name]] <<- .options[[counter.name]] + 1)
		},
		
		getPadding = function() {
			return(str_c(str_c(rep('\t', getOption('level')), collapse = '')))
		},
		
		line = function(lines, line, padding) {
			if (missing(padding)) padding <- getPadding()
			if (missing(line)) return(str_c(padding, lines))
			return(c(lines, str_c(padding, line)))
		},
		
		addToLine = function(lines, value, sep = ' ') {
			lines[[length(lines)]] <- str_c(lines[[length(lines)]], value, sep = sep)
			return(lines)
		},
		
		levelUp = function() {
			setOptions(level = getOption('level') - 1)
			return(.self)
		},
		
		levelDown = function() {
			setOptions(level = getOption('level') + 1)
			return(.self)
		},
		
		finish = function(lines) {
			lines <- unlist(lines)
			lines[[length(lines)]] <- str_c(lines[[length(lines)]], ';')
			return(lines)	
		}
	)
)
