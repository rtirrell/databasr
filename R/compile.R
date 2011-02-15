compileStatement <- function(value, formatter, parent) {
	lines <- c()
	if (!is.null(parent)) lines <- formatter$levelDown()$line(lines, '(', ' ')
	lines <- c(lines, sapply(value$.children, compile, formatter, value))
	if (!is.null(parent)) lines <- formatter$levelUp()$line(lines, ')')
	return(str_c(unlist(lines), collapse = '\n'))
}

compileChildren <- function(formatter, value, include = NULL, exclude = NULL) {
	if (!is.null(include)) {
		indices <- include
	} else {
		indices <- seq_along(value$.children)
		if (!is.null(exclude))
			indices <- indices[-exclude]
	}
	
	return(unlist(sapply(value$.children[indices], compile, formatter, value)))
}



compile <- function(value, formatter, parent) {
	value.class <- class(value)
	function.name <- str_c(
		'compile', toupper(substring(value.class, 1, 1)), substring(value.class, 2)
	)
	#print(str_c('Compile function name: ', function.name))
	get(function.name)(value, formatter, parent)
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

Formatter <- setRefClass('Formatter',
	fields = c(
		'level'
	),
	contains = c(
		'DatabasrObject'
	),
	
	methods = list(
		initialize = function() {
			callSuper()
			initFields(level = 0)
		},
		
		getCounter = function(counter.name) {
			if (!counter.name %in% names(.options)) 
				return(.options[[counter.name]] <<- 1)
			return(.options[[counter.name]] <<- .options[[counter.name]] + 1)
		},
		
		getPadding = function() {
			return(str_c(str_c(rep("  ", level), collapse = "")))
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
			level <<- level - 1
			.self
		},
		
		levelDown = function() {
			level <<- level + 1
			.self
		},
		
		finish = function(lines) {
			lines <- unlist(lines)
			lines[[length(lines)]] <- str_c(lines[[length(lines)]], ';')
			return(lines)	
		}
	)
)

#' Class implementing DBMS-specific compilation behavior (syntax and formatting).
#' If a need arises to add behavior between preparation and this step, we'll add a
#' database-specific preparation step that follows the database-agnostic step, making the workflow
#' composition, agnostic preparation, specific preparation, compilation/formatting.
Compiler <- setRefClass('Compiler',
	fields = c(
		'statement',
		'formatter',
		'identifier.collapse',
		'identifier.quote'
	),
	contains = c(
		'DatabasrObject'
	),
	methods = list(
		initialize = function(statement = NULL, formatter = NULL) {
			initFields(
				statement = statement, 
				formatter = formatter,
				identifier.collapse = NULL,
				identifier.quote = NULL
			)
			
			identifier.base <<- str_c(identifier.quote, "%s", identifier.quote)
			callSuper()
		},
		
		prepareIdentifier = function(...) {
			args <- paste(list(...), collapse = '\n')
			str_c(
				sprintf(identifier.base, unlist(str_split(args, fixed('.')))), 
				collapse = identifier.collapse
			)
		},
		
		##
		# R literals.
		##
		compileInteger = function(value) {
			if (length(value) > 1) formatTuple(value)
			else value
		},
		
		compileNumeric = function(value) {
			if (length(value) > 1) formatTuple(value)
			else value
		},
		
		compileCharacter = function(value) {
			if (length(value) > 1) formatTuple(value, TRUE)
			else sprintf("'%s'", value)
		},
		
		compileLogical = function(value) {
			if (is.na(value)) 'NULL'
			else if (value) 'TRUE'
			else 'FALSE'
		},
		
		##
		# Atomic elements.
		## 
		compileTable = function(value) {
			prepareIdentifier(value$getName())
		},
		
		compileField = function(value) {
		},
		
		##
		# Clauses.
		##
		compileSelectClause = function(value) {
		},
		
		compileFromClause = function(value) {
		},
		
		##
		# Statements.
		## 
		compileSelectStatement = function(value) {
			lines <- c()
			if (!is.null(parent)) lines <- formatter$levelDown()$line(lines, '(', ' ')
			lines <- c(lines, sapply(value$.children, compile, formatter, value))
			if (!is.null(parent)) lines <- formatter$levelUp()$line(lines, ')')
			str_c(unlist(lines), collapse = '\n')
		}
		
	)
)

MySQLCompiler <- setRefClass('MySQLCompiler',
	contains = c(
		'Compiler'
	),
	methods = list(
		initialize = function() {
			initFields(
				identifier.collapse = ".",
				identifier.quote = "`"
			)
			callSuper()
		}
	)
)

PostgresCompiler <- setRefClass('PostgresCompiler',
	contains = c(
		'Compiler'
	),
	methods = list(
		initialize = function() {
			initFields(
				identifier.collapse = ".",
				identifier.quote = '"'
			)
			callSuper()
		}
	)
)
			
