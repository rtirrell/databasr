compileStatement <- function(value, formatter, parent) {
	lines <- c()
	if (!is.null(parent)) lines <- formatter$down()$line(lines, '(', ' ')
	lines <- c(lines, sapply(value$.children, compile, formatter, value))
	if (!is.null(parent)) lines <- formatter$up()$line(lines, ')')
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
		
		getCounter = function(counter.name) {
			if (!counter.name %in% names(.options)) .options[[counter.name]] <<- 1
			else .options[[counter.name]] <<- .options[[counter.name]] + 1
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

.OPERATOR.NAMES <- list(
	"=" = "eq",
	"!=" = "neq",
	">" = "gt",
	">="  = "ge",
	"<" = "lt",
	"<=" = "le",
	"IN" = "in",
	"NOT IN" = "nin"
)
	

#' Class implementing DBMS-specific compilation behavior (syntax and formatting).
#' If a need arises to add behavior between preparation and this step, we'll add a
#' database-specific preparation step that follows the database-agnostic step, making the workflow
#' composition, agnostic preparation, specific preparation, compilation/formatting.
Compiler <- setRefClass("Compiler",
	fields = c(
		"statement",
		"formatter",
		'identifier.collapse',
		'identifier.quote',
		'identifier.base'
	),
	contains = c(
		'DatabasrObject'
	),
	methods = list(
		initialize = function(statement = NULL, formatter = NULL) {
			initFields(
				statement = statement, 
				formatter = formatter
			)
		},
		
		prepare = function() {
			identifier.base <<- str_c(identifier.quote, "%s", identifier.quote)
			.self
		},
		
		compile = function() {
			lines <- dispatch(statement)
			formatter$finish(lines)
		},
		
		dispatch = function(value) {
			method.name <- str_c("compile", capitalize(class(value)))
			#print(str_c("Compile function name: ", method.name))
			# Here we can't just get the function, as it (may) need to be inferred.
			method <- findMethods("$")$envRefClass(.self, as.character(method.name))
			method(value)
		},
		
		dispatchChildren = function(value, include = NULL, exclude = NULL) {
			if (is.null(include)) {
				indices <- seq_along(value$.children)
				if (!is.null(exclude)) indices <- indices[-exclude]
			} else indices <- include
			
			unlist(sapply(value$.children[indices], dispatch))
		},
		
		compileIdentifier = function(...) {
			args <- paste(list(...), collapse = identifier.collapse)
			str_c(
				sprintf(identifier.base, unlist(str_split(args, fixed(identifier.collapse)))), 
				collapse = identifier.collapse
			)
		},
		
		compileAlias = function(value) {
			if (is.null(value$alias)) {
				fields <- value$findChildren("Field")
				if (length(fields) == 1) 
					prepareIdentifier(str_c(tolower(value$func), "_", fields[[1]]$name))
				else 
					prepareIdentifier(str_c(tolower(value$func), "_", formatter$getCounter(value$func)))
			} else compileIdentifier(value$alias)
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
			if (is.na(value)) "NULL"
			else if (value) "TRUE"
			else "FALSE"
		},
		
		##
		# Atomic elements.
		## 

		#' TODO here: possible aliasing?
		compileTable = function(value) {
			compileIdentifier(value$getName())
		},
		
		compileField = function(value) {
			# If the value's parent is a JOIN clause, it must be JOIN ... USING ..., in which case we
	    # want only the name of the field.
			if (inherits(value$.parent, "JoinClause")) name <- compileIdentifier(value$name)
			else name <- compileIdentifier(value$table$getName(), value$name)
					
			if (inherits(value$.parent, "SelectClause")) {
				if (is.null(value$alias)) {
					if (!is.null(value$.parent$getOption("short.alias"))) 
						alias <- compileIdentifier(value$name)
					else alias <- prepareIdenitfier(str_c(value$table$.name, "_", value$name))
				} else alias <- value$alias
				str_c(name, "AS", alias, sep = " ")
			} else name
		},
		
		##
		# Non-atomic elements.
		##
		compileFunction = function(value) {
			func <- str_c(value$func, "(")
			func <- str_c(func, str_c(dispatchChildren(value), collapse = ", "), ")")
			
			# If this is the top-level application of a function in SELECT, alias it. 
			# Will follow a similar logic in compiling operators.
			if (inherits(value$.parent, "SelectClause"))
				str_c(func, "AS", compileAlias(value), sep = " ")
			else func
		},
		
		compileBinaryOperator = function(value) {
			operator <- str_c(
				dispatch(value$.children[[1]]), value$operator, dispatch(value$.children[[2]]), sep = " "
			)
			if (inherits(value$.parent, "SelectClause")) 
				str_c(operator, "AS", compileAlias(value), sep = " ")
			else operator
		},
		
		compileNegatableBinaryOperator = function(value) {
			# TODO: this should not be an option - it's required.
			if (value$negated) 
				value$operator <- switch(value$operator, "=" = "!=", IN = "NOT IN", IS = "NOT IS")
			compileBinaryOperator(value)
		},
		
		##
		# Clauses.
		# 
		# Currently this approach breaks with nesting. To account for this, add block() to formatter,
		# which adds a new element to .lines list and adds syntax to the block; end() 
		# pops that element.
		##

		compileSelectClause = function(value) {
		},
		
		compileUpdateClause = function(value) {
			formatter$begin("UPDATE")$down()$line(dispatchChildren(value, include = 1))
			formatter$up()$line("SET")$down()$line(
				str_c(dispatchChildren(value, exclude = 1), collapse = ", ")
			)$up()$end()
		},
		
		# These clauses should be lines, but check first.
		compileFromClause = function(value) {
			formatter$begin("FROM")$down()$line(str_c(dispatchChildren(value), collapse = " "))
			formatter$up()$end()
		},
		
		compileWhereClause = function(value) {
			formatter$begin("WHERE")$down()$line(str_c(dispatchChildren(value), collapse = " "))
			formatter$up()$end()
		},
		
		compileJoinClause = function(value) {
			if (value$type == "NATURAL JOIN") {
				formatter$begin("NATURAL JOIN")$down()$line(dispatchChildren(value, include = 1))
				return(formatter$up()$end())
			}
			if (value$type == "JOIN ON") {
				formatter$begin("JOIN")$down()$line(dispatchChildren(value, include = 1))$up()
				formatter$line("ON")$down()$lines(dispatchChildren(value, exclude = 1))$up()
				return(formatter$end())
			}
			if (value$type == "JOIN USING") {
				formatter$begin("JOIN")$down()$line(dispatchChildren(value, include = 1))$up()
				formatter$line("USING (")$down()
				return(formatter$lines(dispatchChildren(value, exclude = 1))$up()$line(")")$end())
			}
		},
		
		compileLimitClause = function(value) {
			formatter$begin("LIMIT")$toLine(dispatchChildren(value))$end()
		},
		
		compileOffsetClause = function(value) {
			formatter$begin("OFFSET")$toLine(dispatchChildren(value))$end()
		},
		
		##
		# Clause lists.
		## 
		compileClauseList = function(value) {
			dispatchChildren(value)
		},
		
		##
		# Statements.
		## 

		#' Several possible approaches to deduping functions here. This is simplest.
		compileStatement = function(value) {
			formatter$begin()
			if (!is.null(value$.parent)) formatter$down()$line('(', ' ')
			formatter$lines(dispatchChildren(value))
			if (!is.null(value$.parent)) formatter$up()$line(')')
			str_c(unlist(formatter$end()), collapse = '\n')
		},
		compileSelectStatement = function(value) {
			compileStatement(value)
		},
		compileUpdateStatement = function(value) {
			compileStatement(value)
		}
		
	)
)

MySQLCompiler <- setRefClass('MySQLCompiler',
	contains = c(
		'Compiler'
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
			initFields(
				identifier.collapse = ".",
				identifier.quote = "`"
			)
			prepare()
		},
		
		##
		# Clauses.
		##
		
		#' 
		compileSelectClause = function(value) {
			if (!is.null(value$getOption("distinct"))) formatter$begin("SELECT DISTINCT")$down()
			else formatter$begin("SELECT")$down()
			
			if (!is.null(value$getOption("count"))) 
				return(formatter$line("COUNT(*)")$up()$end())
			
			formatter$line(str_c(dispatchChildren(value), collapse = ", "))
			formatter$up()$end()
		}
		
	)
)

PostgresCompiler <- setRefClass('PostgresCompiler',
	contains = c(
		'Compiler'
	),
	methods = list(
		initialize = function() {
			callSuper()
			initFields(
				identifier.collapse = ".",
				identifier.quote = '"'
			)
			prepare()
		}
	)
)
			
