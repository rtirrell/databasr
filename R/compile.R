compileVector <- function(value, quote = FALSE) {
	if (quote) value <- sprintf("'%s'", value)
	
	return(sprintf('(%s)', str_c(value, collapse = ', ')))
}

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
		# I believe this is shared by all DBMSes that we will target.
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
		
		#' Compile an alias for an expression.
		compileAlias = function(value) {
			if (is.null(value$alias)) {
				fields <- value$findChildren("Field")
				# A field name composed this way could be duplicated. We could track field names on the 
				# parent SELECT and perform checks. But we'd like *all* aliases to be treated the same way,
				# not just all following the first (e.g., if we were to add "_n").
				if (length(fields) == 1) 
					compileIdentifier(str_c(tolower(value$func), "_", fields[[1]]$name))
				else 
					compileIdentifier(str_c(tolower(value$func), "_", formatter$getCounter(value$func)))
			} else compileIdentifier(value$alias)
		},
		
		##
		# R literals.
		##
		compileInteger = function(value) {
			if (length(value) > 1) compileVector(value)
			else value
		},
		
		compileNumeric = function(value) {
			if (length(value) > 1) compileVector(value)
			else value
		},
		
		compileCharacter = function(value) {
			if (length(value) > 1) compileVector(value, TRUE)
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
			name <- compileIdentifier(value$getName())
			if (!is.null(value$.alias)) str_c(name, "AS", compileIdentifier(value$.alias), sep = " ")
			else name
		},
		
		compileField = function(value) {
			# If the value's parent is a JOIN clause, it must be JOIN ... USING ..., in which case we
			# want only the name of the field.
			if (inherits(value$.parent, "JoinClause")) name <- compileIdentifier(value$name)
			else {
				if (inherits(value$table, "Table"))
					name <- compileIdentifier(value$table$getCompileName(), value$name)
				else
					name <- compileIdentifier(value$table$getName(), value$name)
			}
					
			if (inherits(value$.parent, "SelectClause")) {
				if (is.null(value$alias)) {
					if (!is.null(value$.parent$getOption("short.alias"))) 
						alias <- compileIdentifier(value$name)
					else alias <- compileIdentifier(str_c(value$table$.name, "_", value$name))
				} else alias <- value$alias
				str_c(name, "AS", alias, sep = " ")
			} else name
		},
		
		##
		# Non-atomic elements.
		##

		compileTupleElement = function(value) {
			str_c("(", str_c(dispatchChildren(value), collapse = ", "), ")")
		},
		
		compileList = function(value) {
			str_c("(", str_c(unlist(sapply(value, dispatch)), collapse = ", "), ")")
		},
		
		compileFunctionElement = function(value) {
			func <- str_c(value$func, "(")
			func <- str_c(func, str_c(dispatchChildren(value), collapse = ", "), ")")
			
			# If this is the top-level application of a function in SELECT, alias it. 
			# Will follow a similar logic in compiling operators.
			if (inherits(value$.parent, "SelectClause"))
				str_c(func, "AS", compileAlias(value), sep = " ")
			else func
		},
		
		compilePostfixOperatorElement = function(value) {
			str_c(dispatchChildren(value), value$operator, sep = " ")
		},
		
		compileBinaryOperatorElement = function(value, operator = value$operator) {
			operator <- str_c(
				dispatch(value$.children[[1]]), operator, dispatch(value$.children[[2]]), sep = " "
			)
			if (inherits(value$.parent, "SelectClause")) 
				str_c(operator, "AS", compileAlias(value), sep = " ")
			else operator
		},
		
		# TODO: problem with IS NOT NULL.
		compileNegatableBinaryOperatorElement = function(value) {
			if (value$negated) 
				operator <- switch(value$operator, "=" = "!=", IN = "NOT IN", IS = "IS NOT")
			else operator <- value$operator
			compileBinaryOperatorElement(value, operator)
		},
		
		##
		# Clauses.
		# 
		# This formatting approach should continue to work with nesting - as ".lines"
		# on the formatter is stacklike and modifications will never be interleaved. 
		# That said, it's not yet covered by tests.
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
			formatter$begin("FROM")$down()$line(str_c(dispatchChildren(value), collapse = ", "))
			formatter$up()$end()
		},
		
		#' Compile a WHERE clause.
		#' 
		#' Note that in the current expression generation scheme several clause types only ever 
		#' have a single child as their binding clause -- the first (WHERE, HAVING) or second 
		#' (JOIN ON) one. For the sake of right now, we nonetheless collapse by " ".
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
		
		compileGroupClause = function(value) {
			formatter$begin("GROUP BY")$down()$line(str_c(dispatchChildren(value), collapse = ", "))
			formatter$up()$end()
		},
		
		compileHavingClause = function(value) {
			formatter$begin("HAVING")$down()$line(str_c(dispatchChildren(value), collapse = " "))
			formatter$up()$end()
		},
		
		compileOrderClause = function(value) {
			formatter$begin("ORDER BY")$down()$line(str_c(dispatchChildren(value), collapse = ", "))
			formatter$up()$end()
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
			# We only want to parenthetize a statement if its included in a clause or other
			# element.
			if (!is.null(value$.parent) && !inherits(value$.parent, "Statement")) 
				formatter$line("(")$down()
			
			formatter$lines(dispatchChildren(value))
			
			if (!is.null(value$.parent) && !inherits(value$.parent, "Statement")) 
				formatter$up()$line(")")
			
			str_c(unlist(formatter$end()), collapse = "\n")
		},
		compileSelectStatement = function(value) {
			compileStatement(value)
		},
		compileUpdateStatement = function(value) {
			compileStatement(value)
		},
		compileStatementOperator = function(value) {
			formatter$begin()
			formatter$lines(dispatchChildren(value, include = 1))
			formatter$line(value$operator)
			formatter$lines(dispatchChildren(value, include = 2))
			formatter$end()
		}
		
	)
)

MySQLCompiler <- setRefClass("MySQLCompiler",
	contains = c(
		"Compiler"
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
		
		#' Compile a `SELECT` clause.
		#' 
		#' MySQL's behavior needs to diverge from (e.g.) Postgres', as Postgres should never issue
		#' `COUNT(*)`.
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
			
