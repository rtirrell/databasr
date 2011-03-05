#' Class implementing DBMS-specific compilation behavior (syntax and formatting).
#' If a need arises to add behavior between preparation and this step, we'll add a
#' database-specific preparation step that follows the database-agnostic step, making the workflow
#' composition, agnostic preparation, specific preparation, compilation/formatting.
Compiler <- setRefClass('Compiler',
	fields = c(
		'statement',
		'formatter',
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
		
		#' Setup the compiler.
		#' 
		#' This method is called from `initialize` in subclasses.
		prepare = function() {
			identifier.base <<- str_c(identifier.quote, '%s', identifier.quote)
			.self
		},
		
		#' Dispatch compilation on the statement and return a formatted string.
		compile = function() {
			lines <- dispatch(statement)
			formatter$finish(lines)
		},
		
		#' Dispatch a compilation function based on class.
		dispatch = function(value) {
			method.name <- str_c('compile', capitalize(class(value)))
			
			# Here we can't just get the function, as it (may) need to be inferred.
			method <- findMethods('$')$envRefClass(.self, as.character(method.name))
			method(value)
		},
		
		#' Dispatch a compilation function to children of the given value.
		dispatch_children = function(value, include = NULL, exclude = NULL) {
			if (is.null(include)) {
				indices <- seq_along(value$.children)
				if (!is.null(exclude)) indices <- indices[-exclude]
			} else indices <- include
			
			unlist(sapply(value$.children[indices], dispatch))
		},
		
		#' Compile an SQL identifier according to the standard of the current database.
		compileIdentifier = function(...) {
			args <- paste(list(...), collapse = identifier.collapse)
			str_c(
				sprintf(
					identifier.base, 
					unlist(str_split(args, fixed(identifier.collapse)))
				), 
				collapse = identifier.collapse
			)
		},
		
		#' Compile an `AsIs` object.
		compileAsIs = function(value) {
			value
		},
		
		#' Compile an alias for an expression.
		compileAlias = function(value) {
			if (is.null(value$alias)) {
				if (inherits(value, 'OperatorElement')) 
					operfun <- .OPERATOR.NAMES[[value$operator]] 
				else 
					operfun <- value$func 
				
				fields <- value$find_children('Field')
				# A field name composed this way could be duplicated. 
				# We could track field names on the parent SELECT and perform checks. 
				# But we'd like *all* aliases to be treated the same way,
				# not just all following the first (e.g., if we were to add '_n').
				if (length(fields) == 1) 
					compileIdentifier(str_c(tolower(operfun), '_', fields[[1]]$name))
				else 
					compileIdentifier(
						str_c(tolower(operfun), '_', formatter$get_counter(operfun))
					)
			} else 
				compileIdentifier(value$alias)
		},
		
		##
		# R literals.
		##

		#' Compile a vector.
		compileVector = function(value, quote = FALSE) {
			if (quote) 
				value <- sprintf("'%s'", value)
			
			return(sprintf('(%s)', str_c(value, collapse = ', ')))
		},
		
		#' Compile an integer vector.
		compileInteger = function(value) {
			if (length(value) > 1) 
				compileVector(value)
			else 
				value
		},
		
		#' Compile a numeric vector.
		compileNumeric = function(value) {
			if (length(value) > 1) 
				compileVector(value)
			else 
				value
		},
		
		#' Compile a character vector.
		compileCharacter = function(value) {
			if (length(value) > 1) 
				compileVector(value, TRUE)
			else 
				sprintf("'%s'", value)
		},
		
		#' Compile a logical vector of length one.
		compileLogical = function(value) {
			if (is.na(value)) 
				'NULL'
			else if (value) 
				'TRUE'
			else 
				'FALSE'
		},
		
		##
		# Atomic elements.
		# 
		#' Compile a table.
		compileTable = function(value) {
			name <- compileIdentifier(value$get_name())
			if (!is.null(value$.alias)) 
				str_c(name, 'AS', compileIdentifier(value$.alias), sep = ' ')
			else 
				name
		},
		
		#' Compile a field.
		compileField = function(value) {
			# If the value's parent is a JOIN clause, it must be 
			# JOIN ... USING ..., in which case we want only the name of the field.
			if (inherits(value$.parent, 'JoinClause')) 
				name <- compileIdentifier(value$name)
			else {
				if (inherits(value$table, 'Table'))
					name <- compileIdentifier(
						value$table$get_compile_name(), value$name
					)
				else
					name <- compileIdentifier(value$table$get_name(), value$name)
			}
					
			if (inherits(value$.parent, 'SelectClause')) {
				if (is.null(value$alias)) {
					if (value$name %in% value$.parent$get_option('full.alias'))
						alias <- compileIdentifier(
							str_c(value$table$.name, '_', value$name)
						)
					else 
						alias <- compileIdentifier(value$name)
				} else 
					alias <- value$alias
				str_c(name, 'AS', alias, sep = ' ')
			} else 
				name
		},
		
		##
		# Non-atomic elements.
		##

		# Tuples and lists are handled nearly identically - for a tuple, 
		# we dispatch compilation to its children. 
		# For a list, we apply compilation to its elements.

		#' Compile a tuple.
		compileTupleElement = function(value) {
			str_c('(', str_c(dispatch_children(value), collapse = ', '), ')')
		},
		
		#' Compile a list.
		compileList = function(value) {
			str_c('(', str_c(unlist(sapply(value, dispatch)), collapse = ', '), ')')
		},
		
		#' Compile a function.
		compileFunctionElement = function(value) {
			func <- str_c(value$func, '(')
			func <- str_c(
				func, str_c(dispatch_children(value), collapse = ', '), ')'
			)
			
			# If this is the top-level application of a function in SELECT, alias it. 
			# Will follow a similar logic in compiling operators.
			if (inherits(value$.parent, 'SelectClause'))
				str_c(func, 'AS', compileAlias(value), sep = ' ')
			else 
				func
		},
		
		#' Compile a prefix operator - e.g. DISTINCT.
		compilePrefixOperatorElement = function(value) {
			compiled <- str_c(
				value$operator, 
				str_c(dispatch_children(value), collapse = ', '), sep = ' '
			)
			if (inherits(value$.parent, 'SelectClause'))
				str_c(compiled, 'AS', compileAlias(value), sep = ' ')
			else
				compiled
		},
		
		#' Compile a postfix operator - e.g. ASC.
		compilePostfixOperatorElement = function(value) {
			str_c(dispatch_children(value), value$operator, sep = ' ')
		},
		
		#' Compile a binary operator.
		compileBinaryOperatorElement = function(value) {
			compiled <- str_c(
				dispatch(value$.children[[1]]), 
				value$operator, 
				dispatch(value$.children[[2]]), 
				sep = ' '
			)
			if (inherits(value$.parent, 'SelectClause')) 
				str_c(compiled, 'AS', compileAlias(value), sep = ' ')
			else 
				compiled
		},
		
		#' Compile a negatable binary operator.
		#' 
		#' This function just calls `compileBinaryOperatorElement`.
		compileNegatableBinaryOperatorElement = function(value) {
			compileBinaryOperatorElement(value)
		},
		
		##
		# Clauses.
		##
		
		#' Compile a SELECT clause. 
		compileSelectClause = function(value) {
		},
		
		#' Compile an UPDATE ... SET clause.
		compileUpdateClause = function(value) {
			formatter$begin('UPDATE')$down()
			formatter$line(dispatch_children(value, include = 1))
			formatter$up()$line('SET')$down()$line(
				str_c(dispatch_children(value, exclude = 1), collapse = ', ')
			)$up()$end()
		},
		
		#' Compile a FROM clause.
		compileFromClause = function(value) {
			formatter$begin('FROM')$down()
			formatter$line(str_c(dispatch_children(value), collapse = ', '))
			formatter$up()$end()
		},
		
		# Note that in the current expression generation scheme 
		# several clause types only ever have a single child as 
		# their binding clause -- the first (WHERE, HAVING) or second 
		# (JOIN ON) one. 
		# For the sake of right now, we nonetheless collapse by ' '.

		#' Compile a WHERE clause.
		compileWhereClause = function(value) {
			formatter$begin('WHERE')$down()
			formatter$line(str_c(dispatch_children(value), collapse = ' '))
			formatter$up()$end()
		},
		
		#' Compile a [NATURAL] JOIN [ON] clause.
		compileJoinClause = function(value) {
			if (value$type == 'NATURAL JOIN') {
				formatter$begin('NATURAL JOIN')$down()
				formatter$line(dispatch_children(value, include = 1))
				return(formatter$up()$end())
			}
			if (value$type == 'JOIN ON') {
				formatter$begin('JOIN')$down()
				formatter$line(dispatch_children(value, include = 1))$up()
				formatter$line('ON')$down()
				formatter$lines(dispatch_children(value, exclude = 1))$up()
				return(formatter$end())
			}
			if (value$type == 'JOIN USING') {
				formatter$begin('JOIN')$down()
				formatter$line(dispatch_children(value, include = 1))$up()
				formatter$line('USING (')$down()
				formatter$lines(dispatch_children(value, exclude = 1))
				return(formatter$up()$line(')')$end())
			}
		},
		
		#' Compile a GROUP BY clause.
		compileGroupClause = function(value) {
			formatter$begin('GROUP BY')$down()
			formatter$line(str_c(dispatch_children(value), collapse = ', '))
			formatter$up()$end()
		},
		
		#' Compile a HAVING clause.
		compileHavingClause = function(value) {
			formatter$begin('HAVING')$down()
			formatter$line(str_c(dispatch_children(value), collapse = ' '))
			formatter$up()$end()
		},
		
		#' Compile an ORDER clause.
		compileOrderClause = function(value) {
			formatter$begin('ORDER BY')$down()
			formatter$line(str_c(dispatch_children(value), collapse = ', '))
			formatter$up()$end()
		},
		
		#' Compile a LIMIT clause.
		compileLimitClause = function(value) {
			formatter$begin('LIMIT')$to_line(dispatch_children(value))$end()
		},
		
		#' Compile an OFFSET clause.
		compileOffsetClause = function(value) {
			formatter$begin('OFFSET')$to_line(dispatch_children(value))$end()
		},
		
		#' Compile a clause list.
		#' 
		#' Since all clauses manipulate the formatter directly, there is no return value.
		compileClauseList = function(value) {
			dispatch_children(value)
		},
		
		##
		# Statements.
		## 

		#' Compile a statement.
		#' 
		#' Subclasses of `Statement` also use this function, as no special treatment 
		#' seems necessary yet.
		compileStatement = function(value) {
			formatter$begin()
			# We only want to parenthetize a statement if its included in a 
			# clause or other element.
			if (!is.null(value$.parent) && !inherits(value$.parent, 'Statement')) 
				formatter$line('(')$down()
			
			formatter$lines(dispatch_children(value))
			
			if (!is.null(value$.parent) && !inherits(value$.parent, 'Statement')) 
				formatter$up()$line(')')
			
			str_c(unlist(formatter$end()), collapse = '\n')
		},
		
		#' Compile a SELECT statement.
		compileSelectStatement = function(value) {
			compileStatement(value)
		},
		
		#' Compile an UPDATE statement.
		compileUpdateStatement = function(value) {
			compileStatement(value)
		},
		
		#' Compile a statement operator.
		compileStatementOperator = function(value) {
			formatter$begin()
			formatter$lines(dispatch_children(value, include = 1))
			formatter$line(value$operator)
			formatter$lines(dispatch_children(value, include = 2))
			formatter$end()
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
				identifier.collapse = '.',
				identifier.quote = '`'
			)
			prepare()
		},
		
		##
		# Clauses.
		##
		
		#' Compile a SELECT clause.
		#' 
		#' MySQL's behavior needs to diverge from (e.g.) Postgres', as Postgres should never issue
		#' `COUNT(*)`.
		compileSelectClause = function(value) {
			if (!is.null(value$get_option('DISTINCT')))
				formatter$begin('SELECT DISTINCT')$down()
			else 
				formatter$begin('SELECT')$down()
			
			if (!is.null(value$get_option('count'))) 
				return(formatter$line('COUNT(*)')$up()$end())
			
			formatter$line(str_c(dispatch_children(value), collapse = ', '))
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
				identifier.collapse = '.',
				identifier.quote = '"'
			)
			prepare()
		}
	)
)
			
