#evalq?

prepareIdentifier <- function(identifier) {
	identifier <- str_c(sprintf('`%s`', unlist(str_split(identifier, fixed('.')))), collapse = '.')
	return(identifier)
}

##
# Statements.
#
compileSelectStatement <- compileStatement

compileUpdateStatement <- compileStatement

compileCreateTableStatement <- function(statement) {
	
}

##
# Clauses.
#
compileSelectClause <- function(value, formatter, parent) {
	if (!is.null(value$getOption('distinct'))) lines <- formatter$line('SELECT DISTINCT')
	else lines <- formatter$line('SELECT')
	if (!is.null(value$getOption('count'))) {
		lines <- formatter$levelDown()$line(lines, 'COUNT(*)')
		formatter$levelUp()
		return(lines)
	}
	children <- str_c(sapply(value$.children, compile, formatter, value), collapse = ', ')
	lines <- formatter$levelDown()$line(lines, children)
	formatter$levelUp()
	return(lines)
}

compileFromClause <- function(value, formatter, parent) {
	lines <- formatter$line('FROM')
	children <- str_c(sapply(value$.children, compile, formatter, value), collapse = ', ')
	lines <- formatter$levelDown()$line(lines, children)
	formatter$levelUp()
	return(lines)
}

compileUpdateClause <- function(value, formatter, parent) {
	lines <- formatter$line('UPDATE')
	lines <- formatter$levelDown()$line(lines, compile(value$.children[[1]], formatter, parent))
	
	lines <- formatter$levelUp()$line(lines, 'SET')
	children <- str_c(sapply(value$.children[-1], compile, formatter, parent), collapse = ', ')
	lines <- formatter$levelDown()$line(lines, children)
	formatter$levelUp()
	return(lines)
}

compileWhereClause <- function(value, formatter, parent) {
	lines <- formatter$line('WHERE')
	children <- str_c(compileChildren(formatter, value), collapse = ' ')
	lines <- formatter$levelDown()$line(lines, children)
	formatter$levelUp()
	return(lines)
}

compileJoinClause <- function(value, formatter, parent) {
	if (value$getOption('type') == 'NATURAL JOIN') {
		lines <- formatter$line('NATURAL JOIN')
		lines <- formatter$line(lines, compileChildren(value, formatter, include = 1))
	} 
	
	if (value$getOption('type') == 'JOIN ON') {
		lines <- formatter$line('JOIN')
		lines <- formatter$levelDown()$line(lines, compileChildren(formatter, value, include = 1))
		lines <- formatter$levelUp()$line(lines, 'ON')
		lines <- formatter$levelDown()$line(
			lines, compileChildren(formatter, value, exclude = 1)
		)
		formatter$levelUp()
		return(lines)	
	} 
	
	if (value$getOption('type') == 'JOIN USING') {
		lines <- formatter$line('JOIN')
		lines <- formatter$levelDown()$line(lines, compileChildren(formatter, value, include = 1))
		lines <- formatter$levelUp()$line(lines, 'USING (')
		lines <- formatter$levelDown()$line(
			lines, compileChildren(formatter, value, exclude = 1)
		)
		lines <- formatter$levelUp()$line(lines, ')')
		return(lines)	
	}
}

compileGroupClause <- function(value, formatter, parent) {
	lines <- formatter$line('GROUP BY')
	lines <- formatter$addToLine(lines, str_c(compileChildren(formatter, value), collapse = ', '))
	return(lines)
}

compileHavingClause <- function(value, formatter, parent) {
	lines <- formatter$line('HAVING')
	children <- str_c(compileChildren(formatter, value), collapse = ' ')
	lines <- formatter$levelDown()$line(lines, children)
	formatter$levelUp()
	return(lines)
}

compileOrderClause <- function(value, formatter, parent) {
	lines <- formatter$line('ORDER BY')
	lines <- formatter$addToLine(lines, str_c(compileChildren(formatter, value), collapse = ', '))
	return(lines)
}

compileLimitClause <- function(value, formatter, parent) {
	lines <- formatter$line('LIMIT')
	lines <- formatter$addToLine(lines, str_c(compileChildren(formatter, value)))
	return(lines)
}

compileOffsetClause <- function(value, formatter, parent) {
	lines <- formatter$line('OFFSET')
	lines <- formatter$addToLine(lines, str_c(compileChildren(formatter, value)))
	return(lines)
}

compileClauseList <- function(value, formatter, parent) {
	sapply(value$.children, compile, formatter, value)
}

##
# Elements.
##

compileTable <- function(value, formatter, parent) {
	return(prepareIdentifier(value$getName()))
}

compileField <- function(value, formatter, parent) {
	# If the field is a top-level element in a JOIN, we're seeing USING, and thus need only the name
	# of this field.
	if (inherits(parent, 'JoinClause')) name <- prepareIdentifier(value$name)
	else name <- prepareIdentifier(str_c(value$table$getName(), '.', value$name))

	# If the field is a top-level element in a SELECT, we alias it.
	if (inherits(parent, 'SelectClause')) {
		if (is.null(value$alias)) {
			if (!is.null(parent$getOption('short.alias'))) alias <- sprintf('`%s`', value$name)
			else alias <- sprintf('`%s`', str_c(value$table$.name, '_', value$name))
		} else alias <- value$alias
		
		return(str_c(name, 'AS', alias, sep = ' '))
	}
	else return(name)
}

compileFunction <- function(value, formatter, parent) {
	func <- str_c(value$func, '(')
	func <- str_c(func, str_c(sapply(value$.children, compile, formatter, value), collapse = ', '), ')')
	
	if (!is.null(value$alias)) {
		alias <- sprintf("`%s`", value$alias)
	} else {
		fields <- value$findChildren('Field')
		if (length(fields) == 1) alias <- sprintf("`%s_%s`", tolower(value$func), fields[[1]]$name)
		else alias <- sprintf("`%s_%d`", tolower(value$func), formatter$getCounter(value$func))
	}
	
	func <- str_c(func, 'AS', alias, sep = ' ')
	return(func)
}
	
compilePostfixOperator <- function(value, formatter, parent) {
	return(str_c(compile(value$.children[[1]], formatter, parent), value$operator, sep = ' '))
}

compileBinaryOperator <- function(value, formatter, parent) {
	return(str_c(
		compile(value$.children[[1]], formatter, parent),
		value$operator,
		compile(value$.children[[2]], formatter, parent),
		sep = ' '
	))
}

compileNegatableBinaryOperator <- function(value, formatter, parent) {
	operator <- value$operator
	if (value$getOption('negated')) {
		if (operator == '==') value$operator <- "!="
		if (operator == 'IN') value$operator <- "NOT IN"
		if (operator == 'IS') value$operator <- "IS NOT"
	}
	return(compileBinaryOperator(value, formatter, parent))
}

##
# R literals.
#
compileCharacter <- function(value, formatter, parent) {
	if (length(value) > 1) return(formatTuple(value, TRUE))
	else return(sprintf("'%s'", value))
}

compileInteger <- function(value, formatter, parent) {
	if (length(value) > 1) return(formatTuple(value))
	else return(as.character(value))
}

compileNumeric <- compileInteger

compileLogical <- function(value, formatter, parent) {
	if (is.na(value)) return('NULL')
	else if (value) return('TRUE')
	else return('FALSE')
}