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
compileSelectClause <- function(value, compiler, parent) {
	if (!is.null(value$getOption('distinct'))) lines <- compiler$line('SELECT DISTINCT')
	else lines <- compiler$line('SELECT')
	if (!is.null(value$getOption('count'))) {
		lines <- compiler$levelDown()$line(lines, 'COUNT(*)')
		compiler$levelUp()
		return(lines)
	}
	children <- str_c(sapply(value$.children, compile, compiler, value), collapse = ', ')
	lines <- compiler$levelDown()$line(lines, children)
	compiler$levelUp()
	return(lines)
}

compileFromClause <- function(value, compiler, parent) {
	lines <- compiler$line('FROM')
	children <- str_c(sapply(value$.children, compile, compiler, value), collapse = ', ')
	lines <- compiler$levelDown()$line(lines, children)
	compiler$levelUp()
	return(lines)
}

compileUpdateClause <- function(value, compiler, parent) {
	lines <- compiler$line('UPDATE')
	lines <- compiler$levelDown()$line(lines, compile(value$.children[[1]], compiler, parent))
	
	lines <- compiler$levelUp()$line(lines, 'SET')
	children <- str_c(sapply(value$.children[-1], compile, compiler, parent), collapse = ', ')
	lines <- compiler$levelDown()$line(lines, children)
	compiler$levelUp()
	return(lines)
}

compileWhereClause <- function(value, compiler, parent) {
	lines <- compiler$line('WHERE')
	children <- str_c(compileChildren(compiler, value), collapse = ' ')
	lines <- compiler$levelDown()$line(lines, children)
	compiler$levelUp()
	return(lines)
}

compileJoinClause <- function(value, compiler, parent) {
	if (value$getOption('type') == 'NATURAL JOIN') {
		lines <- compiler$line('NATURAL JOIN')
		lines <- compiler$line(lines, compileChildren(value, compiler, include = 1))
	} 
	
	if (value$getOption('type') == 'JOIN ON') {
		lines <- compiler$line('JOIN')
		lines <- compiler$levelDown()$line(lines, compileChildren(compiler, value, include = 1))
		lines <- compiler$levelUp()$line(lines, 'ON')
		lines <- compiler$levelDown()$line(
			lines, compileChildren(compiler, value, exclude = 1)
		)
		compiler$levelUp()
		return(lines)	
	} 
	
	if (value$getOption('type') == 'JOIN USING') {
		lines <- compiler$line('JOIN')
		lines <- compiler$levelDown()$line(lines, compileChildren(compiler, value, include = 1))
		lines <- compiler$levelUp()$line(lines, 'USING (')
		lines <- compiler$levelDown()$line(
			lines, compileChildren(compiler, value, exclude = 1)
		)
		lines <- compiler$levelUp()$line(lines, ')')
		return(lines)	
	}
}

compileGroupClause <- function(value, compiler, parent) {
	lines <- compiler$line('GROUP BY')
	lines <- compiler$addToLine(lines, str_c(compileChildren(compiler, value), collapse = ', '))
	return(lines)
}

compileHavingClause <- function(value, compiler, parent) {
	lines <- compiler$line('HAVING')
	children <- str_c(compileChildren(compiler, value), collapse = ' ')
	lines <- compiler$levelDown()$line(lines, children)
	compiler$levelUp()
	return(lines)
}

compileOrderClause <- function(value, compiler, parent) {
	lines <- compiler$line('ORDER BY')
	lines <- compiler$addToLine(lines, str_c(compileChildren(compiler, value), collapse = ', '))
	return(lines)
}

compileLimitClause <- function(value, compiler, parent) {
	lines <- compiler$line('LIMIT')
	lines <- compiler$addToLine(lines, str_c(compileChildren(compiler, value)))
	return(lines)
}

compileOffsetClause <- function(value, compiler, parent) {
	lines <- compiler$line('OFFSET')
	lines <- compiler$addToLine(lines, str_c(compileChildren(compiler, value)))
	return(lines)
}

compileClauseList <- function(value, compiler, parent) {
	sapply(value$.children, compile, compiler, value)
}

##
# Elements.
##

compileTable <- function(value, compiler, parent) {
	return(prepareIdentifier(value$getName()))
}

compileField <- function(value, compiler, parent) {
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

compileFunction <- function(value, compiler, parent) {
	func <- str_c(value$func, '(')
	func <- str_c(func, str_c(sapply(value$.children, compile, compiler, value), collapse = ', '), ')')
	
	if (!is.null(value$alias)) {
		alias <- sprintf("`%s`", value$alias)
	} else {
		fields <- value$findChildren('Field')
		if (length(fields) == 1) alias <- sprintf("`%s_%s`", tolower(value$func), fields[[1]]$name)
		else alias <- sprintf("`%s_%d`", tolower(value$func), compiler$getCounter(value$func))
	}
	
	func <- str_c(func, 'AS', alias, sep = ' ')
	return(func)
}
	
compilePostfixOperator <- function(value, compiler, parent) {
	return(str_c(compile(value$.children[[1]], compiler, parent), value$operator, sep = ' '))
}

compileBinaryOperator <- function(value, compiler, parent) {
	return(str_c(
		compile(value$.children[[1]], compiler, parent),
		value$operator,
		compile(value$.children[[2]], compiler, parent),
		sep = ' '
	))
}

compileNegatableBinaryOperator <- function(value, compiler, parent) {
	operator <- value$operator
	if (value$getOption('negated')) {
		if (operator == '==') value$operator <- "!="
		if (operator == 'IN') value$operator <- "NOT IN"
		if (operator == 'IS') value$operator <- "IS NOT"
	}
	return(compileBinaryOperator(value, compiler, parent))
}

##
# R literals.
#
compileCharacter <- function(value, compiler, parent) {
	if (length(value) > 1) return(formatTuple(value, TRUE))
	else return(sprintf('"%s"', value))
}

compileInteger <- function(value, compiler, parent) {
	if (length(value) > 1) return(formatTuple(value))
	else return(as.character(value))
}

compileNumeric <- compileInteger

compileLogical <- function(value, compiler, parent) {
	if (is.na(value)) return('NULL')
	else if (value) return('TRUE')
	else return('FALSE')
}