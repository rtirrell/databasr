ClauseElement <- setRefClass('ClauseElement',
	contains = c(
		'SQLObject'
	)
)

Scalar <- setRefClass('Scalar',
	contains = c('ClauseElement')
)

setMethod('%in%', c('Scalar', 'ANY'), function(x, table) {
	# Nested SELECT.
	if (inherits(table, 'Field')) table <- SelectStatement$new(NULL, table$distinct())
	NegatableBinaryOperator$new(operator = 'IN', left = x, right =  table)
})

setMethod('+', c('Scalar', 'ANY'), function(e1, e2) {
	if (missing(e2)) PostfixOperator$new(operator = "ASC", left = e1)
	else BinaryOperator$new(operator = "+", left = e1, right = e2)
})

setMethod('-', c('Scalar', 'ANY'), function(e1, e2) {
	if (missing(e2)) PostfixOperator$new(operator = "DESC", left = e1)
	else BinaryOperator$new(operator = "-", left = e1, right = e2)
})

setMethod('<', c('Scalar', 'ANY'), function(e1, e2) {
	BinaryOperator$new(operator = "<", left = e1, right = e2)
})

setMethod('>', c('Scalar', 'ANY'), function(e1, e2) {
	BinaryOperator$new(operator = ">", left = e1, right = e2)
})

setMethod('==', c('Scalar', 'ANY'), function(e1, e2) {
	NegatableBinaryOperator$new(operator = "=", left = e1, right = e2)
})

setMethod('!=', c('Scalar', 'ANY'), function(e1, e2) {
	operator <- NegatableBinaryOperator$new(operator = '==', left = e1, right = e2)
	operator$setOptions(negated = TRUE)
	operator
})

setMethod('&', c('Scalar', 'ANY'), function(e1, e2) {
	BinaryOperator$new(operator = "AND", left = e1, right = e2)
})

setMethod('|', c('Scalar', 'ANY'), function(e1, e2) {
	BinaryOperator$new(operator = "OR", left = e1, right = e2)
})

##
# Functions.
##
setMethod('max', 'Scalar', function(x) {
	Function$new(func = 'MAX', x)
})

setMethod('min', 'Scalar', function(x) {
	Function$new(func = 'MIN', x)
})

setMethod('unique', 'Scalar', function(x) {
	Function$new(func = 'DISTINCT', x)
})

setMethod('length', 'Scalar', function(x) {
	Function$new(func = 'COUNT', x)
})

setMethod('tolower', 'Scalar', function(x) {
	Function$new(func = 'LOWER', x)
})

setMethod('toupper', 'Scalar', function(x) {
	Function$new('UPPER', x)
})

#setMethod('str_c', c('Scalar', 'ANY'), function(..., sep = '', collapse = NULL) {
#	if (sep == '') Function$new(func = 'CONCAT', ...)
#	Function$new('CONCACT_WS', sep, ...)
#})

`[.Scalar` <- function(x, i, j, drop = FALSE) {
	if (!inherits(x$type, "StringType"))
		stop("Cannot index a non-string field with `[`.")
	return(Function$new('SUBSTRING', x, i[1], i[length(i)] - i[1]))
}

Function <- setRefClass('Function',
	contains = c(
		'Scalar'
	),
	fields = c(
		'func',
		'alias'
	),
	methods = list(
		# TODO: consider order of parent in constructor for Scalar.
		initialize = function(func = NULL, ...) {
			callSuper()
			initFields(func = func, alias = NULL)
			addChildren(...)
			.self
		},
		as = function(name) {
			alias <<- name
			.self
		}
	)
)

Operator <- setRefClass('Operator',
	contains = c(
		'Scalar'
	),
	fields = c(
		'operator'
	),
	methods = list(
		initialize = function(operator = NULL) {
			callSuper()
			initFields(operator = operator)
		}
	)
)

# TODO: add alias to BinaryOperator?
BinaryOperator <- setRefClass('BinaryOperator',
	contains = c(
		'Operator'
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL, right = NULL) {
			initFields()
			callSuper(operator = operator)
			addChildren(left, right)
		}
	)
)

# Class for any binary operator (==, IN, IS) that is negatable (i.e. to !=, NOT IN, IS NOT).
NegatableBinaryOperator <- setRefClass('NegatableBinaryOperator',
	contains = c(
		'BinaryOperator'
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL, right = NULL) {
			callSuper(operator, left, right)
			if (operator == "=" && is.na(right)) operator <<- "IS"

			setOptions(negated = FALSE)
		}
	)
)

setMethod('!', 'NegatableBinaryOperator', function(x) {
	x$setOptions(negated = !x$getOption('negated'))
	x
})


# Class for any operator that is applied postfix to its argument (e.g. DESC or ASC).
PostfixOperator <- setRefClass('PostfixOperator',
	contains = c(
		'Operator'
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL) {
			callSuper(operator = operator)
			addChildren(left)
			.self
		}
	)
)