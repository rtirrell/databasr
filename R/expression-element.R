ClauseElement <- setRefClass('ClauseElement',
	contains = c(
		'SQLObject'
	)
)

Scalar <- setRefClass('Scalar',
	contains = c('ClauseElement')
)

setMethod('%in%', c('Scalar', 'ANY'), function(x, table) {
	NegatableBinaryOperator$new("IN", x, table)
})

setMethod('+', c('Scalar', 'ANY'), function(e1, e2) {
	if (missing(e2)) PostfixOperator$new("ASC", e1)
	else BinaryOperator$new("+", e1, e2)
})

setMethod('-', c('Scalar', 'ANY'), function(e1, e2) {
	if (missing(e2)) PostfixOperator$new("DESC", e1)
	else BinaryOperator$new("-", e1, e2)
})

setMethod('<', c('Scalar', 'ANY'), function(e1, e2) {
	BinaryOperator$new("<", e1, e2)
})

setMethod('>', c('Scalar', 'ANY'), function(e1, e2) {
	BinaryOperator$new(">", e1, e2)
})

setMethod('==', c('Scalar', 'ANY'), function(e1, e2) {
	NegatableBinaryOperator$new("=", e1, e2)
})

setMethod('!=', c('Scalar', 'ANY'), function(e1, e2) {
	operator <- NegatableBinaryOperator$new('==', e1, e2)
	operator$setOptions(negated = TRUE)
	operator
})

setMethod('&', c('Scalar', 'ANY'), function(e1, e2) {
	BinaryOperator$new("AND", e1, e2)
})

setMethod('|', c('Scalar', 'ANY'), function(e1, e2) {
	BinaryOperator$new("OR", e1, e2)
})

##
# Functions.
##
setMethod('max', 'Scalar', function(x) {
	Function$new('MAX', x)
})

setMethod('min', 'Scalar', function(x) {
	Function$new('MIN', x)
})

setMethod('unique', 'Scalar', function(x) {
	Function$new('DISTINCT', x)
})

setMethod('length', 'Scalar', function(x) {
	Function$new('COUNT', x)
})

setMethod('tolower', 'Scalar', function(x) {
	Function$new('LOWER', x)
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
	# TODO: check indices.
	# Maybe just use from, to as args and figure the RDBMS-specific args later.
	Function$new('SUBSTRING', x, i[1], i[length(i)] - i[1])
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
		initialize = function(func = NULL, ...) {
			callSuper()
			initFields(func = func, alias = NULL)
			addChildren(...)
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

# TODO: add alias to BinaryOperator, or alias on Operator.
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
			callSuper(operator = operator, left = left, right = right)
			if (operator == "=" && is.na(right)) operator <<- "IS"

			setOptions(negated = FALSE)
		}
	)
)

setMethod('!', 'NegatableBinaryOperator', function(x) {
	x$setOptions(negated = !x$getOption('negated'))
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
		}
	)
)