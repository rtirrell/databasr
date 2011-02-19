Element <- setRefClass('Element',
	contains = c(
		"SQLObject"
	),
	methods = list(
		initialize = function() {
			callSuper()
		}
	)
)

SelectableElement <- setRefClass("SelectableElement",
	contains = c(
		"Element"
	)
)

TupleElement <- setRefClass("TupleElement",
	contains = c(
		"SelectableElement"
	),
	methods = list(
		initialize = function(...) {
			callSuper()
			addChildren(...)
			.self
		}
	)
)
tuple <- function(...) TupleElement$new(...)

setMethod('%in%', c("SelectableElement", 'ANY'), function(x, table) {
	if (!inherits(table, "TupleElement")) table <- do.call(TupleElement$new, as.list(table))
	NegatableBinaryOperatorElement$new("IN", x, table)
})

setMethod('+', c("SelectableElement", 'ANY'), function(e1, e2) {
	if (missing(e2)) PostfixOperatorElement$new("ASC", e1)
	else BinaryOperatorElement$new("+", e1, e2)
})

setMethod('-', c("SelectableElement", 'ANY'), function(e1, e2) {
	if (missing(e2)) PostfixOperatorElement$new("DESC", e1)
	else BinaryOperatorElement$new("-", e1, e2)
})

setMethod('<', c("SelectableElement", 'ANY'), function(e1, e2) {
	BinaryOperatorElement$new("<", e1, e2)
})

setMethod('>', c("SelectableElement", 'ANY'), function(e1, e2) {
	BinaryOperatorElement$new(">", e1, e2)
})

setMethod('==', c("SelectableElement", 'ANY'), function(e1, e2) {
	NegatableBinaryOperatorElement$new("=", e1, e2)
})

setMethod('!=', c("SelectableElement", 'ANY'), function(e1, e2) {
	NegatableBinaryOperatorElement$new("=", e1, e2, TRUE)
})

setMethod('&', c("SelectableElement", 'ANY'), function(e1, e2) {
	BinaryOperatorElement$new("AND", e1, e2)
})

setMethod('|', c("SelectableElement", 'ANY'), function(e1, e2) {
	BinaryOperatorElement$new("OR", e1, e2)
})

##
# FunctionElements.
##
setMethod('max', "SelectableElement", function(x) {
	FunctionElement$new('MAX', x)
})

setMethod('min', "SelectableElement", function(x) {
	FunctionElement$new('MIN', x)
})

setMethod('unique', "SelectableElement", function(x) {
	FunctionElement$new('DISTINCT', x)
})

#' A \code{\link{length}} method to support generation of 'COUNT' functions.
setMethod('length', "SelectableElement", function(x) {
	FunctionElement$new('COUNT', x)
})

setMethod('tolower', "SelectableElement", function(x) {
	FunctionElement$new('LOWER', x)
})

setMethod('toupper', "SelectableElement", function(x) {
	FunctionElement$new('UPPER', x)
})

# TODO: haven't yet decided how to work with concat. Probably using paste.
#setMethod('paste', c("SelectableElement"), function(x, ..., sep = '', collapse = NULL) {
#	if (sep == '') FunctionElement$new(func = 'CONCAT', ...)
#	FunctionElement$new('CONCACT_WS', sep, ...)
#})

`[.SelectableElement` <- function(x, i, j, drop = FALSE) {
	if (!inherits(x$type, "StringType")) {
		# But we really can, and I'm not even sure it's bad form.
		stop("Cannot index a non-string field with `[`.")
	}
	FunctionElement$new("SUBSTRING", x, i[1], i[length(i)] - i[1] + 1)
}

OperFunElement <- setRefClass("OperFunElement",
	contains = c(
		"SelectableElement"
	),
	fields = c(
		"alias"
	),
	methods = list(
		initialize = function(alias = NULL) {
			initFields(alias = alias)
			callSuper()
		},
		as = function(name) {
			alias <<- name
			.self
		}
	)
)

FunctionElement <- setRefClass('FunctionElement',
	contains = c(
		"OperFunElement"
	),
	fields = c(
		"func"
	),
	methods = list(
		initialize = function(func = NULL, ...) {
			callSuper()
			initFields(func = func)
			addChildren(...)
		}
	)
)

OperatorElement <- setRefClass("OperatorElement",
	contains = c(
		"OperFunElement"
	),
	fields = c(
		"operator"
	),
	methods = list(
		initialize = function(operator = NULL) {
			callSuper()
			initFields(operator = operator)
		}
	)
)

# TODO: add alias to BinaryOperatorElement, or alias on OperatorElement.
BinaryOperatorElement <- setRefClass("BinaryOperatorElement",
	contains = c(
		"OperatorElement"
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL, right = NULL) {
			initFields()
			callSuper(operator = operator)
			addChildren(left, right)
		}
	)
)


#' Class for any binary operator (==, IN, IS) that is negatable (i.e. to !=, NOT IN, IS NOT).
#' 
#' I know this name is a little on the long side, but I like consistency.
NegatableBinaryOperatorElement <- setRefClass("NegatableBinaryOperatorElement",
	contains = c(
		"BinaryOperatorElement"
	),
	fields = c(
		"negated"
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL, right = NULL, negated = FALSE) {
			callSuper(operator = operator, left = left, right = right)
			# TODO: use identical?
			suppressWarnings({
				if (operator == "=" && is.na(right)) operator <<- "IS"
			})
			initFields(negated = negated)
		}
	)
)

setMethod("!", "NegatableBinaryOperatorElement", function(x) {
	x$negated <- !x$negated
})


# Class for any operator that is applied postfix to its argument (e.g. DESC or ASC).
PostfixOperatorElement <- setRefClass("PostfixOperatorElement",
	contains = c(
		"OperatorElement"
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL) {
			callSuper(operator = operator)
			addChildren(left)
		}
	)
)