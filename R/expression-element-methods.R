##
# Modifiers.
##

setMethod("!", "NegatableBinaryOperatorElement", function(x) {
	x$negate()
})



##
# Functions.
##
setMethod("sum", "SelectableElement", function(x) {
		FunctionElement$new("SUM", x)
	})

setMethod("max", "SelectableElement", function(x) {
		FunctionElement$new("MAX", x)
	})

setMethod("min", "SelectableElement", function(x) {
		FunctionElement$new("MIN", x)
	})

setMethod("unique", "SelectableElement", function(x) {
		PrefixOperatorElement$new("DISTINCT", x)
	})

#' A \code{\link{length}} method to support generation of 'COUNT' functions.
setMethod("length", "SelectableElement", function(x) {
		FunctionElement$new("COUNT", x)
	})

setMethod("tolower", "SelectableElement", function(x) {
		FunctionElement$new("LOWER", x)
	})

setMethod("toupper", "SelectableElement", function(x) {
		FunctionElement$new("UPPER", x)
	})

`[.SelectableElement` <- function(x, i, j, drop = FALSE) {
	if (!inherits(x$type, "StringType")) {
		# But we really can, and I'm not even sure it's bad form.
		warning("Indexing a non-string field with `[`.")
	}
	FunctionElement$new("SUBSTRING", x, i[1], i[length(i)] - i[1] + 1)
}

setGeneric("%like%", function(selectable, value) standardGeneric("%like%"))
setMethod("%like%", "SelectableElement", function(selectable, value) {
	BinaryOperatorElement$new("LIKE", selectable, value)
})

setGeneric("%ilike%", function(selectable, value) standardGeneric("%ilike%"))
setMethod("%ilike%", "SelectableElement", function(selectable, value) {
	BinaryOperatorElement$new("ILIKE", selectable, value)
})

setGeneric("concat", function(selectable, ..., sep = "") standardGeneric("concat"))
setMethod("concat", "SelectableElement", function(selectable, ..., sep = "") {
	if (sep != "") FunctionElement$new("CONCAT_WS", sep, selectable, ...)
	else FunctionElement$new("CONCAT", selectable, ...)
})

setGeneric('instr', function(selectable, ...) standardGeneric('instr'))
setMethod('instr', 'SelectableElement', function(selectable, ...) {
	FunctionElement$new('INSTR', selectable, ...)
})

##
# Operators.
##
setMethod("%in%", c("SelectableElement", "ANY"), function(x, table) {
		if (!inherits(table, "TupleElement")) table <- do.call(TupleElement$new, as.list(table))
		NegatableBinaryOperatorElement$new("IN", x, table)
	})

setMethod("+", c("SelectableElement", "ANY"), function(e1, e2) {
		if (missing(e2)) PostfixOperatorElement$new("ASC", e1)
		else BinaryOperatorElement$new("+", e1, e2)
	})

setMethod("-", c("SelectableElement", "ANY"), function(e1, e2) {
		if (missing(e2)) PostfixOperatorElement$new("DESC", e1)
		else BinaryOperatorElement$new("-", e1, e2)
	})

setMethod("<", c("SelectableElement", "ANY"), function(e1, e2) {
		BinaryOperatorElement$new("<", e1, e2)
	})
setMethod("<=", c("SelectableElement", "ANY"), function(e1, e2) {
		BinaryOperatorElement$new("<=", e1, e2)
	})

setMethod(">", c("SelectableElement", "ANY"), function(e1, e2) {
		BinaryOperatorElement$new(">", e1, e2)
	})
setMethod(">=", c("SelectableElement", "ANY"), function(e1, e2) {
		BinaryOperatorElement$new("<=", e1, e2)
	})

setMethod("==", c("SelectableElement", "ANY"), function(e1, e2) {
		NegatableBinaryOperatorElement$new("=", e1, e2)
	})

setMethod("!=", c("SelectableElement", "ANY"), function(e1, e2) {
		NegatableBinaryOperatorElement$new("!=", e1, e2)
	})

setMethod("&", c("SelectableElement", "ANY"), function(e1, e2) {
		BinaryOperatorElement$new("AND", e1, e2)
	})

setMethod("|", c("SelectableElement", "ANY"), function(e1, e2) {
		BinaryOperatorElement$new("OR", e1, e2)
	})
