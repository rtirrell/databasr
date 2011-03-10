Element <- setRefClass('Element',
	contains = c(
		'SQLObject'
	),
	methods = list(
		initialize = function() {
			callSuper()
		}
	)
)

SelectableElement <- setRefClass('SelectableElement',
	contains = c(
		'Element'
	)
)

TupleElement <- setRefClass('TupleElement',
	contains = c(
		'SelectableElement'
	),
	methods = list(
		initialize = function(...) {
			callSuper()
			add_children(...)
			.self
		}
	)
)
#' Wrap the given objects in parentheses.
#' 
#' @param ... the objects to be wrapped
#' @return a \code{\link{TupleElement}} containing the given objects.
#' 
#' @export
tuple <- function(...) TupleElement$new(...)


OperFunElement <- setRefClass('OperFunElement',
	contains = c(
		'SelectableElement'
	),
	fields = c(
		'alias'
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
		'OperFunElement'
	),
	fields = c(
		'func'
	),
	methods = list(
		initialize = function(func = NULL, ...) {
			callSuper()
			initFields(func = func)
			add_children(...)
		}
	)
)

OperatorElement <- setRefClass('OperatorElement',
	contains = c(
		'OperFunElement'
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

# TODO: add alias to BinaryOperatorElement, or alias on OperatorElement.
BinaryOperatorElement <- setRefClass('BinaryOperatorElement',
	contains = c(
		'OperatorElement'
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL, right = NULL) {
			initFields()
			callSuper(operator = operator)
			add_children(left, right)
		}
	)
)


#' Class for any binary operator (==, IN, IS) that is negatable 
#' (i.e. to !=, NOT IN, IS NOT).
#' 
#' I know this name is a little on the long side, but I like consistency.
NegatableBinaryOperatorElement <- setRefClass('NegatableBinaryOperatorElement',
	contains = c(
		'BinaryOperatorElement'
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL, right = NULL) {
			callSuper(operator = operator, left = left, right = right)
			if (identical(right, NA)) {
				if (operator == '=') operator <<- 'IS'
				else if (operator == '!=') operator <<- 'IS NOT'
			}
			.self
		},
		negate = function() {
			operator <<- .NEGATABLE.OPERATOR.MAP[[operator]]
			.self
		}
	)
)



#' Class for any operator that is applied postfix to its argument 
#' (e.g. DESC or ASC).
PostfixOperatorElement <- setRefClass('PostfixOperatorElement',
	contains = c(
		'OperatorElement'
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL) {
			callSuper(operator = operator)
			add_children(left)
		}
	)
)

PrefixOperatorElement <- setRefClass('PrefixOperatorElement',
	contains = c(
		'OperatorElement'
	),
	methods = list(
		initialize = function(operator = NULL, ...) {
			callSuper(operator = operator)
			add_children(...)
		}
	)
)