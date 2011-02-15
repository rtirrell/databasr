# TODO: track underlying type, for smart(er) application of CONCAT or +, as appropriate.
# Issue if we want to CONCAT(some_string, field) - create a function F that returns dummy fields.
# Then we want to add getFields() to all clauses, which filters dummies.

# So we could add Function in elements. And then adding a scalar and anything creates the right
# type of function. But composing functions is more difficult if we're going to basically
# be dispatching on the class. How about - instead, we use c() for concatenation!?
Field <- setRefClass('Field',
	contains = c(
		'Scalar'
	),
	fields = c(
		'table',
		'name',
		'alias',
		'type'
	),
	methods = list(
		initialize = function(table = NULL, name = NULL, alias = NULL, type = NULL) {
			initFields(table = table, name = name, alias = alias, type = type)
			callSuper()
			return(.self)
		},
		as = function(name) {
			alias <<- name 
			return(.self)
		}
	)
)
