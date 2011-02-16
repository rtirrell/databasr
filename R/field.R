#' Class representing a database field.
IntrospectedField <- setRefClass('IntrospectedField',
	contains = c(
		'Scalar'
	),
	fields = c(
		'table',
		'name',
		'type'
	),
	methods = list(
		initialize = function(table = NULL, name = NULL, type = NULL) {
			initFields(table = table, name = name, type = type)
			callSuper()
		},
		
		asField = function() {
			Field$new(.self)
		}
	)
)

#' Represents a field in an SQL expression. Since we can't multiply inherit or mixin
#' directly, we have \code{\link{IntrospectedField}} inherit from \code{\link{SQLObject}},
#' even though an \code{\link{IntrospectedField}} may never occur in an expression.
Field <- setRefClass('Field',
	contains = c(
		'IntrospectedField'
	),
	# Still trying to figure out where alias should go, and if it's best suited to
	# being a field versus an option versus another class.
	fields = c(
		'alias'
	),
	methods = list(
		initialize = function(field) {
			import(field)
			initFields(alias = NULL)
		},
		as = function(name) {
			alias <<- name 
			.self
		}
	)
)

#' This is just a stub idea. Is there a way we can force initialization of fields?
#' NOTE that this doesn't really work.
buildMixinMethod <- function(ref.class, name, definition, mixin.fields = list()) {
	do.call(ref.class$fields, mixin.fields)
	print(ls())
	
	# Now, how to ensure we init these fields in ctor?
	wrapper <- function(...) {
		# We shouldn't ever need to infer, so get ought to work.
		field.names <- names(mixin.fields)
		for (i in seq_along(field.names)) {
			if (inherits(get(.self, field.names[[i]]), 'UninitializedField'))
				assign(.self, field.names[[i]], mixin.fields[[i]])
		}
		definition(...)
	}
	method.list <- list()
	method.list[[name]] <- wrapper
	do.call(ref.class$methods, method.list)
}