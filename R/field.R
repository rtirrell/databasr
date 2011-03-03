#' Class representing a database field.
IntrospectedField <- setRefClass('IntrospectedField',
	contains = c(
		'SelectableElement'
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
		
		set_table = function(new.table) {
			table <<- new.table
			.self
		},
		
		as_field = function() {
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
	fields = c(
		'alias'
	),
	methods = list(
		initialize = function(field) {
			import(field)
			initFields(alias = NULL)
		},
		
		#' Alias this field to the given name.
		as = function(name) {
			alias <<- name 
			.self
		},
		
		#' This is a no-op, returning this field.
		as_field = function() {
			.self
		}
	)
)

#' This is just a stub idea. Is there a way we can force initialization of fields?
#' Look at the slots of .refClassDef.
mixin <- function(ref.class, name, func, fields = list()) {
	if (is.null(ref.class$.mixed.in)) ref.class$.mixed.in <- list()
	mixin.info <- list(name = name, func = func, fields = fields)
	ref.class$.mixed.in[[name]] <- mixin.info
	
	wrapper <- function(...) {
		mixin.info <- .self$TODO
		if (!exists('.mixed.in', .self, inherits = FALSE)) {
			field.names <- names(fields)
			for (i in seq_along(fields)) {
				if (!exists(field.names[[i]], .self, inherits = FALSE)) 
					assign(field.names[[i]], fields[[i]], .self, inherits = FALSE)
			}
			assign('.mixed.in', TRUE, .self, inherits = FALSE)
		}
		func(...)
	}
	method.list <- list()
	method.list[[name]] <- wrapper
	do.call(ref.class$methods, method.list)
}
#mixin(Session, 'hello', function() print('hello'))