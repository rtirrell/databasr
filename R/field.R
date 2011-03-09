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
		
		#' Set the table containing this field.
		#' 
		#' @param table table to set as the parent.
		#' @return \code{.self}
		set_table = function(table) {
			table <<- table
			.self
		},
		
		as_field = function() {
			Field$new(.self)
		}
	)
)

#' Represents a field in an SQL expression. 
#' Since we can't multiply inherit or mixin directly, 
#' we have \code{\link{IntrospectedField}} inherit 
#' from \code{\link{SQLObject}}, even though an 
#' \code{\link{IntrospectedField}} may never occur in an expression.
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
		#' 
		#' @param alias an alias for the field.
		#' @return \code{.self}
		as = function(alias) {
			alias <<- alias 
			.self
		},
		
		#' This is a no-op, returning this field.
		as_field = function() {
			.self
		}
	)
)