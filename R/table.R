IntrospectedTable <- setRefClass('IntrospectedTable',
	contains = c(
		'Element'
	),
	fields = c(
		# Session that introspected this table.
		'.session',
		# Database this table resides in (as a string).
		'.database',
		# Name of this table (as a string).
		'.name',
		# Indexes of keys in this table's fields.
		'.key',
		# This table's fields.
		'.fields'
	),
	
	methods = list(
		#' Though \code{NULL} is more logical, we have to use an empty 
		#' string for \code{.name} to prevent \code{\link{envRefClass}} 
		#' from dying when the package is loaded (as it seems to 
		#' instantiate all classes).
		initialize = function(.session = NULL, .name = '', 
													.database = NULL, .key = NULL) {
			initFields(
				.session = .session, 
				.name = .name, 
				.database = .database, 
				.key = .key
			)
			callSuper()
			# TODO: I think that all DBMS use '.' as an identifier separator, 
			# but need to check.
			if (is.null(.database) && str_detect(.name, fixed('.'))) {
				split.name <- unlist(str_split(.name, fixed('.')))
				.database <<- split.name[1]
				.name <<- split.name[2]
			}
			.self
		},
		
		# Can this go in Table only?
		get_name = function() {
			if (is.null(.database)) .name
			else str_c(.database, .name, sep = '.')
		},
		
		# This should be renamed.
		equals = function(other) {
			if (get_name() != other$get_name()) 
				return(FALSE)
			if (!have_same_elements(.key, other$.key)) 
				return(FALSE)
			
			field.names <- vapply(.fields, function(f) f$name, character(1))
			other.field.names <- vapply(
				other$.fields, function(f) f$name, character(1)
			)
			
			have_same_elements(field.names, other.field.names)
		},
		
		as_table = function() {
			Table$new(.self)
		},
		
		#' Alias the `Table` corresponding to this `IntrospectedTable`.
		as = function(name) {
			as_table()$as(name)
		}
	)
)

#' Represents a table in a query.
#' 
#' I think it's okay that alias is redefined here, 
#' as we also add additional behavior. 
Table <- setRefClass('Table',
	contains = c(
		'IntrospectedTable'
	),
	fields = c(
		'.alias'
	),
	methods = list(
		initialize = function(table) {
			import(table)
			initFields(.alias = NULL)
			.fields <<- sapply(.fields, function(f) f$as_field()$set_table(.self))
			.self
		},
		
		#' Alias this table.
		as = function(name) {
			.alias <<- name
			.self
		},
		
		
		get_compile_name = function(name) {
			if (is.null(.alias)) get_name()
			else .alias
		},
		
		#' This is a no-op, returning this table.
		as_table = function() {
			.self
		}
	)
)

.RESERVED.NAMES <- union(Table$methods(), names(Table$fields()))

#' When extracting an \code{\link{IntrospectedField}} object from this table, 
#' return the corresponding \code{\link{Field}} object.
#' @name $,IntrospectedTable-method
#' @aliases $
#' @docType methods
#' @export
setMethod('$', 'IntrospectedTable', function(x, name) {
	# TODO: we get different results from .field and from direct access.
	if (name %in% names(x[['.fields']])) x[['.fields']][[name]]$as_field()
	else findMethods('$')$envRefClass(x, as.character(name))
})

#' Introspect a database table and return an object representing that table.
#' 
#' @param session a \code{\link{Session}} object
#' @param table name of the table
#' @param database name of the database this table resides in. 
#' 	 If \code{NULL}, this will be taken from the database name of 
#'   the \code{\link{Session}} object.
#' @return a \code{\link{Table}} object representing this database table.
introspect_table <- function(session, table, database = NULL) {
	connection <- session$request()
	
	description <- dbGetQuery(
		connection$connection, sprintf('DESCRIBE %s;', table)
	)
	
	introspected <- IntrospectedTable$new(
		.session = session, .database = database, .name = table, 
		.key = which(description$Key == 'PRI')
	)
	
	fields <- list()
	
	for (i in seq_len(nrow(description))) {
		name <- description$Field[i]
		field.object <- IntrospectedField$new(
			name = name, 
			table = introspected, 
			type = create_type(description$Type[i])
		)
		
		if (name %in% .RESERVED.NAMES) {
			warning(sprintf(
				str_c(
					'Name `%s` is reserved, replacing with `%s_`. ',
					'Access in R requires use of `%s_`. ',
					'The field will still be named by `%s` in results.'
				)
			))
			name <- str_c(name, '_')
		}
		
		# Support tab-completion of field names.
		introspected[[name]] <- NULL
		fields[[name]] <- field.object
	}
	introspected$.fields <- fields
	
	if (!str_detect(table, fixed('.'))) 
		introspected$.database <- session$database
	
	session$release(connection)
	introspected
}

with.IntrospectedTable <- function(data, ...) {
	expr <- substitute(list(...))
	if (is(data, 'IntrospectedTable')) data <- data$as_table()
	
	result <- eval(expr, data$.fields, parent.frame())
	if (length(result) == 1) result[[1]] else result
}