Table <- setRefClass('Table',
	contains = c(
		'ClauseElement'
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
		initialize = function(.session, .name, .database = NULL, .key) {
			initFields(
				.session = .session, .name = .name, .database = .database, .key = .key
			)
			callSuper()
			if (is.null(.database) && str_detect(.name, fixed('.'))) {
				split.name <- unlist(str_split(.name, fixed('.')))
				.database <<- split.name[1]
				.name <<- split.name[2]
			}
			return(.self)
		},
		getName = function() {
			if (is.null(.database)) return(.name)
			return(str_c(.database, .name, sep = '.'))
		}
	)
)


#' Test equality of two tables.
#' @param table the \code{\link{Table}} object to test.
#' @param other the object to test against.
#' @return \code{TRUE} if \code{other} is a \code{\link{Table}} object and shares the same name.
#' 	 \code{FALSE} if the names differ, and \code{NA} if \code{other} is not a \code{\link{Table}}
#'   object.
`==.Table` <- function(table, other) {
	if (!inherits(other, 'Table')) return(NA)
	return(table$getName() == other$getName())
}


`!=.Table` <- function(table, other) {
	return(!`==.Table`(table, other))
}

setMethod("$", "Table", function(x, name) {
	if (name %in% names(x[[".fields"]])) x[[".fields"]][[name]]
	else findMethods("$")$envRefClass(x, as.character(name))
})

setMethod('&', c('Table', 'Table'), function(e1, e2) {
	query <- e1$.session$query()$from(e1)$join(e2)
	return(query)
})

#' Introspect a database table and return an object representing that table.
#' 
#' @param session the \code{\link{Session}} object.
#' @param table the name of the table.
#' @param database the name of the database this table resides in. If \code{NULL}, this will be 
#'   taken from the database name of the \code{\link{Session}} object.
#' @return a \code{\link{Table}} object representing this database table.
#' TODO: check assignment to reserved names.
introspectTable <- function(session, table, database = NULL) {
	connection <- session$request()
	description <- dbGetQuery(connection$connection, sprintf("DESCRIBE %s;", table))
	introspected <- Table$new(
		.session = session, .database = database, .name = table, 
		.key = which(description$Key == "PRI")
	)
	
	fields <- list()
	
	for (i in seq_len(nrow(description))) {
		field.object <- Field$new(
			name = description$Field[i], alias = NULL, table = introspected, 
			type = createType(description$Type[i])
		)
		fields[[description$Field[i]]] <- field.object
	}
	introspected$.fields <- fields
	
	if (!str_detect(table, fixed('.'))) introspected$.database <- connection$parameters$dbname
	session$release(connection)
	return(introspected)
}