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
		#' Though \code{NULL} is more logical, we have to use an empty string for \code{.name} to 
		#' prevent \code{\link{envRefClass}} from dying when the package is loaded (as it seems to 
		#' instantiate all classes).
		initialize = function(.session = NULL, .name = "", .database = NULL, .key = NULL) {
			initFields(
				.session = .session, .name = .name, .database = .database, .key = .key
			)
			callSuper()
			# TODO: I think that all RDBMS use "." as an identifier separator, but need to check.
			if (is.null(.database) && str_detect(.name, fixed("."))) {
				split.name <- unlist(str_split(.name, fixed(".")))
				.database <<- split.name[1]
				.name <<- split.name[2]
			}
			.self
		},
		
		getName = function() {
			if (is.null(.database)) return(.name)
			str_c(.database, .name, sep = ".")
		},
		
		equals = function(other) {
			if (getName() != other$getName()) return(FALSE)
			if (!haveSameElements(.key, other$.key)) return(FALSE)
			
			field.names <- unlist(sapply(.fields, function(f) f$name))
			other.field.names <- unlist(sapply(other$.fields, function(f) f$name))
			if (!haveSameElements(field.names, other.field.names)) return(FALSE)
			
			TRUE
		},
		
		asTable = function() {
			Table$new(.self)
		}
	)
)

Table <- setRefClass('Table',
	contains = c(
		'IntrospectedTable'
	),
	methods = list(
		initialize = function(table) {
			import(table)
			.self
		}
	)
)

#' Test equality of two tables.
#' 
#' TODO: move to value-based testing, as above.
#' @param table the \code{\link{Table}} object to test.
#' @param other the object to test against.
#' @return \code{TRUE} if \code{other} is a \code{\link{Table}} object and shares the same name.
#' 	 \code{FALSE} if the names differ, and \code{NA} if \code{other} is not a \code{\link{Table}}
#'   object.
`==.IntrospectedTable` <- function(table, other) {
	if (!inherits(other, 'IntrospectedTable')) return(NA)
	return(table$getName() == other$getName())
}


`!=.Table` <- function(table, other) {
	return(!`==.Table`(table, other))
}

#' When extracting an \code{\link{IntrospectedField}} object from this table, return the
#' corresponding \code{\link{Field}} object.
setMethod("$", "IntrospectedTable", function(x, name) {
	if (name %in% names(x[[".fields"]])) x[[".fields"]][[name]]$asField()
	else findMethods("$")$envRefClass(x, as.character(name))
})

#' Not sure that we ought to support this. This sort of access is, I think, best reserved and thus
#' we ourselves can handle calling \code{\link{asField()}} as necessary.
#setMethod("[[", "Table", function(x, i, j, ..., drop = TRUE) {
#	if (i %in% names(x$.fields)) x$.fields[[i]]$asField()
#	else as.environment(x)[[x, i, j, ..., drop = drop]]
#	if (i %in% names(.Primitive("[[")(x, ".fields"))) .Primitive("[[")(x, ".fields")[[i]]$asField()
#	else .Primitive("[[")(x, i, j, ..., drop = drop)
#})

setMethod('&', c('IntrospectedTable', 'IntrospectedTable'), function(e1, e2) {
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
	introspected <- IntrospectedTable$new(
		.session = session, .database = database, .name = table, 
		.key = which(description$Key == "PRI")
	)
	
	fields <- list()
	
	for (i in seq_len(nrow(description))) {
		field.object <- IntrospectedField$new(
			name = description$Field[i], table = introspected, 
			type = createType(description$Type[i])
		)
		# This works, but assignment has the advantage of tab-completing field names while running
		# interactively.
		fields[[description$Field[i]]] <- field.object
	}
	introspected$.fields <- fields
	
	if (!str_detect(table, fixed('.'))) introspected$.database <- session$database
	session$release(connection)
	return(introspected)
}