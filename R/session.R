#' Represents a session: a collection of connections to the same database with the same parameters.
#' 
#' DONE: support group for non-default group connections.
Session <- setRefClass('Session',
	contains = c(
		'DatabasrObject'
	),
	
	fields = c(
		"database",
		'driver',
		'parameters',
		'connect.func',
		'connections',
		'users'
	),
	
	methods = list(
		#' Create a new session.
		#' 
		#' At some point we may support custom connect (and possibly also disconnect) functions.
		initialize = function(..., connect.func = NULL) {
			initFields(
				parameters = list(...), 
				connect.func = connect.func,
				connections = list(), 
				users = list()
			)
			callSuper()
			setOptions(finished = FALSE)
			driver <<- dbDriver(parameters[[1]])
			setOptions(driver = parameters[[1]])
			
			if (length(parameters) == 1 || "group" %in% names(parameters)) {
				connection <- request()
				connection.info <- dbGetInfo(connection$connection)
				database <<- connection.info$dbname
				release(connection)
			} else database <<- parameters$dbname
			
			driver.info <- dbGetInfo(driver)
			if ("fetch_default_rec" %in% names(driver.info)) 
				setOptions(fetch.size = driver.info$fetch_default_rec)
			.self
		},
		
		connect = function() {
			if (length(users) > length(connections))
				connections[[length(connections) + 1]] <<- do.call(dbConnect, c(driver, parameters[-1]))
		},
		
		request = function(user = "anonymous") {
			which.unused <- which(is.na(users))
			if (length(which.unused) > 0) {
				users[[which.unused[1]]] <<- user
				return(list(
					connection = connections[[which.unused[1]]], parameters = parameters, 
					index = which.unused[1]
				))
			}
			
			users[[length(users) + 1]] <<- user
			connect()
			
			list(
				connection = connections[[length(connections)]], 
				index = length(connections)
			)
		},
		
		release = function(connection) {
			if (is.null(connection$index)) print(connection)
			users[[connection$index]] <<- NA
		},
		
		listConnections = function() {
			for (i in seq_along(connections)) {
				cat(sprintf(
					'%s %s: %s\n', class(connections[[i]]), 
					str_c(attr(connections[[i]], 'Id'), collapse = ','), users[[i]]
				))
			}
		},
		
		query = function(...) {
			SelectStatement$new(session = .self)$select(...)
		},
		
		finish = function() {
			if (getOption("finished")) {
				warning("Session has already been finished.")
			} else {
				suppressMessages({
					for (connection in connections) try(dbDisconnect(connection), silent = TRUE)
					try(dbUnloadDriver(driver), silent = TRUE)
				})
			setOptions(finished = TRUE)
			}
		},
		
		finalize = function() {
			finish()
		}
	)
)

#' Evaluate a get query with a temporary connection. TODO: we've been warned (by check).
#' 
#' @param data the \code{\link{Session}} object associated with this query.
#' @param expr the query we are executing, a character vector of length one.
#' @return the result as a data frame.
with.Session <- function(data, expr) {
	connection <- data$request()
	if (!is.call(expr)) expr <- substitute(expr)
	res <- dbGetQuery(connection$connection, eval(expr, parent.frame()))
	data$release(connection)
	
	res
}
	