Session <- setRefClass('Session',
	contains = c(
		'DatabasrObject'
	),
	
	fields = c(
		'driver',
		'parameters',
		'connect.func',
		'connections',
		'users'
	),
	
	methods = list(
		initialize = function(..., connect.func = NULL) {
			initFields(
				parameters = list(...), 
				connect.func = connect.func,
				connections = list(), 
				users = list()
			)
			callSuper()
			setOptions(finished = FALSE, parameter.arguments = TRUE)
			driver <<- dbDriver(parameters[[1]])
			
			if (length(parameters) == 1) {
				setOptions(parameter.arguments = FALSE)
				connection <- request()
				connection.info <- dbGetInfo(connection$connection)
				parameters[names(connection.info)] <<- connection.info
				release(connection)
			}
			
			return(.self)
		},
		
		connect = function() {
			if (length(users) > length(connections)) {
				if (getOption('parameter.arguments'))
					connections[[length(connections) + 1]] <<- do.call('dbConnect', c(driver, parameters[-1]))
				else
					connections[[length(connections) + 1]] <<- dbConnect(driver)
			}
		},
		
		request = function(user = 'anonymous') {
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
			
			return(list(
				connection = connections[[length(connections)]], 
				parameters = parameters,
				index = length(connections)
			))
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
			return(SelectStatement$new(session = .self)$select(...))
		},
		
		finish = function() {
			if (getOption('finished')) {
				warning('Session has already been finished.')
			} else {
				suppressMessages({
					for (connection in connections) try(dbDisconnect(connection), silent = TRUE)
					try(dbUnloadDriver(driver), silent = TRUE)
				})
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
	