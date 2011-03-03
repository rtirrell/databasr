#' Represents a session: a collection of connections to the same database with the same parameters.
Session <- setRefClass('Session',
	contains = c(
		'DatabasrObject'
	),
	
	fields = c(
		'database',
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
			set_options(finished = FALSE)
			
			require('RMySQL')
			driver <<- dbDriver(parameters[[1]])
			set_options(driver = parameters[[1]])
			
			if (length(parameters) == 1 || 'group' %in% names(parameters)) {
				connection <- request()
				connection.info <- dbGetInfo(connection$connection)
				database <<- connection.info$dbname
				release(connection)
			} else database <<- parameters$dbname
			
			driver.info <- dbGetInfo(driver)
			if ('fetch_default_rec' %in% names(driver.info)) 
				set_options(fetch.size = driver.info$fetch_default_rec)
			.self
		},
		
		connect = function() {
			if (length(users) > length(connections))
				connections[[length(connections) + 1]] <<- do.call(dbConnect, c(driver, parameters[-1]))
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
			
			list(
				connection = connections[[length(connections)]], 
				index = length(connections)
			)
		},
		
		release = function(connection) {
			users[[connection$index]] <<- NA
		},
		
		list_connections = function() {
			for (i in seq_along(connections)) {
				cat(sprintf(
					'%s %s: %s\n', class(connections[[i]]), 
					str_c(attr(connections[[i]], 'Id'), collapse = ','), users[[i]]
				))
			}
		},
		
		select = function(...) {
			SelectStatement$new(session = .self)$select(...)
		},
		
		finish = function() {
			suppressMessages({
			  for (connection in connections) {
					try(dbDisconnect(connection), silent = TRUE)
					try(dbUnloadDriver(driver), silent = TRUE)
				}
			})
			set_options(finished = TRUE)
		},
		
		finalize = function() {
			finish()
		}
	)
)

	
.do_with <- function(session, query, func) {
	connection <- session$request()
	result <- func(connection$connection, query)
	session$release(connection)
	result
}

#' Call dbSendQuery with the given statement, using a temporary connection.
send_with <- function(session, statement) {
	.do_with(session, statement, dbSendQuery)
}

#' Call dbGetQuery with the given statement, using a temporary connection.
get_with <- function(session, statement) {
	.do_with(session, statement, dbGetQuery)
}

#' Escape SQL-special characters in the given values.
#' 
#' Only the MySQL driver implements this method, so we ought to handle this ourselves.
escape <- function(session, values) {
	connection <- session$request()
	escaped <- dbEscapeStrings(connection$connection, values)
	session$release(connection)
	escaped
}