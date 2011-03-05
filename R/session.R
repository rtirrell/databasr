#' Represents a session: a collection of connections to the same 
#' database with the same parameters.
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
		#' At some point we may support custom connect 
		#' (and possibly also disconnect) functions.
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
		
		#' Add a new connection, increasing the pool's size by one.
		add_connection = function() {
			if (length(users) > length(connections))
				connections[[length(connections) + 1]] <<- connect()
		},
		
		#' Add a new connection at the index of an expired one.
		reconnect = function(index) {
			connections[[index]] <<- connect()
		},
		
		#' Return a new connection.
		connect = function() {
			do.call(dbConnect, c(driver, parameters[-1]))
		},
		
		#' Ensure that a connection is not expired before handing it out.
		check_connection = function(index) {
			tryCatch(
				dbGetInfo(connections[[index]]),
				error = function(e) {
					reconnect(index)
				}
			)
			connections[[index]]
		},
		
		#' Return a connection to a requesting user.
		#' 
		#' This method will use existing connections in the pool if available. 
		#' We first check that those connections are not expired.
		request = function(user = 'anonymous') {
			which.unused <- which(is.na(users))
			if (length(which.unused) > 0) {
				users[[which.unused[1]]] <<- user
				return(list(
					connection = check_connection(which.unused[1]), 
					parameters = parameters, 
					index = which.unused[1]
				))
			}
			
			users[[length(users) + 1]] <<- user
			add_connection()
			
			list(
				connection = connections[[length(connections)]], 
				parameters = parameters,
				index = length(connections)
			)
		},
		
		#' Return a connection to the pool.
		release = function(connection) {
			users[[connection$index]] <<- NA
		},
		
		#' List connectios in the pool.
		list_connections = function() {
			for (i in seq_along(connections)) {
				cat(sprintf(
					'%s %s: %s\n', class(connections[[i]]), 
					str_c(attr(connections[[i]], 'Id'), collapse = ','), users[[i]]
				))
			}
		},
		
		#' Begin a new `SELECT` statement.
		select = function(...) {
			SelectStatement$new(session = .self)$select(...)
		},
		
		#' Tear down a session, disconnecting all connections and 
		#' unloading the database driver.
		finish = function() {
			suppressMessages({
			  for (connection in connections) {
					try(dbDisconnect(connection), silent = TRUE)
					try(dbUnloadDriver(driver), silent = TRUE)
				}
			})
			set_options(finished = TRUE)
		},
		
		#' Destroy the session, finalizing it in the process.
		finalize = function() {
			finish()
		}
	)
)

	
.do_with <- function(session, query, func) {
	connection <- session$request()
	tryCatch(
		func(connection$connection, query), 
		finally = session$release(connection)
	)
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