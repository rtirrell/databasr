#' Class to wrap SELECT queries, supporting modification and pseudo-intelligent fetch.
#' 
#' A result may be in four states with respect to connections and result (sets).
#' \itemize{ 
#'  \item Neither finished nor started. It has no connection, no result set, and no results.
#' 	\item Started but not finished. It has a connection, a result set, and perhaps results.
#'  \item Finished but not started. It has no connection, no result set, and no results. 
#' 	 The associated statement was never executed before the result was torn down.
#' 	\item Both started and finished. It has no connection, no result set, and results.
#' }
#' In short, a result is started when the query is sent.
#' A non-mutable result is finished when dbHasCompleted returns TRUE.
#' 
#' @name Result
#' @exportClass
Result <- setRefClass('Result',
	contains = c(
		'DatabasrObject'
	),
	fields = c(
		'session',
		'connection',
		'statement',
		'SQL',
		'result.set',
		'result',
		'introspected',
		'pending'
	),
	methods = list(
		#' Initialize the result for a given session and statement.
		initialize = function(session, statement, 
													fetch.size = NULL, mutable = FALSE) {
			initFields(
				session = session, 
				connection = NULL, 
				SQL = statement$SQL(),
				result.set = NULL, 
				result = NULL, 
				introspected = NULL, 
				pending = list()
			)
			callSuper()
			
			if (is.null(fetch.size)) 
				fetch.size <- session$get_option('fetch.size')
			
			set_options(
				fetch.size = fetch.size, fetched.row.count = 0, 
				started = FALSE, finished = FALSE
			)
			check_mutable(statement, mutable)
			
			.self
		},
		
		#' Send the query.
		send_query = function() {
			set_options(started = TRUE)
			connection <<- session$request('result')
			result.set <<- dbSendQuery(connection$connection, SQL)
		},
		
		#' Fetch all rows for this query. 
		#' 
		#' If the result is marked mutable, then returns this object.
		#' Otherwise, returns the underlying data frame.
		all = function() {
			get(-1)
			
			if (!get_option('mutable')) 
				get_result()
			else 
				.self
		},
		
		#' Get the first row in the result set.
		first = function() {
			get(1)
			get_result()[1, ]
		},
		
		#' Get the first row in the result set, checking that 
		#' that row is the only one.
		one = function() {
			get(2)
			if (get_affected_count() > 1) 
				stop('More than one row in result set.')
			
			get_result()[1, ]
		},
		
		#' Get the underlying data frame, after finishing this result.
		get_result = function() {
			finish()
			result
		},
		
		#' Return the number of rows affected by this query.
		get_affected_count = function() {
			if (get_option('finished')) 
				get_option('affected.row.count')
			else if (get_option('started')) 
				dbGetRowCount(result.set)
			else 
				NA
		},
		
		#' Return the number of rows fetched so far.
		get_fetched_count = function() {
			get_option('fetched.row.count')
		},
		
		get_statement = function() {
			str_c(SQL, '\n')
		},
		
		#' Fetch results from the result set.
		#' 
		#' @param n number of results to fetch (see documentation for \code{\link{Result}} for
		#'   more information on the default for this, which is a class-level field)
		#' @param retain whether to append to the old result or discard it completely
		get = function(n, retain = TRUE) {
			if (missing(n)) n <- get_option('fetch.size')
			
			if (!get_option('started')) send_query()
			
			if (!is.null(result) && retain) {
				if (n == -1) {
					old.result <- result
					result <<- fetch(result.set, n)
					indices <- (nrow(result) + 1):(nrow(result) + nrow(old.result))
					result[indices] <<- old.result
				} else {
					new.result <- fetch(result.set, n)
					result[1:nrow(new.result) + nrow(result), ] <<- new.result
				}
			} else 
				result <<- fetch(result.set, n)
			
			set_options(fetched.row.count = get_option('fetched.row.count') + n)
			
			if (dbHasCompleted(result.set) && !get_option('mutable')) 
				finish()
			
			.self
		},
		
		is_dirty = function() {
			length(pending) != 0
		},
		
		#' Update information on the result set and return connections.
		finish = function() {
			if (is_dirty()) 
				warning('Finishing result with pending mutations.')
			
			pending <<- list()
			set_options(affected.row.count = get_affected_count())
			
			if (get_option('started')) {
				dbClearResult(result.set)
				session$release(connection)
			}
			set_options(finished = TRUE)
		},
		
		
		#' I'm not sure when to use dbCommit, 
		#' and when to send the equivalent query.
		flush = function() {
			flush.connection <- session$request('flush')
			dbSendQuery(flush.connection$connection, 'START TRANSACTION;')
			for (mutation in pending) {
				mutation.SQL <- mutation$SQL()
				dbSendQuery(flush.connection$connection, mutation.SQL)
			}
			
			dbSendQuery(flush.connection$connection, 'COMMIT;')
			pending <<- list()
			session$release(flush.connection)
		},
		
		
		#' Check whether a result is mutable. 
		#' 
		#' If the user believes the result is mutable, we don't -- check first.
		check_mutable = function(statement, mutable) {
			set_options(mutable = mutable)
			if (mutable) {
				stop.message <- NULL
				statement$prepare()
				
				from.clause <- statement$.children$from
				join.clauses <- statement$.children$joins
					
				if (!is.null(from.clause) && length(from.clause$tables) > 1) 
					stop.message <- 'Cannot alter result FROM multiple tables.'

				else if (!is.null(join.clauses) && join.clauses$has_children())
					stop.message <- 'Cannot alter JOIN result.'

				introspected <<- from.clause$tables[[1]]
				
				table.keys <- vapply(
					introspected$.fields[introspected$.key], 
					function(k) k$name, 
					character(1)
				)
													 
				select.keys <- vapply(
					statement$.children$select$.children, 
					function(k) k$name, 
					character(1)
				)
				
				if (length(introspected$.key) == 0 || 
						!have_same_elements(table.keys, select.keys))
					stop.message <- 'Cannot alter result lacking complete primary key.'

				statement$restore()
				
				if (!is.null(stop.message)) 
					stop(stop.message)
				
				set_options(mutable = TRUE)
			}
		},
		
		#' When this object is being destroyed, make sure we wrap up politely.
		finalize = function() {
			finish()
		}
	)
)

#' Extract values from the underlying data frame.
`[.Result` <- function(result, i, j, ..., drop = FALSE) {
	if (i > result$get_option('affected.row.count')) 
		stop(sprintf('Index %d is out of bounds.', i))
	
	delta <- max(result$get_option('fetch.size'), 
							 i - result$get_option('fetched.row.count'))
	result$get(delta)
	
	`[.data.frame`(result$result, i, j, ..., drop)
}

#' Access a field in the underlying data frame.
setMethod('$', 'Result', function(x, name) {
	if (name %in% names(x[['result']])) 
		x[['result']][, name]
	else 
		findMethods('$')$envRefClass(x, as.character(name))
})

#' Replace value(s) in the underlying data frame.
`[<-.Result` <- function(result, i, j, value) {
	if (!result$get_option('started')) 
		stop(str_c(
			'Attempting to modify result that has not been populated.',
			'Either access the result to trigger fetching or get() manually.', 
			sep = ' '
		))
		
	if (missing(i)) 
		i <- seq_len(nrow(result$result))
	if (missing(j)) 
		j <- seq_along(result$result)
	
	if (result$get_option('mutable')) {
		# This should check affected row count.
		if (i[length(i)] > nrow(result$result)) {
			
		}
		keys <- result$result[i, result$introspected$.key, drop = FALSE]
	}
	
	# result$result[]?
	result$result <- `[<-.data.frame`(result$result, i, j, value)
	
	if (result$get_option('mutable')) {
		if (i[length(i)] > nrow(result$result)) {
			# PendingInsert: keep track of the indices and force 
			# flushing when new result is fetched if retain is false.
		} else {
			for (k in seq_along(i)) {
				update <- UpdateStatement$new()
				update$update(result$introspected$as_table())
				
				for (l in j) 
					update$set(
						result$introspected$.fields[[l]]$as_field() == 
						result$result[i[k], j]
					)
				
				for (l in result$introspected$.key)
					update$where(
						result$introspected$.fields[[l]]$as_field() == keys[k, l]
					)
				
				result$pending <- c(result$pending, update)
			}
		}
	}
	result
}
#' \code{\link{rbind}} rows to a \code{\link{Result}} object, 
#' queueing them for insertion.
rbind.Result <- function(result, ..., deparse.level = 1) {
	warning('Calling "rbind" on objects of class "Result" is not yet supported.')
	result
}

#' Nicely format a \code{\link{Result}} object, 
#' displaying state and underyling result.
#' 
#' @param x the object to print.
#' @param nrows the number of rows of the result to display. 
#' @return \code{NULL}, invisibly.
setMethod('print', 'Result', function(x, nrows = 10, ...) {
	cat('<An object of class "Result">\n')
	
	if (is.null(x$result) || nrow(x$result) == 0) {
		if (x$get_option('started')) 
			cat('* No results for this query.\n')
		else 
			cat('* No results have been fetched for this query.\n')
	} else {
		if (nrows < nrow(x$result)) {
			print(x$result[1:nrows, ])
			cat('* Displaying first', nrows, 'of', nrow(x$result), 'rows.\n')
		} else {
			print(x$result)
			cat('* Displaying all', nrow(x$result), 'rows.\n')
		}
	}
	cat(
		'* completed: ', x$get_option('finished'), 
		', affected: ', x$get_affected_count(), ' rows', sep = ''
	)
	
	if (x$get_option('mutable')) 
		cat(', pending: ', length(x$pending), ' mutations', sep = '')
	
	cat('.\n', sep = '')
})

setMethod('show', 'Result', function(object) print(object))
setMethod('dim', 'Result', function(x) c(nrow(x$result), ncol(x$result)))
