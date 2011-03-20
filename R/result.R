#' Class to wrap \code{SELECT} queries, supporting modification and 
#' pseudo-intelligent fetch.
#' 
#' A result may be in four states with respect to connections 
#' and result sets.
#' \itemize{ 
#'  \item Neither finished nor started. 
#' 		It has no connection, no result set, and no results.
#' 	\item Started but not finished. 
#' 		It has a connection, a result set, and may have fetched results.
#'  \item Finished but not started. 
#' 		It has no connection, no result set, and no results. 
#' 		The associated statement was never executed before the result was 
#' 		torn down.
#' 	\item Both started and finished. 
#' 		It has no connection, no result set, and results.
#' }
#' 
#' In short, a result is started when the query is sent.
#' A non-mutable result is finished when \code{dbHasCompleted} 
#' returns \code{TRUE}.
#' @export
Result <- setRefClass('Result',
	contains = c(
		'DatabasrObject'
	),
	fields = c(
		'session',
		'connection',
		'statement',
		'sql',
		'result.set',
		'result',
		'introspected',
		'pending'
	),
	methods = list(
		#' Initialize the result for a given session and statement.
		#' 
		#' @param session the session this result is associated with.
		#'   Connections will be requested from this session. 
		#'   Usually, this argument will be passed from the statement.
		#' @param statement a subclass of \code{\link{Statement}}.
		#' @param fetch.size the number of rows to fetch at a time, an
		#'   integer vector of length one.
		#' @param mutable whether the result of executing the statement
		#'   should be considered mutable.
		#' 
		#' @return \code{.self}
		initialize = function(session, statement, fetch.size = NULL, 
				mutable = FALSE, flush.mode = NULL, time = NULL, count = NULL) {
			initFields(
				session = session, 
				connection = NULL, 
				sql = statement$sql(),
				result.set = NULL, 
				result = NULL, 
				introspected = NULL, 
				pending = list()
			)
			callSuper()
			
			if (is.null(fetch.size)) 
				fetch.size <- session$get_option('fetch.size')
			
			if (!is.null(flush.mode)) {
				if (flush.mode == 'time')
					set_option(flush.time = time)
				else if (flush.mode == 'count')
					set_option(flush.count = count)
				else
					stop(sprintf('unknown flush mode %s specified.', flush.mode))
			}
			
			set_options(
				fetch.size = fetch.size, 
				started = FALSE, 
				affected.row.count = 0,
				finished = FALSE
			)
			check_mutable(statement, mutable)
			
			.self
		},
		
		#' Send the query.
		#' 
		#' @return \code{NULL}, invisibly.
		send_query = function() {
			set_options(started = TRUE)
			connection <<- session$request('result')
			result.set <<- dbSendQuery(connection$connection, sql)
			invisible()
		},
		
		#' Fetch all rows for this query. 
		#' 
		#' @return If the result is mutable, this
		#'   \code{\link{Result}} object.
		#' Otherwise, the underlying \code{\link{data.frame}}.
		all = function() {
			get(-1)
			
			if (!get_option('mutable')) 
				get_result()
			else 
				.self
		},
		
		#' Get the first row in the result set.
		#' 
		#' @return a \code{\link{data.frame}} with one row.
		first = function() {
			get(1)
			get_result()[1, ]
		},
		
		#' Get the first row in the result set, checking that 
		#' that row is the only one.
		#' 
		#' @return a \code{\link{data.frame}} with one row.
		one = function() {
			get(2)
			if (get_affected_count() > 1) 
				stop('More than one row in result set.')
			
			get_result()[1, ]
		},
		
		#' Get the underlying data frame, after finishing this result.
		#' 
		#' @return a \code{\link{data.frame}} containing all currently
		#'   retained rows.
		get_result = function() {
			finish()
			result
		},
		
		#' Return the number of rows affected by this query.
		#' 
		#' @return the number of rows affected by this query, a numeric vector
		#'   of length one.
		get_affected_count = function() {
			if (is_finished()) 
				get_option('affected.row.count')
			else if (get_option('started')) 
				dbGetRowCount(result.set)
			else 
				0
		},
		
		#' Return the statement that this result represents the execution of.
		#' 
		#' @return the formatted statement, a character vector of length one.
		get_statement = function() {
			str_c(sql, '\n')
		},
		
		#' Fetch results from the result set.
		#' 
		#' @param n number of results to fetch (see documentation for 
		#' 	 \code{\link{Result}} for more information on the default for this, 
		#' 	 which is a class-level field)
		#' @param retain whether to append to the old result or discard 
		#'   it completely
		#' @return \code{.self}
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
			
			if (dbHasCompleted(result.set) && !get_option('mutable')) 
				finish()
			
			.self
		},
		
		#' Check whether this \code{\link{Result}} object has pending mutations -
		#' \code{UPDATE}s or \code{INSERT}s.
		#' 
		#' @return \code{NA} if this \code{\link{Result}} object is not mutable,
		#'   \code{TRUE} if it is and there are pending mutations, 
		#' 	 \code{FALSE} if it is and there are none.
		is_dirty = function() {
			if (!get_option('mutable')) return(NA)
			length(pending) != 0
		},
		
		is_finished = function() {
			get_option('finished')
		},
		
		#' Update information on the result set and return connections.
		#' 
		#' @return \code{.self}
		finish = function() {
			if (identical(is_dirty(), TRUE)) 
				warning('Finishing result with pending mutations.')
			
			pending <<- list()
			set_options(affected.row.count = get_affected_count())
			
			if (get_option('started')) {
				dbClearResult(result.set)
				session$release(connection)
			}
			set_options(finished = TRUE)
		},
		
		
		#' Flush all pending mutations.
		#' 
		#' I'm not sure when to use dbCommit, 
		#' and when to send the equivalent query.
		#' 
		#' @return \code{NULL}, invisibly.
		flush = function() {
			flush.connection <- session$request('flush')
			dbSendQuery(flush.connection$connection, 'START TRANSACTION;')
			for (mutation in pending) {
				mutation.sql <- mutation$sql()
				dbSendQuery(flush.connection$connection, mutation.sql)
			}
			
			dbSendQuery(flush.connection$connection, 'COMMIT;')
			pending <<- list()
			session$release(flush.connection)
			
			invisible()
		},
		
		
		#' Check whether a result is mutable. 
		#' 
		#' If the user believes the result is mutable, we don't -- check first.
		#' 
		#' @return \code{NULL}, invisibly.
		check_mutable = function(statement, mutable) {
			set_options(mutable = mutable)
			if (mutable) {
				stop.message <- NULL
				statement$prepare()
				
				if (length(statement$.children$select$tables) > 1) {
					stop('cannot alter result from multiple tables.')
					statement$restore()
				}

				introspected <<- statement$.children$select$tables[[1]]
				
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
						!have_same_elements(table.keys, select.keys)) {
					stop('cannot alter result lacking complete primary key.')
					statement$restore()
				}
				
				set_options(mutable = TRUE)
			}
			invisible()
		},
		
		#' Check whether the given rows and columns are in-bounds.
		#' 
		#' \code{stop()}s if either rows or columns are not.
		#' 
		#' @return \code{NULL}, invisibly.
		check_bounds = function(i, j) {
			if (!missing(i) && i > get_affected_count())
				stop(sprintf('row %d is out of bounds.', i), call. = FALSE)
			if (!missing(j) && j > length(result)) 
				stop(sprintf('column %d is out of bounds.', j), call. = FALSE)
			
			invisible()
			
		},
		
		compute_offset_rows = function(i) {
			rows <- i - get_affected_count() + nrow(result)
			if (any(rows <= 0))
				stop(str_c(
						'requested rows that have already been fetched ',
						'and disposed of. ',
						'`get` was called with `retain = TRUE`.'
					), call. = FALSE
				)
			rows
		},
		
		#' When this object is being destroyed, make sure we wrap up politely.
		#' 
		#' @return this \code{\link{Result}} object, finalized.
		#'   It has no fields and no methods.
		finalize = function() {
			finish()
		}
	)
)

#' Extract values from the underlying data frame.
#' 
#' @param ... arguments passed to \code{[.data.frame]}.
setMethod('[', 'Result', function(x, i, j, ..., drop = FALSE) {
	
	if (!missing(i) && !x$is_finished() && 
			i > x$get_affected_count()) {
		delta <- max(
			x$get_option('fetch.size'), i - x$get_affected_count()
		)
		x$get(delta)
	}
	
	x$check_bounds(i, j)
	i <- x$compute_offset_rows(i)
	
	`[.data.frame`(x$result, i, j, ..., drop)
})

#' Access a field in the underlying data frame.
setMethod('$', 'Result', function(x, name) {
	if (name %in% names(x[['result']])) 
		x[['result']][, name]
	else 
		findMethods('$')$envRefClass(x, as.character(name))
})

#' Replace value(s) in the underlying data frame.
setMethod('[<-', 'Result', function(x, i, j, value) {
		if (!x$get_option('started')) 
			stop(str_c(
				'Attempting to modify x that has not been populated.',
				'Either access the x to trigger fetching or get() manually.', 
				sep = ' '
			))
			
		if (missing(i)) 
			i <- seq_len(nrow(x$result))
		if (missing(j)) 
			j <- seq_along(x$result)
		
		x$check_bounds(i, j)
		i <- x$compute_offset_rows(i)
		
		if (x$get_option('mutable')) {
			# This should check affected row count.
			if (i[length(i)] > nrow(x$result)) {
				
			}
			keys <- x$result[i, x$introspected$.key, drop = FALSE]
		}
		
		# x$result[]?
		x$result <- `[<-.data.frame`(x$result, i, j, value)
		
		if (x$get_option('mutable')) {
					
			if (i[length(i)] > nrow(x$result)) {
				# PendingInsert: keep track of the indices and force 
				# flushing when new result is fetched if retain is false.
			} else {
				for (k in seq_along(i)) {
					update <- UpdateStatement$new()
					update$update(x$introspected$as_table())
					
					for (l in j) 
						update$set(
							x$introspected$.fields[[l]]$as_field() == 
							x$result[i[k], j]
						)
					
					for (l in x$introspected$.key)
						update$where(
							x$introspected$.fields[[l]]$as_field() == keys[k, l]
						)
					
					x$pending <- c(x$pending, update)
				}
			}
			if (!is.null(x$get_option('flush.time'))) {
				if (is.null(x$get_option('last.flush')))
					x$set_option(flushed = as.numeric(Sys.time()))
				else {
					delta <- as.numeric(Sys.time()) - x$get_option('last.flush')
					if (delta >= x$get_option('flush.time'))
						x$flush()
				}
			}
			
			if (!is.null(x$get_option('flush.count'))) {
				if (x$get_option('flush.count') >= length(x$pending))
					x$flush()
			}
		}
		x
	}
)
#' \code{\link{rbind}} rows to a \code{\link{Result}} object, 
#' adding them to it's list of pending mutations.
rbind.Result <- function(result, ..., deparse.level = 1) {
	warning(
		'Calling "rbind" on objects of class "Result" is not yet supported.'
	)
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
		'* completed: ', x$is_finished(), 
		', affected: ', x$get_affected_count(), ' rows', sep = ''
	)
	
	if (x$get_option('mutable')) 
		cat(', pending: ', length(x$pending), ' mutations', sep = '')
	
	cat('.\n', sep = '')
})

#' Nicely format a \code{\link{Result}} object, 
#' displaying state and the underlying result.
#' 
#' @param x a \code{\link{Result}} object
#' @return \code{NULL}, invisibly.
setMethod('show', 'Result', function(object) print(object))

#' Get the dimensions of a \code{\link{Result}} object.
#' 
#' @param x a \code{\link{Result}} object
#' @return the dimensions (number of rows and columns) of the result.
setMethod('dim', 'Result', function(x) c(nrow(x$result), ncol(x$result)))

#' Get the number of columns of a \code{\link{Result}} object.
#' 
#' @param x a \code{\link{Result}} object
#' @return the number of columns.
setMethod('length', 'Result', function(x) length(x$result))
