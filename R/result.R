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
#' A result is finished when dbHasCompleted returns TRUE.
#' TODO: getting all could just use dbReadTable.
#' 
#' @name Result
#' @exportClass
Result <- setRefClass("Result",
	contains = c(
		"DatabasrObject"
	),
	fields = c(
		"session",
		"connection",
		"statement",
		"SQL",
		"result.set",
		"result",
		# This is more aptly named "table".
		"introspected",
		"pending"
	),
	methods = list(
		#' Initialize the result for a given session and statement.
		#' 
		#' TODO: we can pull fetch.size from driver options.
		initialize = function(session, statement, fetch.size = NULL, mutable = FALSE) {
			initFields(
				session = session, connection = NULL, SQL = statement$SQL(),
				result.set = NULL, result = NULL, introspected = NULL, pending = list()
			)
			callSuper()
			if (is.null(fetch.size)) fetch.size <- session$getOption("fetch.size")
			setOptions(fetch.size = fetch.size, started = FALSE, finished = FALSE)
			checkMutable(statement, mutable)
			return(.self)
		},
		
		sendQuery = function() {
			#debug(logger, SQL)
			setOptions(started = TRUE)
			connection <<- session$request("result")
			result.set <<- dbSendQuery(connection$connection, SQL)
		},
		
		# Get all results for this query. If the result is marked mutable, then we return this object.
		# Otherwise, we return the underlying data frame.
		all = function() {
			get(-1)
			if (!getOption('mutable')) return(getResult())
			return(.self)
		},
		
		getAll = function() {
			all()
		},
		
		getResult = function() {
			finish()
			return(result)
		},
		
		isStarted = function() {
			return(getOption('started'))
		},
		
		isFinished = function() {
			return(getOption('finished'))
		},
		
		getAffectedRowCount = function() {
			if (isFinished()) return(getOption('affected.row.count'))
			else if (isStarted()) return(dbGetRowCount(result.set))
			return(NA)
		},
		
		getStatement = function() {
			return(str_c(SQL, '\n'))
		},
		
		#' Fetch results from the result set.
		#' 
		#' @param n number of results to fetch (see documentation for \code{\link{Result}} for
		#'   more information on the default for this, which is a class-level field)
		#' @param retain whether to append to the old result or discard it completely
		get = function(n, retain = TRUE) {
			if (missing(n)) n <- getOption('fetch.size')
			
			if (!isStarted()) sendQuery()
			
			if (!is.null(result) && retain) {
				if (n == -1) {
					old.result <- result
					result <<- fetch(result.set, n)
					result[(nrow(result) + 1):(nrow(result) + nrow(old.result)), ] <<- old.result
				} else {
					new.result <- fetch(result.set, n)
					result[1:nrow(new.result) + nrow(result), ] <<- new.result
				}
			} else result <<- fetch(result.set, n)
			
			if (dbHasCompleted(result.set) && !getOption("mutable")) return(getResult())
			return(.self)
		},
		
		isDirty = function() {
			return(length(pending) != 0)
		},
		
		finish = function() {
			if (getOption("finished")) {
				#warning('Result has already been finished.')
			} else {
				
				if (isDirty()) warning('Finishing result with pending mutations.')
				pending <<- list()
				
				setOptions(affected.row.count = getAffectedRowCount())
				
				if (getOption("started")) {
					dbClearResult(result.set)
					session$release(connection)
				}
				setOptions(finished = TRUE)
			}
		},
		
		
		# I'm not sure when to use dbCommit, and when to send the equivalent query.
		flush = function() {
			flush.connection <- session$request('flush')
			dbSendQuery(flush.connection$connection, 'START TRANSACTION;')
			for (mutation in pending) {
				mutation.SQL <- mutation$SQL()
				#debug(session, mutation.SQL)
				dbSendQuery(flush.connection$connection, mutation.SQL)
			}
			
			dbSendQuery(flush.connection$connection, 'COMMIT;')
			pending <<- list()
			session$release(flush.connection)
		},
		
		
		# If the user believes the result for this query is mutable, we don't -- check first.
		checkMutable = function(statement, mutable) {
			setOptions(mutable = mutable)
			if (mutable) {
				stop.message <- NULL
				statement$prepare()
				
				from.clause <- statement$.children$from
				join.clauses <- statement$.children$joins
					
				if (!is.null(from.clause) && length(from.clause$tables) > 1) 
					stop.message <- 'Cannot alter result FROM multiple tables.'
				else if (!is.null(join.clauses) && join.clauses$hasChildren())
					stop.message <- 'Cannot alter JOIN result.'
				introspected <<- from.clause$tables[[1]]
				
				table.keys <- sapply(introspected$.fields[introspected$.key], function(k) k$name)
				select.keys <- sapply(statement$.children$select$.children, function(k) k$name)
				if (length(introspected$.key) == 0 || !haveSameElements(table.keys, select.keys))
					stop.message <- 'Cannot alter result lacking complete primary key.'

				statement$restore()
				if (!is.null(stop.message)) stop(stop.message)
				setOptions(mutable = TRUE)
			}
		},
		
		finalize = function() {
			finish()
		}
	)
)


`[.Result` <- function(result, ...) {
	if (!result$isStarted()) result$get()
	return(`[.data.frame`(result$result, ...))
}

#' Access a field in the underlying data frame.
#' 
#' TODO: as far as I know, there's no way to allow result.object$field[10] <- 4 to be intercepted
#'   to the tune of ("field", 4). We could also overload `[[<-` to allow simpler (e.g., 
#'   "[['field']] <- value" column-wise replacement, versus "[, 'field'] <- value".
setMethod('$', 'Result', function(x, name) {
	if (name %in% names(x[['result']])) x[['result']][, name]
	else findMethods('$')$envRefClass(x, as.character(name))
})

#' Replace value(s) in the underlying data frame.
#' 
#' TODO: adding rows and using dbWriteTable.
`[<-.Result` <- function(result, i, j, value) {
	if (!result$isStarted()) 
		stop(str_c(
			'Attempting to modify result that has not been populated.',
			'Either access the result to trigger fetching or get() manually.', sep = ' '
		))
		
	if (missing(i)) i = seq_len(nrow(result$result))
	if (missing(j)) j = seq_along(result$result)
	
	if (result$getOption("mutable")) {
		if (i[length(i)] > nrow(result$result)) {
			
		}
		keys <- result$result[i, result$introspected$.key, drop = FALSE]
	}
	
	result$result <- `[<-.data.frame`(result$result, i, j, value)
	
	if (result$getOption("mutable")) {
		if (i[length(i)] > nrow(result$result)) {
			# PendingInsert: keep track of the indices and force flushing when new result is fetched
			# if retain is false.
		} else {
			for (k in seq_along(i)) {
				update <- UpdateStatement$new()
				update$update(result$introspected$asTable())
				for (l in j) 
					update$set(result$introspected$.fields[[l]]$asField() == result$result[i[k], j])
				for (l in result$introspected$.key)
					update$where(result$introspected$.fields[[l]]$asField() == keys[k, l])
				
				result$pending <- c(result$pending, update)
			}
		}
	}
	return(result)
}
#' \code{\link{rbind}} rows to a \code{\link{Result}} object, queueing them for insertion.
rbind.Result <- function(result, ..., deparse.level = 1) {
	warning("Calling 'rbind' on objects of class 'Result' is not yet supported.")
	return(result)
}

#' Nicely format a \code{\link{Result}} object, displaying state and underyling result.
#' 
#' @param x the object to print.
#' @param nrows the number of rows of the result to display. 
#' @return \code{NULL}, invisibly.
setMethod('print', 'Result', function(x, nrows = 10, ...) {
	cat("<An object of class 'Result'>\n")
	if (is.null(x$result) || nrow(x$result) == 0) {
		if (x$isStarted()) cat('* No results for this query.\n')
		else cat('* No results have been fetched for this query.\n')
	} else {
		if (nrows < nrow(x$result)) {
			print(x$result[1:nrows, ])
			cat('* Displaying first', nrows, 'of', nrow(x$result), 'rows.\n')
		} else {
			print(x$result)
			cat('* Displaying all', nrow(x$result), 'rows.\n')
		}
	}
	cat('* completed: ', x$isFinished(), ', affected: ', x$getAffectedRowCount(), ' rows.', sep = '')
	if (x$getOption("mutable")) cat(', pending: ', length(x$pending), ' mutations', sep = '')
	cat('.\n', sep = '')
})
setMethod('show', 'Result', function(object) print(object))
setMethod('dim', 'Result', function(x) return(c(nrow(x$result), ncol(x$result))))
