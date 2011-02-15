Statement <- setRefClass('Statement',
	contains = c(
		'SQLObject'
	),
	
	fields = c(
		'session'
	),
	
	methods = list(
		initialize = function(session = NULL) {
			initFields(session = session)
			return(callSuper())
		}
	)
)


UpdateStatement <- setRefClass('UpdateStatement',
	contains = c(
		'Statement'
	),
	methods = list(
		initialize = function(session = NULL) {
			callSuper(session = session)
			.children <<- list(
				update = UpdateClause$new(.self),
				where = WhereClause$new(.self)
			)
			return(.self)
		},
		update = function(table) {
			.children$update$.children[[1]] <<- table
			return(.self)
		},
		set = function(...) {
			.children$update$addChildren(...)
			return(.self)
		},
		where = function(...) {
			.children$where$addChildren(...)
			return(.self)
		},
		SQL = function() {
			compiler <- Compiler$new()
			lines <- compile(.self, compiler, NULL)
			statement <- compiler$finish(lines)
			#debug(logger, statement)
			return(compiler$finish(lines))
		}
	)
)


SelectStatement <- setRefClass('SelectStatement',
	contains = c(
		'Statement'
	),
	
	fields = c(
		'unprepared.children'
	),
	
	methods = list(
		initialize = function(session = NULL, ...) {
			callSuper(session = session)
			.children <<- list(
				select = SelectClause$new(.self), 
				from = FromClause$new(.self)
			)
			return(select(...))
		},
		select = function(...) {
			.children$select$addChildren(...)
			return(.self)
		},
		
		from = function(...) {
			.children$from$addChildren(...)
			return(.self)
		},
		
		join = function(...) {
			if (!'joins' %in% names(.children)) .children$joins <<- ClauseList$new(.self)
			join.clause <- JoinClause$new(.self)
			.children$joins$addChildren(join.clause$addChildren(...))
			return(.self)
		},
		
		where = function(...) {
			if (!'where' %in% names(.children)) .children$where <<- WhereClause$new(.self)
			.children$where$addChildren(...)
			return(.self)
		},
		
		group = function(...) {
			if (!"group" %in% names(.children)) .children$group <<- GroupClause$new(.self)
			.children$group$addChildren(...)
			return(.self)
		},
		
		having = function(...) {
			if (!"having" %in% names(.children)) .children$having <<- HavingClause$new(.self)
			.children$having$addChildren(...)
			return(.self)
		},
		
		order = function(...) {
			if (!'where' %in% names(.children)) .children$where <<- WhereClause$new(.self)
			.children$order$addChildren(...)
			return(.self)
		},
		
		limit = function(n) {
			if (!'limit' %in% names(.children)) .children$limit <<- LimitClause$new(.self)
			.children$limit$addChildren(n)
			return(.self)
		},
		
		offset = function(n) {
			if (!'offset' %in% names(.children)) .children$offset <<- OffsetClause$new(.self)
			.children$offset$addChildren(n)
			return(.self)
		},
		
		distinct = function() {
			.children$select$setOptions(distinct = TRUE)
			return(.self)
		},
		count = function() {
			.children$select$setOptions(count = TRUE)
			return(.self)
		},
		
		# TODO: potential design decision here. prepare is database-agnostic preparation.
		# Could also add, dbPrepare for database-specific preparation. # For dbPrepare, some sort 
		# of rider responsible for database-specific behavior tags along as we walk down the tree.
		# Also want to consider interaction with compile* functions and their role in schemes
		# specific to any database.
		prepare = function() {
			unprepared.children <<- .children
			prepared.children <- list()
			
			for (clause.name in .CLAUSE.ORDER) {
				unprepared.child <- unprepared.children[[clause.name]]
				if (!is.null(unprepared.child)) 
					prepared.children[[clause.name]] <- unprepared.child$prepare()
			}
			
			.children <<- prepared.children
		},		
		
		restore = function() {
			.children <<- unprepared.children
		},
		
		execute = function(...) {
			result <- Result$new(session = session, statement = .self, ...)
			return(result)
		},
		
		SQL = function() {
			prepare()
			compiler <- Compiler$new()
			lines <- compile(.self, compiler, NULL)
			statement <- compiler$finish(lines)
			#debug(logger, statement)
			restore()
			
			return(statement)
		}
	)
)

setMethod('[', c('SelectStatement', 'ANY', 'ANY'), function(x, i, ...) {
	return(x$limit(i[length(i)] - i[1] + 1)$offset(i[1] - 1))
})

Transaction <- setRefClass('Transaction',
	contains = c(
		'SQLObject'
	),
	methods = list(
		initialize = function(session) {
			initFields(session = session)
			callSuper()
		}
	)
)
