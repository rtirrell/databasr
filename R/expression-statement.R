#' Aliasing of statements?
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
			callSuper()
		},
		
		restore = function() {
			.self
		},
		
		# I think that ORDER could also be moved to the superclass?
		limit = function(n) {
			if (is.null(.children$limit)) .children$limit <<- LimitClause$new(.self)
			.children$limit$set_children(n)
			.self
		},
		
		offset = function(n) {
			if (is.null(.children$offset)) .children$offset <<- OffsetClause$new(.self)
			.children$offset$set_children(n)
			.self
		},
		
		
		SQL = function() {
			prepare()
			
			formatter <- Formatter$new()
			compiler <- MySQLCompiler$new(.self, formatter)
			statement <- compiler$compile()
			
			restore()
			statement
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
			.children$update$add_child(table)
		},
		
		set = function(...) {
			.children$update$add_children(...)
			return(.self)
		},
		
		where = function(...) {
			.children$where$add_children(...)
			return(.self)
		}
	)
)


# TODO: consider closure function to do the dumb child-adding work. Also, support named children
# and handling of setting parents.
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
			select(...)
		},
		
		select = function(...) {
			.children$select$add_children(...)
			.self
		},
		
		from = function(...) {
			.children$from$add_children(...)
			.self
		},
		
		join = function(...) {
			if (is.null(.children$joins)) .children$joins <<- ClauseList$new(.self)
			join.clause <- JoinClause$new(.children$joins)
			.children$joins$add_child(join.clause$add_children(...))
			.self
		},
		
		#' TODO: the logic for the next four below is the same in every case. 
		where = function(...) {
			if (!'where' %in% names(.children)) .children$where <<- WhereClause$new(.self)
			.children$where$add_children(...)
			.self
		},
		
		group = function(...) {
			if (is.null(.children$group)) .children$group <<- GroupClause$new(.self)
			.children$group$add_children(...)
			.self
		},
		
		having = function(...) {
			if (is.null(.children$having)) .children$having <<- HavingClause$new(.self)
			.children$having$add_children(...)
			.self
		},
		
		order = function(...) {
			if (is.null(.children$order)) .children$order <<- OrderClause$new(.self)
			.children$order$add_children(...)
			.self
		},
		
		distinct = function() {
			.children$select$set_options(DISTINCT = TRUE)
			.self
		},
		
		all = function(...) {
			execute(...)$all()
		},
		
		first = function(...) {
			execute(...)$first()
		},
		
		one = function(...) {
			execute(...)$one()
		},
		
		# TODO: potential design decision here. prepare is database-agnostic preparation.
		# Could also add, dbPrepare for database-specific preparation. For dbPrepare, some sort 
		# of rider responsible for database-specific behavior tags along as we walk down the tree.
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
		
		#' If this statement has no session, then a session must be the first argument to execute.
		execute = function(...) {
			args <- list(...)
			if (is.null(session)) {
				session.arg <- args[[1]]
				args <- args[-1]
			} else session.arg <- session
			do.call(Result$new, c(session = session.arg, statement = .self, args))
		}
		
	)
)

# This stuff needs some more thought and is untested.

#' Generate a new `SELECT`, unbound to any session.
#' 
#' For example, to 
select <- function(...) {
	SelectStatement$new()$select(...)
}


OperFunStatement <- setRefClass('OperFunStatement',
	contains = c(
		'Statement'
	),
	methods = list(
		initialize = function() {
			callSuper()
		}
	)
)

OperatorStatement <- setRefClass('OperatorStatement',
	contains = c(
		'OperFunStatement'
	),
	fields = c(
		'operator'
	),
	methods = list(
		initialize = function(operator = NULL, left = NULL, right = NULL) {
			initFields(operator = operator)
			callSuper()
			add_children(left, right)
		}
	)
)

