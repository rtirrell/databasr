#' Represents a statement in SQL.
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
			if (is.null(.children$limit)) 
				.children$limit <<- LimitClause$new(.self)
			.children$limit$set_children(n)
			.self
		},
		
		offset = function(n) {
			if (is.null(.children$offset)) 
				.children$offset <<- OffsetClause$new(.self)
			.children$offset$set_children(n)
			.self
		},
		
		
		sql = function() {
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

#' Class representing an \code{SELECT} statement.
#'
#' TODO: consider closure function to do the dumb child-adding work. 
#' Also, support named children and handling of setting parents.
SelectStatement <- setRefClass('SelectStatement',
	contains = c(
		'Statement'
	),
	
	fields = c(
		#' A \code{list} to which \code{.children} is saved before 
		#' \code{prepare()} is called.
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
		
		#' Add elements to the statement's \code{SELECT} clause.
		#' @param ... arguments added as children to the \code{SELECT} clause.
		#' @return .self
		select = function(...) {
			.children$select$add_children(...)
			.self
		},
		
		#' Add elements to the statement's \code{FROM} clause.
		#' @return .self
		from = function(...) {
			.children$from$add_children(...)
			.self
		},
		
		#' Add a new \code{JOIN} clause to the statement's 
		#' \code{{JOIN} clauses.
		#' 
		#' \code{JOIN} clauses are contained in a \code{\link{ClauseList}} object.
		#' 
		#' @param ... arguments added as children to the new \code{JOIN} clause.
		#' @return .self
		join = function(...) {
			if (is.null(.children$joins)) 
				.children$joins <<- ClauseList$new(.self)
			
			join.clause <- JoinClause$new(.children$joins)
			.children$joins$add_child(join.clause$add_children(...))
			.self
		},
		
		#' Add elements to the statement's \code{WHERE} clause.
		#' 
		#' @param ... arguments added as children to the \code{WHERE} clause.
		#' 
		#' @return .self
		where = function(...) {
			if (is.null(.children$where)) 
				.children$where <<- WhereClause$new(.self)
			
			.children$where$add_children(...)
			.self
		},
		
		#' Add elements to the statement's \code{GROUP BY} clause.
		#' 
		#' @param ... arguments added as children to the \code{GROUP BY} clause.
		#' 
		#' @return .self
		group = function(...) {
			if (is.null(.children$group)) 
				.children$group <<- GroupClause$new(.self)
			
			.children$group$add_children(...)
			.self
		},
		
		#' Add elements to the statement's \code{HAVING} clause.
		#' 
		#' @param ... arguments added as children to the \code{HAVING} clause.
		#' 
		#' @return .self
		having = function(...) {
			if (is.null(.children$having)) 
				.children$having <<- HavingClause$new(.self)
			
			.children$having$add_children(...)
			.self
		},
		
		#' Add elements to the statement's \code{ORDER} clause.
		#' @param ... arguments passed to \code{execute()}.
		#' @return .self
		order = function(...) {
			if (is.null(.children$order)) 
				.children$order <<- OrderClause$new(.self)
			
			.children$order$add_children(...)
			.self
		},
		
		#' Set the \code{SELECT} clause to \code{SELECT DISTINCT}.
		#' @return .self
		distinct = function() {
			.children$select$set_options(DISTINCT = TRUE)
			.self
		},
		
		#' Compile and execute the statement, immediately returning all
		#' results.
		#' @param ... arguments passed to \code{execute()}.
		#' 
		#' @return a \code{\link{Result}} if the result is mutable, 
		#' a \code{\link{data.frame}} otherwise.
		all = function(...) {
			execute(...)$all()
		},
		
		#' Compile and execute the statement, immediately returning the first
		#' result.
		#' 
		#' @param ... arguments passed to \code{execute()}.
		#' 
		#' @seealso \code{Result#first}
		#' 
		#' @return a \code{\link{data.frame}} with one row.
		first = function(...) {
			execute(...)$first()
		},
		
		#' Compile and execute the statement, immediately returning the first
		#' result, after checking that exactly one row exists in the result set.
		#' 
		#' @seealso \code{Result#one}.
		#' 
		#' @return a \code{\link{data.frame}} with one row.
		one = function(...) {
			execute(...)$one()
		},
		
		#' Prepare this statement, infer tables, check for ambiguous field names, 
		#' and set options for the compiler
		#' 
		#' @return \code{NULL}, invisibly.
		prepare = function() {
			unprepared.children <<- .children
			for (clause.name in .CLAUSE.ORDER) {
				unprepared.child <- unprepared.children[[clause.name]]
				if (!is.null(unprepared.child)) 
					.children[[clause.name]] <<- unprepared.child$prepare()
			}
			
			invisible()
		},		
		
		#' Restore this statement to the state it was in 
		#' before \code{prepare()}} was called.
		#' 
		#' @return \code{NULL}, invisibly.
		restore = function() {
			.children <<- unprepared.children
			invisible()
		},
		
		#' If this statement has no session, then a session must be the 
		#' first argument to execute.
		#' 
		#' @param ... arguments passed to \code{\link{Result}}'s constructor.
		#' 
		#' @return a \code{\link{Result}} object.
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

# This stuff needs some more thought and is basically untested.

#' Generate a new \code{\link{SelectStatement}}, unbound to any session.
#' 
#' @return a \code{\link{SelectStatement}} object.
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

