# TODO: need all of the boilerplate initialize methods in children?
Clause <- setRefClass('Clause',
	contains = c(
		'SQLObject'
	),
	fields = c(
		'tables'
	),
	methods = list(
		initialize = function(...) {
			initFields(tables = list())
			callSuper(...)
		},
		
		
		add_table = function(table) {
			if (!has_table(table)) tables <<- c(tables, table)
			.self
		},
		add_tables = function(...) {
			for (table in list(...)) add_table(table)
			.self
		},
		
		has_tables = function() {
			length(tables) > 0
		},
		
		has_table = function(table) {
			any(vapply(tables, function(t) table$equals(t), logical(1)))
		},
		
		get_table_names = function() {
			vapply(tables, function(t) t$get_name(), character(1))
		}
	)
)


ClauseList <- setRefClass('ClauseList',
	contains = c(
		'Clause'
	),
	
	methods = list(
		get_table_names = function() {
			unlist(sapply(.children, function(c) c$get_table_names()))
		},
		has_table = function(table) {
			any(vapply(.children, function(c) c$has_table(table), logical(1)))
		}
	)
)

SelectClause <- setRefClass('SelectClause',
	contains = c(
		'Clause' 
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		},
		
		#' Add a child to this clause's children.
		#' 
		#' There are three cases depending on the class of the arguments.
		add_child = function(child, name, after)  {
			fields <- list()
			
			if (inherits(child, 'IntrospectedTable')) {
				# Child is a table, add all of it's IntrospectedFields as Fields.
				for (field in child$.fields) callSuper(field$as_field())
				add_table(child)
			} else if (inherits(child, 'Field')) {
				# Child is a field - add it's parent table to tables.
				callSuper(child, name, after)
				add_table(child$table)
			} else if (inherits(child, 'SQLObject')) {
				# Child is some expression. Extract the Fields and add their tables.
				child.fields <- child$find_children('Field')
				for (field in child.fields) add_tables(field$table)
				callSuper(child, name, after)
			} else {
				# Field is some literal element.
				callSuper(I(child))
			}
		},
		
		#' If field names are unique, prefer using shorter aliases.
		prepare = function() {
			field.names <- c()
			for (field in find_children('Field'))
				# Operator and function fields handle uniquely 
				# aliasing at compile-time.
				if (!inherits(field$.parent, 'OperFunElement')) 
					field.names <- c(field.names, field$name)
			
			field.counts <- table(field.names)
			set_options(full.alias = names(field.counts)[field.counts > 1])
			
			callSuper()
		}
	)
)

UpdateClause <- setRefClass('UpdateClause',
	contains = c(
		'Clause'
	)
)

FromClause <- setRefClass('FromClause',
	contains = c(
		'Clause'
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		},
		
		# Accepts only Tables.
		add_child = function(child, name, after) {
			add_table(child)
			callSuper(child$as_table())
		},
		
		# Add any table named in select that is not in joins to the from clause. 
		# We may also want to look at the where clause.
		prepare = function() {
			table.names <- get_table_names()
			if (!is.null(.parent$.children$joins))
				table.names <- c(table.names, .parent$.children$joins$get_table_names())

			clause.tables <- .parent$.children$select$tables
			
			if (!is.null(.parent$.children$where))
				clause.tables <- c(clause.tables, .parent$.children$where$tables)
			
			for (clause.table in clause.tables) {
				if (!clause.table$get_name() %in% table.names) {
					if (!has_table(clause.table)) {
						add_child(clause.table)
					}
				}
			}
			
			callSuper()
		}
	)
)

#' Represents a JOIN clause.
#' 
#' TODO: need to think about how to handle self-joins automatically. Lots of things won't work
#' as-is.
JoinClause <- setRefClass('JoinClause',
	contains = c(
		'Clause'
	),
	fields = c(
		'type'
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		},
		
		#' Add children to this JOIN clause.
		#' 
		#' This sets the type based on the last element we add.
		add_children = function(...) {
			args <- list(...)
			if (inherits(args[[1]], 'SelectableElement')) {
				table <- args[[1]]$find_children('Field', TRUE)[[1]]$table
				add_table(table)
				# TODO: this should probably be added in prepare. 
				# At that point, we can tell if this is the correct table 
				# (think JOIN ON CONCAT(other.table, this.table)).
				add_child(table$as_table(), 0)
			}
			callSuper(...)
		},
			
			
		#' Add a child to this JOIN clause.
		#' 
		#' Shorthand syntax for JOIN USING gives only fields. In this case, we take the table
		#' the first argument (all arguments must be from the same table).
		#' Shorthand syntax for JOIN ON gives only an expression. In this case,
		#' we take the table of the leftmost field.
		add_child = function(child, name, after) {
			if (inherits(child, 'IntrospectedTable')) {
				type <<- 'NATURAL JOIN'
				add_table(child)
				child <- child$as_table()
			} else if (inherits(child, 'Field')) {
				type <<- 'JOIN USING'
			} else type <<- 'JOIN ON'
			callSuper(child)
		}
	)
)

# TODO: check use of keyword arguments for internal consistency.
BindingClause <- setRefClass('BindingClause',
	contains = c(
		'Clause'
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		},
		add_children = function(...) {
			args <- list(...)
			
			# Join this clause to an existing one. 
			# Then we pass a list - which add_children handles.
			if (length(.children) != 0) {
				.children[[length(.children)]] <<- BinaryOperatorElement$new(
					'AND', .children[[length(.children)]], args[[1]]
				)$set_parent(.self)
				
				if (length(args) > 1) 
					args <- args[2:length(args)]
				else 
					return(.self)
			}
			callSuper(args)
		},
		add_child = function(child, name, after) {
			fields <- child$find_children('Field')
			sapply(fields, function(f) add_table(f$table))
			callSuper(child)
		}
	)
)

WhereClause <- setRefClass('WhereClause',
	contains = c(
		'BindingClause'
	)
)

HavingClause <- setRefClass('HavingClause',
	contains = c(
		'BindingClause'
	)
)

UpdateClause <- setRefClass('UpdateClause',
	contains = c(
		'Clause'
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		},
		
		#' Add a child to this clause.
		#' 
		#' Need to think a bit on whether we ever really need to keep tables. One situation
		#' where that might be simplest is if a clause has extensive nesting - then a naive
		#' \code{\link{find_children}} would incorrectly return those.
		add_child = function(child, name, after) {
			if (inherits(child, 'IntrospectedTable')) {
				callSuper(child$as_table(), after = 0)
				add_table(child)
			} else 
				callSuper(child)
		}
	)
)

GroupClause <- setRefClass('GroupClause',
	contains = c(
		'Clause'
	)
)

HavingClause <- setRefClass('HavingClause',
	contains = c(
		'Clause'
	)
)

OrderClause <- setRefClass('OrderClause',
	contains = c(
		'Clause'
	)
)

RestrictClause <- setRefClass('RestrictClause',
	contains = c(
		'Clause'
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		}
	)
)

LimitClause <- setRefClass('LimitClause',
	contains = c(
		'RestrictClause'
	)
)

OffsetClause <- setRefClass('OffsetClause',
	contains = c(
		'RestrictClause'
	)
)