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
		
		addTable = function(...) {
			for (table in list(...)) {
				if (!is.null(table) && !hasTable(table)) tables <<- c(tables, table)
			}
			.self
		},
		
		hasTables = function() {
			length(tables) > 0
		},
		
		hasTable = function(table) {
			if (all(sapply(tables, function(t) t != table))) FALSE
			else TRUE
		},
		
		getTableNames = function() {
			sapply(tables, function(t) t$getName())
		}
	)
)


ClauseList <- setRefClass('ClauseList',
	contains = c(
		'Clause'
	),
	
	methods = list(
		getTableNames = function() {
			return(unlist(sapply(.children, function(c) c$getTableNames())))
		}
	)
)

SelectClause <- setRefClass('SelectClause',
	contains = c(
		'Clause' 
	),
	methods = list(
		initialize = function(...) {
			return(callSuper(...))
		},
		
		addChildren = function(...) {
			fields <- list()
			
			for (child in list(...)) {
				if (inherits(child, 'Table')) {
					callSuper(child$.fields)
					addTable(child)
				} else if (inherits(child, 'Field')) {
					callSuper(child)
					addTable(child$table)
				} else {
					child.fields <- child$findChildren('Field')
					for (field in child.fields) {
						addTable(field$table)
					}
					callSuper(child)
				}
			}
			
			return(.self)
		},
		
		prepare = function() {
			field.names <- unlist(sapply(findChildren('Field'), function(c) c$name))
			if (length(field.names) == length(unique(field.names))) setOptions(short.alias = TRUE)
			
			return(callSuper())
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
		
		# Accepts only Tables and literals.
		addChildren = function(...) {
			for (child in list(...)) {
				if (inherits(child, 'Table')) addTable(child)
				callSuper(child)
			}
		},
		
		# Add any table named in select that is not in joins to the from clause. We may also want to
		# look at the where clause.
		prepare = function() {
			if (!is.null(.parent$.children$joins))
				join.table.names <- .parent$.children$joins$getTableNames()
			else join.table.names <- ''
			select.tables <- .parent$.children$select$tables
			table.names <- getTableNames()
			for (select.table in select.tables) {
				if (!select.table$getName() %in% join.table.names) {
					if (!select.table$getName() %in% table.names) addChildren(select.table)
				}
			}
			
			return(callSuper())
		}
	)
)

JoinClause <- setRefClass('JoinClause',
	contains = c(
		'Clause'
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		},
		
		# Also give name of filed as string for JOIN USING?
		addChildren = function(...) {
			args <- list(...)
			for (child in args) {
				if (inherits(child, 'Table')) 
					setOptions(type = 'NATURAL JOIN')$addTable(child)
				else if (inherits(child, 'Field'))
					setOptions(type = 'JOIN USING')
				else
					setOptions(type = 'JOIN ON')
				callSuper(child)
			}
			
			# Shorthand syntax for JOIN USING gives only fields. In this case, we take the table
			# the first argument (all arguments must be from the same table).
			# Shorthand syntax for JOIN ON gives only an expression composed of fields. In this case,
			# we take the table of the leftmost field.
			if (inherits(args[[1]], 'Field')) {
				addTable(args[[1]]$table)
				insertChild(args[[1]]$table, 1)
			} else if (inherits(args[[1]], 'Scalar')) {
				fields <- args[[1]]$findChildren('Field')
				addTable(fields[[1]]$table)
				insertChild(fields[[1]]$table, 1)
			}
				
			.self
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
		addChildren = function(...) {
			args <- list(...)
			
			# Join this clause to an existing one.
			if (length(.children) != 0) {
				.children[[length(.children)]] <<- BinaryOperator$new(
					"AND", .children[[length(.children)]], args[[1]]
				)
				
				if (length(args) > 1) args <- args[2:length(args)]
				else return(.self)
			}
			callSuper(args)
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