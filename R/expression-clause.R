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
		
		
		addTable = function(table) {
			if (!hasTable(table)) tables <<- c(tables, table)
			.self
		},
		addTables = function(...) {
			for (table in list(...)) addTable(table)
			.self
		},
		
		hasTables = function() {
			length(tables) > 0
		},
		
		hasTable = function(table) {
			if (all(sapply(tables, function(t) !table$equals(t)))) FALSE
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
			unlist(sapply(.children, function(c) c$getTableNames()))
		},
		hasTable = function(table) {
			any(unlist(sapply(.children, function(c) c$hasTable(table))))
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
		addChild = function(child, name)  {
			fields <- list()
			if (inherits(child, "IntrospectedTable")) {
				for (field in child$.fields) callSuper(field$asField())
				addTable(child)
			} else if (inherits(child, "Field")) {
				callSuper(child)
				addTable(child$table)
			} else {
				# Otherwise, child is some expression. Extract the Fields and add their tables.
				child.fields <- child$findChildren("Field")
				for (field in child.fields) addTables(field$table)
				callSuper(child)
			}
		},
		
		#' If field names are unique, prefer using shorter aliases.
		#' 
		#' TODO: could have a generic "apply function to children".
		prepare = function() {
			field.names <- c()
			for (field in findChildren('Field')) {
				# Operator and function fields handle uniquely aliasing at compile-time.
				if (!inherits(field$.parent, 'OperFunElement')) field.names <- c(field.names, field$name)
			}
			if (length(field.names) == length(unique(field.names))) setOptions(short.alias = TRUE)
			
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
		addChild = function(child, name) {
			addTable(child)
			callSuper(child$asTable())
		},
		
		# Add any table named in select that is not in joins to the from clause. We may also want to
		# look at the where clause.
		prepare = function() {
			table.names <- getTableNames()
			if (!is.null(.parent$.children$joins))
				table.names <- c(table.names, .parent$.children$joins$getTableNames())

			clause.tables <- .parent$.children$select$tables
			
			if (!is.null(.parent$.children$where))
				clause.tables <- c(clause.tables, .parent$.children$where$tables)
			
			for (clause.table in clause.tables) {
				if (!clause.table$getName() %in% table.names) {
					if (!hasTable(clause.table)) {
						addChild(clause.table)
					}
				}
			}
			
			return(callSuper())
		}
	)
)

#' Represents a JOIN clause.
#' 
#' TODO: need to think about how to handle self-joins automatically. Lots of things won't work
#' as-is.
JoinClause <- setRefClass("JoinClause",
	contains = c(
		"Clause"
	),
	fields = c(
		"type"
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		},
		
		# Also give name of field as string for JOIN USING?
		addChildren = function(...) {
			callSuper(...)
			args <- list(...)
			
			if (inherits(args[[1]], "SelectableElement")) {
				table <- args[[1]]$findChildren("Field", TRUE)[[1]]$table
				addTable(table)
				# TODO: this should probably be added in prepare. At that point, we can tell if this is the
				# correct table (think JOIN ON CONCAT(other.table, this.table)).
				insertChild(table$asTable(), 1)
			}
			.self
		},
			
			
		# Shorthand syntax for JOIN USING gives only fields. In this case, we take the table
		# the first argument (all arguments must be from the same table).
		# Shorthand syntax for JOIN ON gives only an expression composed of fields. In this case,
		# we take the table of the leftmost field.
		#' TODO: make type a field, rather than an option.
		addChild = function(child, name) {
			# This sets the type based on the last element we see -- which is what we want.
			if (inherits(child, "IntrospectedTable")) {
				type <<- "NATURAL JOIN"
				addTable(child)
				child <- child$asTable()
			} else if (inherits(child, "Field")) {
				type <<- "JOIN USING"
			} else type <<- "JOIN ON"
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
		addChildren = function(...) {
			args <- list(...)
			
			# Join this clause to an existing one. Then we pass a list - which addChildren handle.
			if (length(.children) != 0) {
				.children[[length(.children)]] <<- BinaryOperator$new(
					"AND", .children[[length(.children)]], args[[1]]
				)$setParent(.self)
				
				if (length(args) > 1) args <- args[2:length(args)]
				else return(.self)
			}
			callSuper(args)
		},
		addChild = function(child, name) {
			fields <- child$findChildren("Field")
			lapply(fields, function(f) addTable(f$table))
			callSuper(child, name)
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
		#' \code{\link{findChildren}} would incorrectly return those.
		addChild = function(child, name) {
			if (inherits(child, "IntrospectedTable")) {
				insertChild(child$asTable(), 1)
				addTables(child)
			} else callSuper(child, name)
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