#' Base class to support attaching options to an instance. 
# 
#' \code{\link{setOptions}} and \code{\link{getOption}} are not generally intended for end users 
#' of the package. When such a need would exist, we write a wrapper around each.
DatabasrObject <- setRefClass('DatabasrObject',
	fields = c(
		'.options'
	),
	methods = list(
		initialize = function() {
			initFields(.options = list())
		},
		
		#' Get a single option by name.
		getOption = function(name) {
			.options[[name]]
		},
		
		#' Set one or more options, as name = value pairs.
		setOptions = function(...) {
			opts <- list(...)
			opt.names <- names(opts)
			for (i in seq_along(opts)) .options[[opt.names[i]]] <<- opts[[i]]
			.self
		},
		
		#' Get a counter by name - create with value 1 if it does not exist, increment otherwise.
		getCounter = function(counter.name) {
			if (!counter.name %in% names(.options)) .options[[counter.name]] <<- 1
			else .options[[counter.name]] <<- .options[[counter.name]] + 1
		}
		
	)
)

# Base class encapsulating objects representing query constructs.
SQLObject <- setRefClass('SQLObject',
	contains = c(
		'DatabasrObject'
	),
	fields = c(
		'.parent',
		'.children'
	),
	methods = list(
		initialize = function(parent = NULL) {
			initFields(.parent = parent, .children = list())
			callSuper()
		},
		
		#' Set the parent of this node.
		setParent = function(parent) {
			.parent <<- parent
			.self
		},
		
		#' Return TRUE if this node has any children, FALSE otherwise.
		hasChildren = function() {
			length(.children) != 0
		},
		
		#' Find children of this node of a given class. If self is TRUE, also include the node itself
		#' in the search.
		findChildren = function(class, self = FALSE) {
			class.children <- list()	
			
			if (self && inherits(.self, class)) class.children <- c(class.children, .self)
			
			for (child in .children) {
				# TODO: should this be an either-or situation? As of now I can't think of a use-case for
				# needing every child where a given class may contain other objects of that class.
				if (inherits(child, class)) class.children <- c(class.children, child)
				else if (inherits(child, 'SQLObject') && child$hasChildren()) 
					class.children <- c(class.children, child$findChildren(class))
			}
			return(class.children)
		},
		
		#' Set the children of this node to the given varargs.
		setChildren = function(...) {
			.children <<- list()
			addChildren(...)
		},
		
		#' Insert a child into this node's children at the given index, pushing others to the right.
		#' 
		#' @param child object to insert
		#' @param where location to insert at (pushing other children back)
		#' @return \code{.self}
		insertChild = function(child, where = length(.children)) {
			.children <<- append(.children, child, after = where - 1) 
			if (inherits(child, 'SQLObject')) child$setParent(.self)
			.self
		},
		
		#' Add a child to this node's children at the end, with an optional name.
		#' 
		#' @param child object to insert
		#' @param name name of the object
		#' @return \code{.self}
		addChild = function(child, name = NULL) {
			if (is.null(name)) .children[[length(.children) + 1]] <<- child
			else .children[[name]] <<- child
			if (inherits(child, 'SQLObject')) child$setParent(.self)
			.self
		},
		
		#' Add children to this node's children at the end. 
		#' 
		#' @param ... either a single list or a series of children to add in order.
		addChildren = function(...) {
			args <- list(...)
			
			# Handle SELECT when using with(Table), which passes a list, 
			# and WHERE, which passes a list if children are being joined to existing children
			# by some boolean operator.
			if (length(args) == 1 && is.list(args[[1]])) args <- unlist(args)
			arg.names <- names(args)
			for (i in seq_along(args)) addChild(args[[i]], arg.names[[i]])
			.self
		},
		
		#' Prepare all children of this node that inherit from \code{SQLObject}.
		prepare = function() {
			for (child in .children) 
				if (inherits(child, 'SQLObject')) child$prepare()
			.self
		}
	)
)
