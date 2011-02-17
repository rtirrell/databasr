#' Base class to support attaching options to an instance. 
#' 
#' TODO: think about insert, add, etc..
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
		
		getOption = function(name) {
			return(.options[[name]])
		},
		
		setOptions = function(...) {
			opts <- list(...)
			opt.names <- names(opts)
			for (i in seq_along(opts)) .options[[opt.names[i]]] <<- opts[[i]]
			
			.self
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
		
		##
		# Tree-related functionality.
		##
		setParent = function(parent) {
			.parent <<- parent
			.self
		},
		
		
		hasChildren = function() {
			length(.children) != 0
		},
		
		findChildren = function(class) {
			class.children <- list()	
			for (child in .children) {
				# TODO: should this be an either-or situation? As of now I can't think of a use-case for
				# needing every child where a given class may contain other objects of that class.
				if (inherits(child, class)) 
					class.children <- c(class.children, child)
				else if (inherits(child, "SQLObject") && child$hasChildren()) 
					class.children <- c(class.children, child$findChildren(class))
			}
			return(class.children)
		},
		
		setChildren = function(...) {
			.children <<- list()
			addChildren(...)
		},
		
		#' Insert a child object into this object's \code{.children} at a given location.
		#' 
		#' @param child object to insert
		#' @param where location to insert at (pushing other children back)
		#' @return \code{.self}
		insertChild = function(child, where = length(.children)) {
			.children <<- append(.children, child, after = where - 1) 
			if (inherits(child, 'SQLObject')) child$setParent(.self)
			.self
		},
		
		#' Add a child object to this object's \code{.children} at the end, with an optional name.
		#' 
		#' @param child object to insert
		#' @param name name of the object
		#' @return \code{.self}
		addChild = function(child, name = NULL) {
			# TODO: prevent collapsing of arguments.
			if (is.null(name)) .children[[length(.children) + 1]] <<- child
			else .children[[name]] <<- child
			if (inherits(child, "SQLObject")) child$setParent(.self)
			.self
		},
			
		
		addChildren = function(...) {
			args <- list(...)
			# Handle WHERE, which may pass a list. I don't know how to do.call(callSuper).
			if (length(args) == 1 && is.list(args[[1]])) args <- unlist(args)
			arg.names <- names(args)
			for (i in seq_along(args)) addChild(args[[i]], arg.names[[i]])
			.self
		},
		
		prepare = function() {
			for (child in .children) 
				if (inherits(child, "SQLObject")) child$prepare()
			.self
		},
		
		#' Propagate options up or down the tree.
		#' 
		#' Note that \code{"all"} starts by setting to the root, at which point a full and mildly
		#' redundant downward propagation begins.
		#' 
		#' @param ... options to set on all successors or predecessors or both.
		#' @param to direction toward which to set these options.
		propagateOptions = function(..., to = "children") {
			setOptions(...)
			for (child in .children) {
				child$setOptions(...)
				if (inherits(child, 'Clause')) {
					child$propagateOptions(...)
				}
			}
		}
	)
)

#' Capitalize a string.
#' 
#' @param value a character vector of length one
#' @return the capitalized
#' @examples 
#' capitalize("hello world") # "Hello world"
capitalize <- function(value) {
	str_c(toupper(str_sub(value, 1, 1)), str_sub(value, 2))
}