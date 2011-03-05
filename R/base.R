#' Base class to support attaching options to an instance. 
DatabasrObject <- setRefClass('DatabasrObject',
	fields = c(
		'.options'
	),
	
	methods = list(
		initialize = function() {
			initFields(.options = list())
		},
		
		#' Get a single option by name.
		get_option = function(name) {
			.options[[name]]
		},
		
		#' Set one or more options, as name = value pairs.
		set_options = function(...) {
			opts <- list(...)
			opt.names <- names(opts)
			
			for (i in seq_along(opts)) 
				.options[[opt.names[i]]] <<- opts[[i]]
			
			.self
		},
		
		#' Get a counter by name - create with value 1 if it does not exist, 
		#' increment otherwise.
		get_counter = function(counter.name) {
			if (!counter.name %in% names(.options)) 
				.options[[counter.name]] <<- 1
			else 
				.options[[counter.name]] <<- .options[[counter.name]] + 1
		}
		
	)
)

#' Base class encapsulating objects representing query constructs.
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
		set_parent = function(parent) {
			.parent <<- parent
			.self
		},
		
		#' Return TRUE if this node has any children, FALSE otherwise.
		has_children = function() {
			length(.children) != 0
		},
		
		#' Find children of this node of a given class. 
		#' If self is TRUE, also include the node itself in the search.
		find_children = function(class, self = FALSE) {
			class.children <- list()	
			
			if (self && inherits(.self, class)) 
				class.children <- c(class.children, .self)
			
			for (child in .children) {
				# TODO: should this be an either-or situation? 
				# As of now I can't think of a use-case for needing every child 
				# where a given class may contain other objects of that class.
				if (inherits(child, class)) 
					class.children <- c(class.children, child)
				else if (inherits(child, 'SQLObject') && child$has_children()) 
					class.children <- c(class.children, child$find_children(class))
			}
			class.children
		},
		
		#' Set the children of this node to the given varargs.
		set_children = function(...) {
			.children <<- list()
			add_children(...)
		},
		
		#' Add a child to this node's children at the end, with an optional name.
		#' 
		#' @param child object to insert
		#' @param name name of the object
		#' @return \code{.self}
		add_child = function(child, name = NULL, after) {
			# I'd really only like to test for missingness.
			if (missing(name) || is.null(name)) {
				to.append <- list(child)
			} else {
				to.append <- list()
				to.append[[name]] <- child
			}
			
			if (missing(after))
				after <- length(.children)
			.children <<- append(.children, to.append, after)
			
			if (inherits(child, 'SQLObject')) 
				child$set_parent(.self)
			
			.self
		},
		
		#' Add children to this node's children at the end. 
		#' 
		#' @param ... either a single list or a series of children to add in order.
		add_children = function(...) {
			args <- list(...)
			
			# Handle SELECT when using with(Table), which passes a list, 
			# and WHERE, which passes a list if children are 
			# being joined to existing children by some boolean operator.
			if (length(args) == 1 && is.list(args[[1]])) 
				args <- unlist(args)
			arg.names <- names(args)
			
			for (i in seq_along(args)) 
				add_child(args[[i]], arg.names[[i]])
			
			.self
		},
		
		#' Prepare all children of this node that inherit from \code{SQLObject}.
		prepare = function() {
			for (child in .children) {
				if (inherits(child, 'SQLObject')) 
					child$prepare()
			}
			.self
		}
	)
)
