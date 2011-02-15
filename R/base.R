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
			return(callSuper())
		},
		
		##
		# Tree-related functionality.
		##
		hasChildren = function() {
			return(length(.children) != 0)
		},
		
		isEmpty = function() {
			return(!hasChildren())
		},
		
		setParent = function(parent) {
			.parent <<- parent
			return(.self)
		},
		
		findChildren = function(class) {
			class.children <- list()	
			for (child in .children) {
				# Should this be an either-or situation?
				if (inherits(child, class)) 
					class.children <- c(class.children, child)
				else if (inherits(child, 'SQLObject') && child$hasChildren()) 
					class.children <- c(class.children, child$findChildren(class))
			}
			return(class.children)
		},
		
		insertChild = function(child, where = length(.children)) {
			if (missing(where))
				where <- length(.children)
			.children <<- append(.children, child, after = where - 1) 
		},
		
		addChildren = function(...) {
			for (child in list(...)) {
				.children <<- c(.children, child)
				if (inherits(child, 'SQLObject')) child$setParent(.self)
			}
			return(.self)
		},
		
		prepare = function() {
			for (child in .children) 
				if (inherits(child, 'SQLObject')) child$prepare()
			return(.self)
		},
		
		propagateOptions = function(...) {
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
