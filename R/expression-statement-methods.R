##
# Modifiers.
##

setMethod("[", c("Statement", "ANY", "ANY"), function(x, i, ...) {
	if (i[length(i)] == -1) x$offset(i[1] - 1)
	else x$limit(i[length(i)] - i[1] + 1)$offset(i[1] - 1)
})

##
# Functions.
##
setMethod("exists", "SelectStatement", function(x) {
		FunctionStatement$new("EXISTS", x)
	})	

##
# Operators.
##
setMethod("|", c("SelectStatement", "SelectStatement"), function(e1, e2) {
		OperatorStatement$new("UNION", e1, e2)
	})

setMethod("&", c("SelectStatement", "SelectStatement"), function(e1, e2) {
		OperatorStatement$new("INTERSECT", e1, e2)
	})

setMethod("-", c("SelectStatement", "SelectStatement"), function(e1, e2) {
		OperatorStatement$new("EXCEPT", e1, e2)
	})
