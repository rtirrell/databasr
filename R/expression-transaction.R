
Transaction <- setRefClass("Transaction",
	contains = c(
		"SQLObject"
	),
	methods = list(
		initialize = function(session) {
			initFields(session = session)
			callSuper()
		},
		add = function(element) {
			add_child(element)
		}
	)
)