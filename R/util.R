# Performs much better for character versus the below. Is marginally slower for numeric types.
haveSameElements <- function(v1, v2) {
	if (length(v1) != length(v2)) FALSE
	else length(intersect(v1, v2)) == length(v1)
}

countCharacter <- function(character, ...) {
	sapply(strsplit(unlist(list(...)), '', fixed = TRUE), function(value) {
		sum(value == character)
	})
}

haveSameElementsTwo <- function(v1, v2) {
	all.equal(sort(v1), sort(v2))
}

doSampleInt <- function() {
	sample.int(seq.int(100 * 1000), 10 * 1000 * 1000, replace = TRUE)
}

doSampleLetters <- function() {
	sample(LETTERS, 1000 * 1000, replace = TRUE)
}
	
timeSames <- function() {
	list(one = system.time(replicate(10, {
			t <- doSampleLetters()
			s <- doSampleLetters()
			haveSameElements(t, s)
		})),
	two = system.time(replicate(10, {
		t <- doSampleLetters()
		s <- doSampleLetters()
		haveSameElementsTwo(t, s)
	})))
}