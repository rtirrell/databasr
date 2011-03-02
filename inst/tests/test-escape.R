unescaped <- c(
	"\"I'm leaving\", she said.",
	"\"No! Please! I'm so sorry\", he cried.",
	"\"I can't forgive you -- what you did. Not now. Probably never.\"",
	"She went on. \"It's like you have... there are two of you. And I never know which.\""
)

escaped <- escape(session, unescaped)
correct.escaped <- c(
	"\\\"I\\'m leaving\\\", she said.",
	"\\\"No! Please! I\\'m so sorry\\\", he cried.",
	"\\\"I can\\'t forgive you -- what you did. Not now. Probably never.\\\"",
	"She went on. \\\"It\\'s like you have... there are two of you. And I never know which.\\\""
)
expect_equal(escaped, correct.escaped)