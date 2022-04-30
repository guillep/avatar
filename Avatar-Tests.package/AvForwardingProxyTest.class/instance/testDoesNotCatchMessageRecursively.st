tests
testDoesNotCatchMessageRecursively

	self newProxy displayString.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #displayString.