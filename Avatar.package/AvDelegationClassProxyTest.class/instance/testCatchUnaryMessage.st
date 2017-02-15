tests
testCatchUnaryMessage

	self newProxy yourself.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #yourself.