tests
testCatchKeywordMessage

	self newProxy test: 5.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #test:.
	self assert: handler messages first arguments asArray equals: #( 5 ).