tests
testCatchKeywordMessage

	self newProxy assert: true.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #assert:.
	self assert: handler messages first arguments asArray equals: #( true ).