tests
testCatchKeywordMessage

	| target handler proxy |
	target := Object new.
	handler := AvTestLogHandler new.
	proxy := AvDelegationProxy target: target handler: handler.
	
	proxy test: 5.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #test:.
	self assert: handler messages first arguments asArray equals: #( 5 ).