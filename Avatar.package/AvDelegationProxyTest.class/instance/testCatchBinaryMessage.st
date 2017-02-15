tests
testCatchBinaryMessage

	| target handler proxy |
	target := Object new.
	handler := AvTestLogHandler new.
	proxy := AvDelegationProxy target: target handler: handler.
	
	proxy + 5.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #+.
	self assert: handler messages first arguments asArray equals: #( 5 ).