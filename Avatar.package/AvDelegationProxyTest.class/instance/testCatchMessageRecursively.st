tests
testCatchMessageRecursively

	| target handler proxy |
	target := Object new.
	handler := AvTestLogAndResendHandler new.
	proxy := AvDelegationProxy target: target handler: handler.
	
	proxy displayString.
	
	self assert: handler messages size equals: 5.
	self assert: handler messages first selector equals: #displayString.
	self assert: handler messages second selector equals: #printString.
	self assert: handler messages third selector equals: #printStringLimitedTo:.
	self assert: handler messages fourth selector equals: #printOn:.
	self assert: handler messages fifth selector equals: #class.