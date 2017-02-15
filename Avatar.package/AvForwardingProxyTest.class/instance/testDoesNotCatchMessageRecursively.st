tests
testDoesNotCatchMessageRecursively

	| target handler proxy |
	target := Object new.
	handler := AvTestLogHandler new.
	proxy := AvForwardingProxy target: target handler: handler.
	
	proxy displayString.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #displayString.