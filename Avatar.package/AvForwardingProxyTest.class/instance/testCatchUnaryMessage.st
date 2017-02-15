tests
testCatchUnaryMessage

	| target handler proxy |
	target := Object new.
	handler := AvTestLogHandler new.
	proxy := AvForwardingProxy target: target handler: handler.
	
	proxy yourself.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #yourself.