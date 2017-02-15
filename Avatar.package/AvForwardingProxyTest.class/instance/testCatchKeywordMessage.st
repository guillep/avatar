tests
testCatchKeywordMessage

	| target handler proxy |
	target := Object new.
	handler := AvTestLogHandler new.
	proxy := AvForwardingProxy target: target handler: handler.
	
	proxy assert: true.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #assert:.
	self assert: handler messages first arguments asArray equals: #( true ).