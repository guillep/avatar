tests
testCatchIntanceVariableRead

	| target handler proxy |
	target := Point new.
	handler := AvTestLogAndResendHandler new.
	proxy := AvForwardingProxy target: target handler: handler.
	
	proxy x.
	
	self assert: handler messages size equals: 2.
	self assert: handler messages first selector equals: #x.
	
	self assert: handler messages second selector equals: #instVarAt:.
	self assert: handler messages second arguments asArray equals: #( 1 ).