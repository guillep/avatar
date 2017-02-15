tests
testDoesNotCatchIntanceVariableRead

	| target handler proxy |
	target := Point new.
	handler := AvTestLogAndResendHandler new.
	proxy := AvForwardingProxy target: target handler: handler.
	
	proxy x.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #x.
	
	"The variable access is not captured because it is a forwarding proxy"