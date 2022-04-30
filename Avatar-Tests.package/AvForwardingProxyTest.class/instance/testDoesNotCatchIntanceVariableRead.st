tests
testDoesNotCatchIntanceVariableRead

	target := Point new.
	handler := AvTestLogAndResendHandler new.
	
	self newProxy x.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #x.
	
	"The variable access is not captured because it is a forwarding proxy"