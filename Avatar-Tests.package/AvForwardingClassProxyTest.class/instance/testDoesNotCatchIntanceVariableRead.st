tests
testDoesNotCatchIntanceVariableRead

	handler := AvTestLogAndResendHandler new.
	
	self newProxy superclass.
	
	self assert: handler messages size equals: 1.
	self assert: handler messages first selector equals: #superclass.
	
	"The variable access is not captured because it is a forwarding proxy"