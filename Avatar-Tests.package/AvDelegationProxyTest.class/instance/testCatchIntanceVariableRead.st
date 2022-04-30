tests
testCatchIntanceVariableRead

	target := Point new.
	handler := AvTestLogAndResendHandler new.
	self newProxy x.
	
	self assert: handler messages size equals: 2.
	self assert: handler messages first selector equals: #x.
	
	self assert: handler messages second selector equals: #instVarAt:.
	self assert: handler messages second arguments asArray equals: #( 1 ).