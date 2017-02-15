tests
testCatchIntanceVariableWrite

	target := Object newAnonymousSubclass.
	handler := AvTestLogAndResendHandler new.
	self newProxy superclass: nil.
	
	self assert: handler messages size equals: 2.
	self assert: handler messages first selector equals: #superclass:.
	
	self assert: handler messages second selector equals: #instVarAt:put:.
	self assert: handler messages second arguments asArray equals: #( 1 nil ).