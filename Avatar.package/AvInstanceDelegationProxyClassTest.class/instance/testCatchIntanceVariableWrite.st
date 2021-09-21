tests
testCatchIntanceVariableWrite

	target := Object newAnonymousSubclass.
	handler := AvTestLogAndResendHandler new.
	self newProxy superclass: nil.
	
	self assert: handler messages size equals: 3.
	self assert: handler messages first selector equals: #superclass:.
	self assert: handler messages second selector equals: #basicSuperclass:.
	
	self assert: handler messages third selector equals: #instVarAt:put:.
	self assert: handler messages third arguments asArray equals: #( 1 nil ).