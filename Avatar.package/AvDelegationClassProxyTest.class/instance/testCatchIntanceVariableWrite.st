tests
testCatchIntanceVariableWrite

	target := Point new.
	handler := AvTestLogAndResendHandler new.
	self newProxy setX: 3 setY: 4.
	
	self assert: handler messages size equals: 3.
	self assert: handler messages first selector equals: #setX:setY:.
	
	self assert: handler messages second selector equals: #instVarAt:put:.
	self assert: handler messages second arguments asArray equals: #( 1 3 ).
	
	self assert: handler messages third selector equals: #instVarAt:put:.
	self assert: handler messages third arguments asArray equals: #( 2 4 ).