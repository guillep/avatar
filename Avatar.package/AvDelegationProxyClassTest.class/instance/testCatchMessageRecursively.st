tests
testCatchMessageRecursively

	handler := AvTestLogAndResendHandler new.
	self newProxy displayString.
	
	self assert: handler messages first selector equals: #displayString.
	self assert: handler messages second selector equals: #printString.
	self assert: handler messages third selector equals: #printStringLimitedTo:.
	self assert: handler messages fourth selector equals: #printOn:.
	self assert: handler messages fifth selector equals: #name.
	self assert: handler messages sixth selector equals: #instVarAt:.