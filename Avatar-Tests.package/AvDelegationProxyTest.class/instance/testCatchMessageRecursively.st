tests
testCatchMessageRecursively

	handler := AvTestLogAndResendHandler new.
	self newProxy displayString.
	
	self assert: handler messages size equals: 7.
	self assert: handler messages first selector equals: #displayString.
	self assert: handler messages second selector equals: #displayStringLimitedTo:.
	self assert: handler messages third selector equals: #printStringLimitedTo:using:.
	self assert: handler messages fourth selector equals: #displayStringOn:.
	self assert: handler messages fifth selector equals: #printOn:.
	self assert: handler messages sixth selector equals: #class.