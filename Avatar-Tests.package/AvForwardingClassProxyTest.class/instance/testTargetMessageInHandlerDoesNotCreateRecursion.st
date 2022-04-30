tests
testTargetMessageInHandlerDoesNotCreateRecursion

	| result |
	handler := AvTestMetaHandler new.
	
	result := self newProxy yourself.
	
	self assert: result equals: target printString.