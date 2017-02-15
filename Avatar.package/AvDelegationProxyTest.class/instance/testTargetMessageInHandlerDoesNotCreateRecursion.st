tests
testTargetMessageInHandlerDoesNotCreateRecursion

	| target handler proxy result |
	target := Object new.
	handler := AvTestMetaHandler new.
	proxy := AvDelegationProxy target: target handler: handler.
	
	result := proxy yourself.
	
	self assert: result equals: target printString.