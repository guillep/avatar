tests
testReplaceTargetReferences
	| result |
	handler := AvTestLogAndResendHandler new.
	target := Point x: 1 y: 2.
	AvDelegationProxy becomeTarget: target handler: handler.
	result := target x.
	self assert: result equals: 1.
	self assert: handler messages notEmpty.
	self assert: handler messages first message selector equals: #x