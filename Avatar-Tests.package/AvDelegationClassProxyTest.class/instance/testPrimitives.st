tests
testPrimitives
	| result |
	handler := AvTestLogAndResendHandler new.
	result := self newProxy identityHash.	
	
	self assert: handler messages size equals: 3.
	self assert: handler messages first selector equals: #identityHash.
	self assert: handler messages second selector equals: #basicIdentityHash.
	self assert: handler messages third selector equals: #tryPrimitive:withArgs:.
	
	self assert: result equals: target identityHash.	