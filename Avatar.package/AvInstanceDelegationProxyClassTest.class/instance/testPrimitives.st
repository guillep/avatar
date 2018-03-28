tests
testPrimitives
	| result proxy |
	handler := AvTestLogAndResendHandler new.
	proxy := self newProxy.
	result := proxy identityHash.	
	
	self assert: handler messages size equals: 3.
	self assert: handler messages first selector equals: #identityHash.
	self assert: handler messages second selector equals: #basicIdentityHash.
	self assert: handler messages third selector equals: #tryPrimitive:withArgs:.
	
	self assert: result equals: target identityHash.	
	self assert: (MirrorPrimitives classOf: proxy) equals: AvInstanceDelegationClassProxy