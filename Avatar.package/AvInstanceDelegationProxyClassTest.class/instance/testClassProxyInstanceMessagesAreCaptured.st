tests
testClassProxyInstanceMessagesAreCaptured

	"This is just a smoke test. We do not assert anything in this test cause it could cause a VM crash in case it goes wrong."
	handler := AvTestLogAndResendHandler new.
	self newProxy basicNew yourself.
	
	self assert: handler messages size equals: 2.
	self assert: handler messages first selector equals: #basicNew.
	self assert: handler messages second selector equals: #yourself.