tests
testClassProxyInstanceAnswersMessages

	"This is just a smoke test. We do not assert anything in this test cause it could cause a VM crash in case it goes wrong."
	handler := AvTestLogAndResendHandler new.
	self newProxy basicNew yourself.