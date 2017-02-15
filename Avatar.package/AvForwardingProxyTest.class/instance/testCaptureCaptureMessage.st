tests
testCaptureCaptureMessage

	| target handler proxy result |
	target := Object new.
	handler := AvTestLogHandler new.
	proxy := AvForwardingProxy target: target handler: handler.
	
	result := proxy target: nil handler:  nil.
	
	self assert: handler messages first selector equals: #target:handler:.