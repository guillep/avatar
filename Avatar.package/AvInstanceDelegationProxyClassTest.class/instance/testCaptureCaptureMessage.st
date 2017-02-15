tests
testCaptureCaptureMessage

	self newProxy target: nil handler:  nil.
	
	self assert: handler messages first selector equals: #target:handler:.