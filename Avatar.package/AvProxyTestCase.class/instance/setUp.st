instance creation
setUp

	super setUp.
	handler := AvTestLogHandler new.
	self setUpTarget.