instance-creation
message: aMessage proxy: aProxy target: aTarget handler: aHandler

	^ self new
		message: aMessage;
		proxy: aProxy;
		target: aTarget;
		handler: aHandler;
		yourself