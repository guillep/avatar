instance-creation
message: aMessage proxy: aProxy target: aTarget handler: aHandler instance: instance

	^ self new
		message: aMessage;
		proxy: aProxy;
		target: aTarget;
		handler: aHandler;
		instance: instance;
		yourself