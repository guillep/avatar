handling
handleInstanceMessage: aMessage toTarget: anObject

	messages add: aMessage.
	^ aMessage sendTo: anObject.