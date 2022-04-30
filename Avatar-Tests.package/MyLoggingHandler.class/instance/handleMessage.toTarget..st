as yet unclassified
handleMessage: aMessage toTarget: anObject

	aMessage traceCr.
	^ aMessage sendTo: anObject