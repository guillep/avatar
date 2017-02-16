handling
handleMessage: aMessage toTarget: anObject

	crossHandler ifNil: [ ^ self ].
	(self isWet: anObject)
		ifTrue: [ crossHandler dryToWetMessage: aMessage toTarget: anObject ]
		ifFalse: [ crossHandler wetToDryMessage: aMessage toTarget: anObject ]