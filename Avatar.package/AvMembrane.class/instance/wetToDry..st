handling
wetToDry: anObject

	| newProxy |
	(self shouldLetPass: anObject) ifTrue: [ ^ anObject ].
	
	wetToDry at: anObject ifPresent: [ :proxy | ^ proxy ].
	
	newProxy := AvForwardingProxy
		target: anObject
		handler: (AvMembraneHandler membrane: self).
	
	wetToDry at: anObject put: newProxy.
	dryToWet at: newProxy put: anObject.
	
	^ newProxy