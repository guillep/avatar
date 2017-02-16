as yet unclassified
dryToWet: anObject

	| newProxy |
	(self shouldLetPass: anObject) ifTrue: [ ^ anObject ].
	
	dryToWet at: anObject ifPresent: [ :proxy | ^ proxy ].
	
	newProxy := AvForwardingProxy
		target: anObject
		handler: (AvMembraneHandler membrane: self).

	dryToWet at: anObject put: newProxy.
	wetToDry at: newProxy put: anObject.
	
	^ newProxy