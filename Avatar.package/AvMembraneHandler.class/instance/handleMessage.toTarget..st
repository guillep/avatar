handling
handleMessage: aMessage toTarget: anObject

	| isWet traversingMembraneMessage result |
	isWet := membrane isWet: anObject.
	
	membrane handleMessage: aMessage toTarget: anObject.
	
	traversingMembraneMessage := Message
		selector: aMessage selector
		arguments: (aMessage arguments collect: [ :obj |
			isWet
				ifTrue: [ membrane dryToWet: obj ]
				ifFalse: [ membrane wetToDry: obj ] ]).
	result := traversingMembraneMessage sendTo: anObject.
	^ isWet
		ifTrue: [ membrane wetToDry: result ]
		ifFalse: [ membrane dryToWet: result ]