accessing
delegationMethodFor: aMethod 
	
	| delegationMethod |
	aMethod hasInstVarRef ifFalse: [ ^ aMethod ].
	
	delegationMethod := aMethod ast copy.
	delegationMethod nodesDo: [ :node | 
		(node isVariable and: [ node isInstance ])
			ifTrue: [
				(node parent isAssignment and: [ node parent variable = node ])
					ifTrue: [ node parent replaceWith: (RBMessageNode
																	receiver: (RBSelfNode named: 'self')
																	selector: #instVarAt:put:
																	arguments: { 
																		RBLiteralValueNode value: node binding slot index.
																		node parent value }) ]
					ifFalse: [ node replaceWith: (RBMessageNode
															receiver: (RBSelfNode named: 'self')
															selector: #instVarAt:
															arguments: { RBLiteralValueNode value: node binding slot index }) ] ] ].
	^ OpalCompiler new
		class: aMethod methodClass;
		compile: delegationMethod formattedCode