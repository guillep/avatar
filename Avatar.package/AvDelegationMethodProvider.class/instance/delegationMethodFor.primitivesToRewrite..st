accessing
delegationMethodFor: aMethod primitivesToRewrite: aBlock

	"This method creates a new method rewritten when a primitive is call or when a inst variable is assigned. It receives a block that is evaluated in the method, to check if the primitive should be rewriten or not"

	| delegationMethod |
	(aBlock value: aMethod) ifTrue: [ ^ self rewritePrimitive: aMethod ].
	aMethod hasInstVarRef ifFalse: [ ^ aMethod ].

	delegationMethod := aMethod ast copy.
	delegationMethod nodesDo: [ :node | 
		(node isVariable and: [ node isInstanceVariable ]) ifTrue: [ 
			node parent isAssignment
				ifTrue: [ 
					node parent replaceWith: (RBMessageNode
							 receiver: (RBSelfNode named: 'self')
							 selector: #instVarAt:put:
							 arguments: { 
									 (RBLiteralValueNode value: node binding index).
									 node parent value }) ]
				ifFalse: [ 
					node replaceWith: (RBMessageNode
							 receiver: (RBSelfNode named: 'self')
							 selector: #instVarAt:
							 arguments:
							 { (RBLiteralValueNode value: node binding index) }) ] ] ].
	^ OpalCompiler new
		  class: aMethod methodClass;
		  compile: delegationMethod formattedCode