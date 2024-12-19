accessing
delegationMethodFor: aMethod primitivesToRewrite: aBlock

	"This method creates a new method rewritten when a primitive is call or when a inst variable is assigned. It receives a block that is evaluated in the method, to check if the primitive should be rewriten or not"

	| delegationMethod |
	(aBlock value: aMethod) ifTrue: [ 
		^ cache at: aMethod ifAbsentPut: [ self rewritePrimitive: aMethod ] ].

	"If the method does not acccess instance variables, no rewrite is required"
	((1 to: aMethod methodClass instSize)
		anySatisfy: [ :idx | aMethod accessesField: idx ])
			ifFalse: [ ^ aMethod ].


	^ cache at: aMethod ifAbsentPut: [ 
		  delegationMethod := aMethod ast copy.
		  delegationMethod nodesDo: [ :node | 
			  (node isVariable and: [ node isInstanceVariable ]) ifTrue: [ 
				  node parent isAssignment
					  ifTrue: [ 
						  node parent replaceWith: (RBMessageNode
								   receiver: RBVariableNode selfNode
								   selector: #instVarAt:put:
								   arguments: { 
										   (RBLiteralValueNode value: node binding index).
										   node parent value }) ]
					  ifFalse: [ 
						  node replaceWith: (RBMessageNode
								   receiver: RBVariableNode selfNode
								   selector: #instVarAt:
								   arguments:
								   { (RBLiteralValueNode value: node binding index) }) ] ] ].
		  OpalCompiler new
			  class: aMethod methodClass;
			  compile: delegationMethod formattedCode ]