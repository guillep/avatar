accessing
rewritePrimitive: aMethod
	| delegationMethod primitive |
	primitive := aMethod primitive.

	delegationMethod := aMethod ast copy.

	delegationMethod pragmas: #().
	delegationMethod body replaceWith: (RBReturnNode value: (RBMessageNode receiver: (RBSelfNode named: 'self') selector: #tryPrimitive:withArgs: arguments: ({RBLiteralNode value: primitive . RBArrayNode statements: delegationMethod arguments})  )).
	
	^ OpalCompiler new
		class: aMethod methodClass;
		compile: delegationMethod formattedCode