tests
testDelegationMethodOfMethodWritingInstanceVariablesDoesNotAccessInstanceVariables

	| provider setter delegatedSetter |
	provider := AvDelegationMethodProvider new.

	setter := Point>>#setX:setY:.
	delegatedSetter := provider delegationMethodFor: setter.

	self deny: delegatedSetter hasInstVarRef