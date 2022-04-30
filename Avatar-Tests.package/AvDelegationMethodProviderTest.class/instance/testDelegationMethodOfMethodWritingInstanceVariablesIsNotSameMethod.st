tests
testDelegationMethodOfMethodWritingInstanceVariablesIsNotSameMethod

	| provider setter delegatedSetter |
	provider := AvDelegationMethodProvider new.

	setter := Point>>#setX:setY:.
	delegatedSetter := provider delegationMethodFor: setter.

	self deny: delegatedSetter == setter