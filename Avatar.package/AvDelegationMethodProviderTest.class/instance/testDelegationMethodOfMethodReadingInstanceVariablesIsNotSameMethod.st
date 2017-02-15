tests
testDelegationMethodOfMethodReadingInstanceVariablesIsNotSameMethod

	| provider getter delegatedGetter |
	provider := AvDelegationMethodProvider new.

	getter := Point>>#x.
	delegatedGetter := provider delegationMethodFor: getter.

	self deny: delegatedGetter == getter