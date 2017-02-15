tests
testDelegationMethodOfMethodNotAccessingInstanceVariablesIsSameMethod

	| provider yourself delegatedYourself |
	provider := AvDelegationMethodProvider new.

	yourself := Object>>#yourself.
	delegatedYourself := provider delegationMethodFor: yourself.

	self assert: delegatedYourself == yourself