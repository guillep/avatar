sending
delegationMethodFor: method
	"It calculates the delegation method, if it has to be rewritten"
	^ AvDelegationMethodProvider uniqueInstance delegationMethodFor: method