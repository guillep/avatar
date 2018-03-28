sending
delegationMethodFor: method
	"It calculates the delegation method, if it has to be rewritten.
	Also it filters the basicNew an basicNew: as they should not rewritten when using a ClassDelegation"

	^ AvDelegationMethodProvider uniqueInstance
		delegationMethodFor: method
		primitivesToRewrite: [ :aPrimitiveMethod | 
			aPrimitiveMethod isRealPrimitive
				and: [ (#(71 70) includes: aPrimitiveMethod primitive) not ] ]