accessing
delegationMethodFor: aMethod
	^ self
		delegationMethodFor: aMethod
		primitivesToRewrite: [ :aPrimitiveMethod | aPrimitiveMethod isRealPrimitive ]