sending
sendTo: anObject

	| method |
	anObject == instance ifFalse: [
		^ anObject perform: message selector withArguments: message arguments.
	].

	method := AvDelegationMethodProvider uniqueInstance delegationMethodFor: (target lookupSelector: message selector).

	^ MirrorPrimitives
			withReceiver: instance
			andArguments: message arguments
			execute: method.