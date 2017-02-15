sending
sendTo: anObject

	| method |
	anObject == target ifFalse: [
		^ anObject perform: message selector withArguments: message arguments.
	].

	method := AvDelegationMethodProvider uniqueInstance delegationMethodFor: (target class lookupSelector: message selector).

	(self shouldExecuteOnTarget: method) ifTrue: [ 
		^ MirrorPrimitives
			withReceiver: target
			andArguments: message arguments
			execute: method
	].

	^ MirrorPrimitives
		withReceiver: proxy
		andArguments: message arguments
		execute: method