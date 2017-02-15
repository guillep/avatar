sending
sendTo: anObject

	"Checks if the object to send the message is the target of the proxy or not. If positive, the AvDelegationMethodProvider is asked to obtain a delegation method.
	This delegation method is executed on the proxy, and allows the proxy to handle all messages and even instance variable accesses as messages.
	
	Check AvDelegationMethodProvider for more information.

   Note also, that some methods such as primitives are executed on the target and not the proxy to get a correct behaviour."
	
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