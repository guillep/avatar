sending
sendTo: anObject

	| method |
	anObject == target ifFalse: [
		^ proxy perform: message selector withArguments: message arguments.
	].

	method := target class lookupSelector: message selector.
	^ MirrorPrimitives
		withReceiver: proxy
		andArguments: message arguments
		execute: method