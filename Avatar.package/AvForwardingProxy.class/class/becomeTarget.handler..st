instance-creation
becomeTarget: anObject handler: aHandler
	| proxy |
	proxy := self target: anObject handler: aHandler.
	anObject become: proxy.
	MirrorPrimitives fixedFieldOf: anObject at: (self slotNamed: #target) index put: proxy.
	^ anObject