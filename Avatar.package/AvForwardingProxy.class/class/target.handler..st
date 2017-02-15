instance-creation
target: anObject handler: aHandler
	
	| proxy |
	proxy := self basicNew.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #target) index put: anObject.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #handler) index put: aHandler.
	^ proxy