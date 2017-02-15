instance-creation
target: anObject handler: aHandler
	
	| proxy |
	proxy := self basicNew.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #superclass) index put: nil.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #format) index put: anObject format.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #methodDictionary) index put: MethodDictionary new.
	
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #target) index put: anObject.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #handler) index put: aHandler.
	^ proxy