instance-creation
target: anObject handler: aHandler
	
	| proxy methodDictionary |
	proxy := self basicNew.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #superclass) index put: nil.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #format) index put: anObject format.
	
	methodDictionary := MethodDictionary new.
	methodDictionary at: #doesNotUnderstand: put: (self class >> #instanceDoesNotUnderstand:).
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #methodDictionary) index put: methodDictionary.
	
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #target) index put: anObject.
	MirrorPrimitives fixedFieldOf: proxy at: (self slotNamed: #handler) index put: aHandler.
	^ proxy