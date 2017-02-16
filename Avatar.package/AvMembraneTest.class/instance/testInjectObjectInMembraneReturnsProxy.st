tests
testInjectObjectInMembraneReturnsProxy

	| membrane dryObject |
	membrane := AvMembrane new.
	dryObject := membrane inject: (Rectangle origin: 0@0 corner: 10@10).
	
	self assert: (MirrorPrimitives classOf: dryObject) = AvForwardingProxy