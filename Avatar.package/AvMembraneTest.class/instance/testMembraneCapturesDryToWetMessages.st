tests
testMembraneCapturesDryToWetMessages

	| membrane dryObject object membraneHandler |
	membrane := AvMembrane new.
	membraneHandler := AvTestLogCrossMembraneHandler new.
	membrane crossHandler: membraneHandler.
	object := Rectangle origin: 0@0 corner: 10@10.
	dryObject := membrane inject: object.
	
	dryObject origin x.
	
	self assert: (membraneHandler dryToWetMessages includes: #origin).
	self assert: (membraneHandler dryToWetMessages includes: #x)