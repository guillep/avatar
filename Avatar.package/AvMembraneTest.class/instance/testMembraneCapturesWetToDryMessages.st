tests
testMembraneCapturesWetToDryMessages

	| membrane dryObject object membraneHandler |
	membrane := AvMembrane new.
	membraneHandler := AvTestLogCrossMembraneHandler new.
	membrane crossHandler: membraneHandler.
	object := Rectangle origin: 0@0 corner: 10@10.
	dryObject := membrane inject: object.
	
	dryObject privateSetOrigin: 0@1.
	dryObject privateSetCorner: 0@1.
	dryObject extent.
	
	self assert: (membraneHandler wetToDryMessages includes: #-)