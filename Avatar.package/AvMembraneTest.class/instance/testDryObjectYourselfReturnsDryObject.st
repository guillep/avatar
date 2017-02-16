tests
testDryObjectYourselfReturnsDryObject

	| membrane dryObject object |
	membrane := AvMembrane new.
	object := Rectangle origin: 0@0 corner: 10@10.
	dryObject := membrane inject: object.
	
	self assert: dryObject == dryObject yourself