tests
testMembraneLetsFloatPass

	| membrane dryObject object literal |
	membrane := AvMembrane new.
	literal := 1.27.
	object := Array with: literal.
	dryObject := membrane inject: object.
	
	self assert: dryObject first == literal