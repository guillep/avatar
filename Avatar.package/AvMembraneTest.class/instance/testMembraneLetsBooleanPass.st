tests
testMembraneLetsBooleanPass

	| membrane dryObject object literal |
	membrane := AvMembrane new.
	literal := true.
	object := Array with: literal.
	dryObject := membrane inject: object.
	
	self assert: dryObject first == literal