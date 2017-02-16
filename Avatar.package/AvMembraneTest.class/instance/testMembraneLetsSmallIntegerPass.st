tests
testMembraneLetsSmallIntegerPass

	| membrane dryObject object |
	membrane := AvMembrane new.
	object := 0@0.
	dryObject := membrane inject: object.
	
	self assert: dryObject x == 0