as yet unclassified
shouldLetPass: anObject

	^ anObject isNumber
		or: [ anObject == true
			or: [ anObject == false ] ]