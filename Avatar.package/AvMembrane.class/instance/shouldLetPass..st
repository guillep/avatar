handling
shouldLetPass: anObject

	^ anObject isNumber
		or: [ anObject == true
			or: [ anObject == false
				or: [ anObject isCharacter ] ] ]