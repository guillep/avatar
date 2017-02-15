instance creation
uniqueInstance

	^ Instance ifNil: [ Instance := self new ]