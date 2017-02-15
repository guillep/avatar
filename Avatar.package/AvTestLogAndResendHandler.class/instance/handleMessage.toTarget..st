handling
handleMessage: aMessage toTarget: anObject 
	
	super handleMessage: aMessage toTarget: anObject.
	^ aMessage sendTo: anObject.