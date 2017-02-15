sending
shouldExecuteOnTarget: aMethod

	^ { Object>>#instVarAt: . Object>>#instVarAt:put: } includes: aMethod