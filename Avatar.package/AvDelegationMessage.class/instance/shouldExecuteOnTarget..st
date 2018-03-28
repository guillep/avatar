sending
shouldExecuteOnTarget: aMethod

	^ { Object>>#instVarAt: . Object>>#instVarAt:put: . ProtoObject>>#tryPrimitive:withArgs: } includes: aMethod