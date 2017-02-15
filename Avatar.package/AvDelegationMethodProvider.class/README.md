An AvDelegationMethodProvider is a singleton object used to  adapt a method for delegation.  That is to replace every instance variable access by  ̀instVarAt: ̀ and  ̀instVarAt:put: ̀ messages.

This is done by taking the AST of the original method and in it replace 
 - instance variable reads by   ̀ instVarAt: ̀ message send nodes
 - instance variable writes by  ̀instVarAt: put:̀ message send nodes

Then the new ast is recompiled in the context of the same method's class.