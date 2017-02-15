A delegation Message is like a normal message managing how message sends are resent in a delegation proxy.

The key implementation point of this class is the  ̀sendTo: ̀ message, that checks if the object to send the message is the target of the proxy or not. If positive:
 - a method lookup is performed in the target's class to get the method that should be executed
 - the obtained method is recompiled replacing every instance variable access by instVarAt: and instVarAt:put: messages
 - finally the new method is executed on the proxy.

This allows the delegation proxy to handle all messages and even instance variable accesses as messages.

Note also, that some methods such as primitives are executed on the target and not the proxy to get a correct behaviour.

An AvDelegationMessage is instantiated as it appears in AvDelegationProxy>>doesNotUnderstand:

doesNotUnderstand: aMessage

	^ handler handleMessage: (AvDelegationMessage message: aMessage proxy: self target: target handler: handler) toTarget: target