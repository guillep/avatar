A delegation proxy is a reentrant proxy. This means that messages dispatched from the delegation proxy will pass through the delegation proxy too. Let's consider for example the following method in class `Rectangle`:

Rectangle>>area
  ^  self height * self width

If we create a delegation proxy of a rectangle and we call area on it, the delegation proxy will catch the area message and all other messages sent to self such as `height` and `width`.

proxy := AvDelegationProxy target: Rectangle new handler: MyLoggingHandler new.
proxy area.

This will output in the `Transcript`:

area
height
instVarAt: 1
width
instVarAt: 2

Notice that the delegation proxy captured messages named `instVarAt:`. Delegation proxies capture all interactions with the object, even read and write accesses to instance variables in the object. In case we would like to ignore these messages, we could do it by changing the behaviour of our handler. We discuss further about it in the handlers section.

Implementation Details:
=====================

This forwarding proxy implementation uses doesNotUnderstand: for reasons of simplicity and performance. This implementation has a main drawback: the message #doesNotUnderstand: could not be captured by the library.

The alternative could have used  cannotInterpret. This would have required a bit more complexity in the implementation, but the main point is that cannotInterpret: is not optimized in the VM JIT, thus it provokes a considerable loss of performance.