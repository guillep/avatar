A forwarding proxy is a non-reentrant proxy. This means that messages dispatched from the forwarding proxy do not pass any more through the forwarding proxy. Let's consider for example the following method:

Rectangle>>area
  ^  self height * self width

If we create a forwarding proxy of a rectangle and we call area on it, the forwarding proxy will catch **only** the area message. Height and width messages are dispatched directly to the rectangle and the proxy is not used anymore from that point on.

Usage:
======
To create a forwarding proxy, use the following expression.

proxy := AvForwardingProxy target: objectToProxy handler: handlerObject.

The target is the object that will be represented by the proxy.
The handler is an object that describes what to do when a message is captured. See more on handlers in the AvHandler class.

Once a proxy is created, you can use it normally as any other object:

proxy yourself.
proxy + 2.
proxy someKeywordMessage: #true.

Implementation Details:
=====================

This forwarding proxy implementation uses doesNotUnderstand: for reasons of simplicity and performance. This implementation has a main drawback: the message #doesNotUnderstand: could not be captured by the library.

The alternative could have used  cannotInterpret. This would have required a bit more complexity in the implementation, but the main point is that cannotInterpret: is not optimized in the VM JIT, thus it provokes a considerable loss of performance.