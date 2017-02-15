A forwarding proxy is a non-reentrant proxy. This means that messages dispatched from the forwarding proxy do not pass any more through the forwarding proxy. Let's consider for example the following method in class `Rectangle`:

Rectangle>>area
  ^  self height * self width

If we create a forwarding proxy of a rectangle and we call area on it, the forwarding proxy will catch **only** the area message. Height and width messages are dispatched directly to the rectangle and the proxy is not used anymore from that point on.

proxy := AvForwardingProxy target: Rectangle new handler: MyLoggingHandler new.
proxy area.

This will output in the `Transcript`:

area

Implementation Details:
=====================

This forwarding proxy implementation uses doesNotUnderstand: for reasons of simplicity and performance. This implementation has a main drawback: the message #doesNotUnderstand: could not be captured by the library.

The alternative could have used  cannotInterpret. This would have required a bit more complexity in the implementation, but the main point is that cannotInterpret: is not optimized in the VM JIT, thus it provokes a considerable loss of performance.