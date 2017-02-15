# Avatar

Avatar is a proxy library for the Pharo language. The main focus of this library is to be of simple usage and providing clear examples for final users.

## Proxies and Handlers

A proxy is an object that takes the place of a `target` object. This allows a proxy to capture all interactions with the target and do some particular intercession. For example, a proxy could log every message received, or stop in a breakpoint when a particular combination of messages happens. The intercession done by a proxy is specified by a `handler`.

In summary, a proxy captures messages sent to a target and domes some special handling in a handler.

Avatar provides pre-made forwarding and delegation proxy implementations that work for classes and objects. In addition, it provides a nice way to describe handlers that are independent of the proxy implementation.

### First Example

Let's imagine we want to create a proxy logging all the interactions with a particular object. This means that we require to create a proxy with a logging handler. An avatar handle is any object that understands the message: `handleMessage: aMessage toTarget: anObject`. To implement our logging handler we need to define a new handler class:

```
Object subclass: #MyLoggingHandler
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Avatar-Tests'
```

And put in it a `handleMessage: aMessage toTarget: anObject` method:

```
MyLoggingHandler >> handleMessage: aMessage toTarget: anObject
    aMessage logCr.
    aMessage sendTo: anObject.
```

In such a manner, our handler will log the message into the `Transcript`, and then resend the message to the object.

We can create a proxy using this handler as follows:

```
proxy := AvForwardingProxy target: MyObject new handler: MyLoggingHandler new.
proxy yourself.
proxy isNil.
```

## Kind of Proxies

### Forwarding Proxies

A forwarding proxy is a non-reentrant proxy. This means that messages dispatched from the forwarding proxy do not pass any more through the forwarding proxy. Let's consider for example the following method:

```smalltalk
Rectangle>>area
  ^  self height * self width
```

If we create a forwarding proxy of a rectangle and we call area on it, the forwarding proxy will catch **only** the area message. Height and width messages are dispatched directly to the rectangle and the proxy is not used anymore from that point on.

#### Usage

To create a forwarding proxy, use the following expression.

```
proxy := AvForwardingProxy target: objectToProxy handler: handlerObject.
```

The target is the object that will be represented by the proxy.
The handler is an object that describes what to do when a message is captured. See more on handlers in the AvHandler class.

Once a proxy is created, you can use it normally as any other object:

proxy yourself.
proxy + 2.
proxy someKeywordMessage: #true.

## Implementation Details

This forwarding proxy implementation uses doesNotUnderstand: for reasons of simplicity and performance. This implementation has a main drawback: the message #doesNotUnderstand: could not be captured by the library.

The alternative could have used  cannotInterpret. This would have required a bit more complexity in the implementation, but the main point is that cannotInterpret: is not optimized in the VM JIT, thus it provokes a considerable loss of performance.
