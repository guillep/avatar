# Avatar

Avatar is a proxy library for the Pharo language. The main focus of this library is to be of simple usage and providing clear examples for final users.

## Proxies and Handlers

A proxy is an object that takes the place of a `target` object. This allows a proxy to capture all interactions with the target and do some particular intercession. For example, a proxy could log every message received, or stop in a breakpoint when a particular combination of messages happens. The intercession done by a proxy is specified by a `handler`.

In summary, a proxy captures messages sent to a target and domes some special handling in a handler.

Avatar provides pre-made forwarding and delegation proxy implementations that work for classes and objects. In addition, it provides a nice way to describe handlers that are independent of the proxy implementation.

### First Example

Let's imagine we want to create a proxy logging all the interactions with a particular object. This means that we require to create a proxy with a logging handler. An avatar handle is any object that understands the message: `handleMessage: aMessage toTarget: anObject`. To implement our logging handler we need to define a new handler class:

```smalltalk
Object subclass: #MyLoggingHandler
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Avatar-Tests'
```

And put in it a `handleMessage: aMessage toTarget: anObject` method:

```smalltalk
MyLoggingHandler >> handleMessage: aMessage toTarget: anObject
    aMessage logCr.
    aMessage sendTo: anObject.
```

In such a manner, our handler will log the message into the `Transcript`, and then resend the message to the object.

We can create a proxy using this handler as follows:

```smalltalk
proxy := AvForwardingProxy target: MyObject new handler: MyLoggingHandler new.
proxy yourself.
proxy = true.
```

And see that the output of our Transcript will say:

```
yourself
= true
```

We used in this example a forwarding proxy. More about them follows in next sections.

### 1 . Forwarding Proxies

A forwarding proxy is a non-reentrant proxy. This means that messages dispatched from the forwarding proxy do not pass any more through the forwarding proxy. Let's consider for example the following method in class `Rectangle`:

```smalltalk
Rectangle>>area
  ^  self height * self width
```

If we create a forwarding proxy of a rectangle and we call area on it, the forwarding proxy will catch **only** the area message. Height and width messages are dispatched directly to the rectangle and the proxy is not used anymore from that point on.

```smalltalk
proxy := AvForwardingProxy target: Rectangle new handler: MyLoggingHandler new.
proxy area.
```

This will output in the `Transcript`:

```
area
```

### 2 . Delegation Proxies

A delegation proxy is a reentrant proxy. This means that messages dispatched from the delegation proxy will pass through the delegation proxy too. Let's consider for example the following method in class `Rectangle`:

```smalltalk
Rectangle>>area
  ^  self height * self width
```

If we create a delegation proxy of a rectangle and we call area on it, the delegation proxy will catch the area message and all other messages sent to self such as `height` and `width`.

```smalltalk
proxy := AvDelegationProxy target: Rectangle new handler: MyLoggingHandler new.
proxy area.
```

This will output in the `Transcript`:

```
area
height
instVarAt: 1
width
instVarAt: 2
```

Notice that the delegation proxy captured messages named `instVarAt:`. Delegation proxies capture all interactions with the object, even read and write accesses to instance variables in the object. In case we would like to ignore these messages, we could do it by changing the behaviour of our handler. We discuss further about it in the handlers section.

### Handlers

Handlers are meta-objects that define what to do when a proxy receives a message. As we saw before a handler can be any object, as soon as it implements de defined `handleMessage:toTarget:` interface. Using different patterns, a handler can then capture messages in different ways:

- *instead* of sending the message to the target

By default, a handler will execute its defined action *instead* of sending the message. For example, a handler could log the received messages but never resend the message to the original object:

```smalltalk
MyLoggingHandler >> handleMessage: aMessage toTarget: anObject
    aMessage logCr.
```

- *before* or *after* the message is sent to the target

To not replace the send to the original object (because for example we would like to keep its behaviour), we need to explicitly resend the message to the target using the `sendTo:` message of the received message. Using this, we could do something *before* the message continues, something *after* it continues.

```smalltalk
MyLoggingHandler >> handleMessage: aMessage toTarget: anObject
    | result |
    ('message: ', aMessage asString) logCr.
    result := aMessage sendTo: anObject.
    ('result: ', result asString) logCr.
    ^ result
```


## Implementation Details

Avatar uses `doesNotUnderstand:` for reasons of simplicity and performance. This implementation has a main drawback: `doesNotUnderstand:` cannot be captured by the library.

Alternatively, we could have implemented it with `cannotInterpret:`. This would have required a bit more complexity in the implementation, but the main point is that cannotInterpret: is not optimized in the VM JIT, thus it provokes a considerable loss of performance.

## Inspirations and Acknowledgements

Avatar is based in previous research from M.M.Peck, C. Teruel, their first implementation of Ghost and the continuation of this work by D. Kudriashov.
